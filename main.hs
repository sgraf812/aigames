{-# LANGUAGE Rank2Types #-}
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.Tree as TG
import qualified Data.Map as M
--------
--Data--
--------
newtype SRID = SRID Int deriving (Show, Eq, Ord)
newtype RID = RID Int deriving (Show, Eq, Ord)
newtype Reward = Reward Int deriving (Show, Eq)
newtype Millis = Millis Int deriving (Show, Eq)
newtype Player = Player String deriving (Show, Eq)

data Message = SuperRegions [(SRID, Reward)] 
             | Regions [(RID, SRID)] 
             | Neighbors [(RID, [RID])]
             | UpdateMap [(RID, Player, Int)] -- last val: #armies 
             | YourBot Player
             | OpponentBot Player
             | StartingArmies Int
             | PickStartingRegions Millis [RID]
             | OpponentMoves [Move] 
             | PlaceArmiesRequest Millis 
             | AttackTransferRequest Millis 
                deriving (Show, Eq)

data Move = StartingRegions [RID] -- currently exactly 6 RIDs allowed. Maybe this can be forced through the type system somehow without resorting to tuples
          | PlaceArmies Player [(RID, Int)] -- last val: #armies
          | AttackTransfer Player [(RID, RID, Int)] -- src Reg, tgt Reg, #armies
          | NoMoves
            deriving (Show, Eq)

data Faction = Friendly Player | Neutral | Enemy Player
                deriving (Show, Eq)

type SuperRegion = (SRID, Reward, [RID])
type Region = (RID, SRID, Faction, Int) 

data World = World { world :: TG.Gr RID Int 
                   , regions :: M.Map RID Region
                   , superRegions :: M.Map SRID SuperRegion
                   , me :: Player
                   , opponent :: Player
                   , tbd :: [Message] -- list of messages yet to be processed, lifo.
                   }

emptyWorld :: World
emptyWorld = World G.empty M.empty M.empty (Player "") (Player "") []

data Brain mem = Brain { emptyMemory :: mem
                       , logic :: mem -> World -> mem
                       , chosenMove :: mem -> Move
                       }

stupidBrain :: Brain Int 
stupidBrain = Brain 0 logic chosenMove
              where logic m w = G.spLength (head . G.nodes . world $ w) (last . G.nodes . world $ w) (world w)
                    chosenMove m = AttackTransfer (Player "opp") [((RID 23), (RID 21), m)]

initWorld :: World -> World
initWorld w = let srs = last $ [srs | SuperRegions srs <- tbd w]
                  rs = last $ [rs | Regions rs <- tbd w]
                  ns = last $ [ns | Neighbors ns <- tbd w]
                  srs' = [(sr, rw, [r | (r, sr') <- rs, sr'==sr]) | (sr, rw) <- srs]
                  rs' = [(r, sr, Neutral, 0) | (r, sr) <- rs] 
                  nodes = zip [1..] (map fst rs)
                  ridToNode r = head [n | (n, r') <- nodes, r'==r] -- scales really bad
                  edges = [(ridToNode l, ridToNode r, 1) | (l, rs) <- ns, r <- rs]
                  w' = w { world = G.mkGraph nodes edges
                         , regions = M.fromList . zip (map fst rs) $ rs'
                         , superRegions = M.fromList . zip (map fst srs) $ srs'
                         , tbd = []
                         }
              in w' 
updateWorld :: World -> Message -> World
updateWorld w msg = case msg of
                        YourBot p -> w { me = p }
                        OpponentBot p -> w { opponent = p }
                        PickStartingRegions _ _ -> let w' = initWorld w in w' { tbd = msg:tbd w' }
                        _ -> w { tbd = msg:tbd w }


step :: (mem -> World -> mem) -> (mem, World, Bool) -> Message -> (mem, World, Bool) 
step logic (m,w,_) msg = let w' = updateWorld w msg
                             o = requestsOutput msg
                             m' = logic m w' 
                         in (m',w',o) where requestsOutput msg = case msg of
                                                            PickStartingRegions _ _ -> True
                                                            PlaceArmiesRequest _ -> True
                                                            AttackTransferRequest _ -> True
                                                            _ -> False
                                                        
----------
--Output--
----------
formatMove :: Move -> String
formatMove m = case m of
                    StartingRegions rids -> unwords . map (\(RID i) -> show i) $ rids 
                    PlaceArmies (Player p) armies -> intercalate ", " . map (\(RID r, na) -> unwords [p, "place_armies", show r, show na]) $ armies
                    AttackTransfer (Player p) mvmts -> intercalate ", " . map (\(RID src, RID tgt, na) -> unwords [p, "attack/transfer", show src, show tgt, show na]) $ mvmts
                    NoMoves -> "No moves"
               
---------
--Input--
---------
pairwise :: [a] -> [(a, a)]
pairwise (f:s:xs) = (f, s) : pairwise xs
pairwise _ = []

triplewise :: [a] -> [(a, a, a)]
triplewise (f:s:t:xs) = (f, s, t) : triplewise xs
triplewise _ = []


parseInput :: String -> Message
parseInput s = case words s of
                ("setup_map":ws) -> setupMap ws
                ("update_map":ws) -> updateMap ws
                ("settings":ws) -> settings ws  
                ("pick_starting_regions":ws) -> pickStartingRegions ws 
                ("opponent_moves":ws) -> opponentMoves ws
                ("go":ws) -> go ws

setupMap :: [String] -> Message
setupMap ws = case ws of
                ("super_regions":ws') -> SuperRegions . map (\(x, y) -> (SRID x, Reward y)) . pairwise . map read $ ws' 
                ("regions":ws') -> Regions . map (\(r, sr) -> (RID r, SRID sr)) . pairwise . map read $ ws' 
                ("neighbors":ws') -> Neighbors . map (\(r, ns) -> (RID . read $ r, map RID . map read . splitOn "," $ ns)) . pairwise $ ws'

updateMap :: [String] -> Message
updateMap ws = UpdateMap . map (\(r, p, na) -> (RID $ read r, Player p, read na)) . triplewise $ ws

settings :: [String] -> Message
settings ws = case ws of
                ("your_bot":ws') -> YourBot . Player . head $ ws'
                ("opponent_bot":ws') -> OpponentBot . Player . head $ ws'
                ("starting_armies":ws') -> StartingArmies . read . head $ ws'

pickStartingRegions :: [String] -> Message
pickStartingRegions ws = PickStartingRegions (Millis . read . head $ ws) . map (RID . read) . tail $ ws   

opponentMoves :: [String] -> Message
opponentMoves ws' = OpponentMoves $ parseMoves ws'
                    where parseMoves [] = [] 
                          parseMoves ws = case ws !! 1 of
                            "place_armies" -> PlaceArmies (Player . head $ ws) [(RID . read $ ws !! 2, read $ ws !! 3)] : parseMoves (drop 4 ws)
                            "attack/transfer" -> AttackTransfer (Player . head $ ws) [(RID . read $ ws !! 2, RID . read $ ws !! 3, read $ ws !! 4)] : parseMoves (drop 5 ws)                 

go :: [String] -> Message
go ws = case ws of
            ("place_armies":ws') -> PlaceArmiesRequest . Millis . read . head $ ws'
            ("attack/transfer":ws') -> AttackTransferRequest . Millis . read . head $ ws' 

handleInput :: Brain mem -> [String] -> [String]
handleInput b = formatOutput . scanl (step $ logic b) (emptyMemory b, emptyWorld, False) . map parseInput
                    where
                        formatOutput = mapMaybe (\(m,_,o) -> if o then Just . formatMove . chosenMove b $ m else Nothing)
                   

main :: IO ()
main = interact $ unlines . handleInput stupidBrain . lines

