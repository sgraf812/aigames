import Data.List.Split (splitOn)
--------
--Data--
--------
newtype SRID = SRID Int deriving (Show, Eq)
newtype RID = RID Int deriving (Show, Eq)
newtype Reward = Reward Int deriving (Show, Eq)
newtype Millis = Millis Int deriving (Show, Eq)
newtype Player = Player String deriving (Show, Eq)

data Message = SuperRegions [(SRID, Reward)] 
             | Regions [RID] 
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

data Move = StartingRegions [RID] -- currently exactly 6 RIDs allowed
          | PlaceArmies Player RID Int -- last val: #armies
          | AttackTransfer Player RID RID Int -- src Reg, tgt Reg, #armies
          | NoMoves
            deriving (Show, Eq)

step :: Message -> [Move]
step = undefined

formatOutput :: [Move] -> [String]
formatOutput = undefined
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
                ("regions":ws') -> Regions . map RID . map read $ ws' 
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
                            "place_armies" -> PlaceArmies (Player . head $ ws) (RID . read $ ws !! 2) (read $ ws !! 3) : parseMoves (drop 4 ws)
                            "attack/transfer" -> AttackTransfer (Player . head $ ws) (RID . read $ ws !! 2) (RID . read $ ws !! 3) (read $ ws !! 4) : parseMoves (drop 5 ws)                 

go :: [String] -> Message
go ws = case ws of
            ("place_armies":ws') -> PlaceArmiesRequest . Millis . read . head $ ws'
            ("attack/transfer":ws') -> AttackTransferRequest . Millis . read . head $ ws' 

handleInput :: [String] -> [String]
handleInput = concat . map formatOutput . map step . map parseInput

main :: IO ()
--main = interact $ unlines . handleInput . lines
main = interact $ unlines . map show . map parseInput . lines

