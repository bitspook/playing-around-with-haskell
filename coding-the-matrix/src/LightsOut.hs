module LightsOut where

import           Data.List
import           Data.Maybe
import           Debug.Trace

data GF2 = Zero | One deriving (Show, Eq)

add :: GF2 -> GF2 -> GF2
add Zero One  = add One Zero
add One  Zero = One
add One  One  = Zero
add Zero Zero = Zero

type Cell = (Integer, Integer)
type Light = (Cell, GF2)
type LightsGrid = [Light]

addLights :: Light -> Light -> Light
addLights (a, l1) (_, l2) = (a, add l1 l2)

findLight :: LightsGrid -> Cell -> Light
findLight g c = fromMaybe (c, Zero) $ find (\l' -> fst l' == c) g

pressBtn :: LightsGrid -> Cell -> LightsGrid
pressBtn grid cell = case cell of
        (x, y) ->
                filter (\l -> snd l == One)
                        . map (findLight grid)
                        $ [ (x    , y)
                          , (x - 1, y)
                          , (x + 1, y)
                          , (x    , y - 1)
                          , (x    , y + 1)
                          ]

addLightsGrid :: LightsGrid -> LightsGrid -> LightsGrid
addLightsGrid grid move = case grid of
        [] -> []
        (x : xs) ->
                addLights x (findLight move $ fst x) : addLightsGrid xs move

isSolution :: LightsGrid -> LightsGrid -> Bool
isSolution g = (==) 0 . length . filter (\l -> snd l == One) . addLightsGrid g

goDark :: LightsGrid -> [Cell]
goDark grid = concat $ goDark' allBtnCombs 1
    where
        goDark' :: [[Cell]] -> Int -> [[Cell]]
        goDark' btns tryCount = case moves tryCount of
                []     -> []
                moves' -> trace
                        ("Try: " ++ show tryCount ++ "; Moves: " ++ show moves')
                        (if isSolution grid (add' moves')
                                then btns
                                else goDark' btns (tryCount + 1)
                        )

        allBtnCombs :: [[Cell]]
        allBtnCombs = drop 1 . subsequences . map fst $ grid

        moves :: Int -> [LightsGrid]
        moves tryCount = map
                (pressBtn grid)
                (concat $ drop (tryCount - 1) $ take tryCount allBtnCombs)

        add' :: [LightsGrid] -> LightsGrid
        add' = foldr addLightsGrid []
