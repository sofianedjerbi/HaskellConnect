module Main where

import Board
import Computer

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game



background :: Color
background = makeColorI 125 65 225 255 -- aka Platter color

-- |Window
window :: Display
window = InWindow "Connect 4 - Haskell" (700, 600) (0, 0)

-- |Handle mouse left button to play
handleMouse :: Event -> [[Int]] -> [[Int]]
handleMouse (EventKey (MouseButton LeftButton) Down _ (x, _)) b
   | f = p
   | otherwise = playComputer 5 p
   where
     p = if isFinished b then b else place 1 ((round x + 350) `div` 100) b
     f = isFinished p
handleMouse _ b = b

-- |Identity (we don't update)
update :: Float -> [[Int]] ->  [[Int]]
update _ b = b

------ MAIN ------
main :: IO ()
main = play window background 10 newBoard drawBoard handleMouse update
