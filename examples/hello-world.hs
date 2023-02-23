module Hello where

import GHC.Conc (threadDelay)
import System.IO

-- | Game loop
render 0 = putStrLn "game-over"
render score
    | score >= 1, score <= 2 = pure ()
    | otherwise = putStrLn ("score: " ++ show score)

list | otherwise = [1, 2, 3]

two =
    let
        x = 1
        y = 1
     in
        x + y

main :: IO ()
main = render (21 * two)
