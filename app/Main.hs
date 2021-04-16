module Main where

import Register (Money (..), register)

main :: IO ()
main = register (Money 0) getLine putStrLn
