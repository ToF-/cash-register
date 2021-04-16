module Register (Money (..), register, readMoney, showMoney)
    where

import Text.Printf     (printf)
import Data.List.Split (splitOn)

data Money = Money Integer
    deriving (Eq, Show)

register :: Monad m =>
    Money
    -> (m String)             -- an input function
    -> (String -> m ())    -- an output function
    -> m ()                -- result in the same monad
register (Money total) inputFunction outputFunction = do
    s <- inputFunction
    case s of 
      "" -> outputFunction (showMoney (Money total))
      s -> do
          let (Money value) = readMoney s
          outputFunction (showMoney (Money (total + value)))


readMoney :: String -> Money
readMoney s = case splitOn "." s of
                [intPart,centPart] -> readMoneyWithCents intPart centPart
                [intPart] -> Money $ (read s) * 100
                [] -> error "readMoney: empty argument"

    where
        readMoneyWithCents [] [c] = mkMoney 0 (read [c] * 10)
        readMoneyWithCents [] cs  = mkMoney 0 (read (take 2 cs))
        readMoneyWithCents i []  =  mkMoney (read i) 0
        readMoneyWithCents i [c] =  mkMoney (read i) (read [c] * 10)
        readMoneyWithCents i cs  =  mkMoney (read i) (read (take 2 cs))

mkMoney :: Integer -> Integer -> Money
mkMoney units cents = Money $ units * 100 + cents

showMoney :: Money -> String
showMoney (Money value) = printf "%d.%02d" (value `div` 100) (value `mod` 100)
