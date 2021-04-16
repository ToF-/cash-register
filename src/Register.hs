module Register (register, readMoney, showMoney)
    where

import Text.Printf     (printf)
import Data.List.Split (splitOn)

data Money = Money Integer
    deriving (Eq, Show)

register :: Monad m =>
    (m String)             -- an input function
    -> (String -> m ())    -- an output function
    -> m ()                -- result in the same monad
register inputFunction outputFunction = do
    s <- inputFunction
    outputFunction (showMoney (readMoney s))

readMoney :: String -> Money
readMoney s = case splitOn "." s of
                [intPart,centPart] -> readMoneyWithCents intPart centPart
                [intPart] -> Money $ (read s) * 100
                [] -> error "readMoney: empty argument"

    where
        readMoneyWithCents [] [c] = Money $ (read [c]) * 10
        readMoneyWithCents [] cs  = Money $ read (take 2 cs)
        readMoneyWithCents i []  = Money $ (read i) * 100
        readMoneyWithCents i [c] = Money $ (read i) * 100 + (read [c]) * 10
        readMoneyWithCents i cs  = Money $ (read i) * 100 + (read (take 2 cs))

showMoney :: Money -> String
showMoney (Money value) = printf "%d.%02d" (value `div` 100) (value `mod` 100)
