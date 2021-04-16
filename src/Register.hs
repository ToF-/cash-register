module Register (register, readMoney, showMoney)
    where

import Text.Printf

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
readMoney s = Money $ (read s) * 100

showMoney :: Money -> String
showMoney (Money value) = printf "%d.%02d" (value `div` 100) (value `mod` 100)
