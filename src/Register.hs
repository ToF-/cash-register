module Register
    where
import Data.List.Split
import Data.Fixed

data Price = Price (Fixed E2)

instance Show Price where
    show (Price v) = showFixed False v

instance Read Price where
  readsPrec _ input =
      let parts = (splitOn "." input)
          intPart = read (head parts) 
          centPart = case parts of
                       [_,[c]] -> (read [c]) * 10
                       [_,[c,d]]   -> read [c,d]
                       [_] -> 0
      in [(Price (MkFixed ((intPart + centPart))),"")]

readPrice s = Price $ case (splitOn "." s) of
                [i,[c]] -> ((read i) * 100) + ((read [c]) * 10)
                [i,cs]  -> ((read i) * 100) + (read cs)
                [i]     -> ((read i) * 100)

register :: Monad m =>
    (m String)             -- an input function
    -> (String -> m ())    -- an output function
    -> m ()                -- result in the same monad
register inputFunction outputFunction = do
    s <- inputFunction
    let price = readPrice s
    outputFunction (show price)

