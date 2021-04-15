module Register
    where
register :: Monad m =>
    (m String)             -- an input function
    -> (String -> m ())    -- an output function
    -> m ()                -- result in the same monad
register inputFunction outputFunction = do
    s <- inputFunction
    outputFunction ("you entered " ++ s)

