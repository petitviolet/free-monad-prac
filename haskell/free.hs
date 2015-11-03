data Free f r = Free (f (Free f r)) | Pure r

instance Functor f => Functor (Free f) where
    fmap f (Pure x) = Pure (f x)
    fmap f (Free x) = Free $ fmap (fmap f) x

instance Applicative f => Applicative (Free f) where
    Pure f <*> x = fmap f x
    Free f <*> x = Free (fmap (<*> x) f)
    pure = Pure

-- instance Functor f => Monad (Free f) where
instance Applicative f => Monad (Free f) where
    return = Pure
    Free x >>= f = Free (fmap (>>= f) x)
    Pure x >>= f = f x

data MyProgram n = Old Int n | Hey n | Hello n | GoodBye

instance Functor MyProgram where
    fmap f (Old o n) = Old o (f n)
    fmap f (Hey n) = Hey (f n)
    fmap f (Hello n) = Hello (f n)
    fmap f GoodBye = GoodBye

runProgram :: Show r => Free MyProgram r -> IO ()
runProgram (Free (Old o n)) = putStrLn ("I'm " ++ show o ++ " years old") >> runProgram n
runProgram (Free (Hey n)) = putStrLn "Hey!!!" >> runProgram n
runProgram (Free (Hello n)) = putStrLn "Hello!!!" >> runProgram n
runProgram (Free GoodBye) = putStrLn "GoodBye!!!"
runProgram (Pure r) = putStrLn $ "return " ++ show r


liftF :: Functor f => f r -> Free f r
liftF cmd = Free (fmap Pure cmd)

old :: Int -> Free MyProgram ()
old o = liftF (Old o ())

hey :: Free MyProgram ()
hey = liftF (Hey ())

hello :: Free MyProgram ()
hello = liftF (Hello ())

goodBye :: Free MyProgram ()
goodBye = liftF GoodBye

-- sub :: Free MyProgram ()
-- sub = do
--     hello
--     old 25

-- pg :: Free MyProgram ()
-- pg = do
--     hey
--     -- sub
--     hey
--     goodBye
--
-- main :: IO ()
-- main = runProgram pg
