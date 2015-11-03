import Control.Monad.Free

data Result s = Get s | Fail deriving Show

instance Functor Result where
    fmap f (Get x) = Get (f x)
    fmap f Fail = Fail

liftF' :: Functor f => f r -> Free f r
liftF' cmd = Free (fmap Pure cmd)

getF :: a -> Free Result a
getF = liftF' . Get

failF :: Free Result ()
failF = liftF' Fail

makeGetResult :: Show a => a -> String
makeGetResult n = "Result is " ++ show n ++ "\n"

makeFailResult :: String
makeFailResult = "Nothing...\n"

makePureResult :: Show a => a -> String
makePureResult n = "return " ++ show n ++ "\n"

logging :: String -> IO()
logging = appendFile "file.log"

runProgram :: Show a => (String -> IO ()) -> Free Result a -> IO ()
runProgram f (Free (Get n)) = f (makeGetResult n) >> runProgram f n
runProgram f (Free Fail) = f makeFailResult
runProgram f (Pure n) = f $ makePureResult n

runStdIO :: Show a => Free Result a -> IO()
runStdIO = runProgram putStr

runFileIO :: Show a => Free Result a -> IO()
runFileIO = runProgram logging

sub = do
    x <- getF 10
    y <- getF x
    getF y

-- main = runStdIO sub
main = runFileIO sub
