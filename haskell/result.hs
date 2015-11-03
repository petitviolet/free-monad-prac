data Result s = Get s | None deriving Show

instance Functor Result where
    fmap f (Get x) = Get (f x)
    fmap f None = None

instance Applicative Result where
    pure = Get
    None <*> _ = None
    (Get f) <*> x = fmap f x

instance Monad Result where
    return = pure
    None >>= f = None
    Get x >>= f = f x
    fail _ = None

main = do
    print $ fmap (\x -> x * 2) $ Get 10
    print $ pure (\x y -> x * y) <*> Get 10 <*> Get 2
    print $ Get 10 >>= (\x -> Get (x * 2))
    return 0
