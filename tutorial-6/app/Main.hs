module Main where

import System.Environment

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn "Hello, world!!"

-- main = do
--   putStrLn "Hello, what's your name"
--   s <- getLine
--   putStrLn $ "Hello, " ++ s

-- main = do
--   putStrLn "Hello, what's your name" >> getLine >>= \x -> putStrLn $ "Hello, " ++ x

-- main = do
--   args <- getArgs
--   print args
--

-- main = interact reverse

data Failure a = Fail | Ok a deriving (Show)

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

instance Functor Failure where
  fmap f (Ok x) = Ok $ f x
  fmap f Fail = Fail


-- class (Functor f) => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Failure where
  pure              = Ok
  (Ok f) <*> (Ok x) = Ok $ f x
  Fail <*> _        = Fail
  _ <*> Fail        = Fail

data Person = Person String Int Int deriving (Show)

-- class (Applicative m) => Monad m where
--   return :: a -> m a
--   return = pure

--   (>>=) :: m a -> (a -> m b -> m b

instance Monad Failure where
  (Ok x) >>= f  = f x
  Fail   >>= f  = Fail

safeDivide :: Failure Int -> Failure Int -> Failure Int
safeDivide xm ym = xm >>= (\x ->
                   ym >>= (\y ->
                      if y == 0 then Fail 
                                else return (x `div` y)))



-- easier example with do but it's the same as above
safeDivide' xm ym = do
                x <- xm
                y <- ym
                if y == 0 then Fail 
                          else return (x `div` y)


