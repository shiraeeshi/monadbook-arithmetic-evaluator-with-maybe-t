module Lib
    (
      Expr(..)
      , Op(..)
      , Name
      , Assignment
      , State(..)
      , MaybeT(..)
      , eval3
    ) where

import Control.Monad.Reader --(Reader, ask)
import Control.Monad.Fail (MonadFail)
--import Control.Monad.Trans.Maybe --(MaybeT, runMaybeT)

type Name = String
data Expr = Literal Integer
          | Var Name
          | Op Op Expr Expr
          | Assign Name Expr
          | Sequence [Expr]

data Op = Add | Subtract | Multiply | Divide

type Assignment = [(Name, Integer)]

----------------------------------
--eval3 :: Expr -> MaybeT (State Assignment) Integer
----------------------------------

----------------------------------
-- State
----------------------------------

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State x) = State $ \s ->
    let (a, s') = x s
    in (f a, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  (State sf) <*> (State sx) = State $ \s ->
    let (f, s') = sf s
        (x, s'') = sx s'
    in (f x, s'')

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  (State g) >>= f = State $ \s ->
    let (a, s') = g s
        st = f a
    in runState st s'

put :: s -> State s ()
put x = State $ \_ -> ((), x)

get :: State s s
get = State $ \s -> (s, s)

----------------------------------
-- MaybeT
----------------------------------

data MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT x) = MaybeT $ fmap (fmap f) x

instance Monad m => Applicative (MaybeT m) where
  pure x = MaybeT $ return (Just x)
  (MaybeT ml) <*> (MaybeT mr) = MaybeT $ do
    maybe_l <- ml
    maybe_r <- mr
    return $ do
      l <- maybe_l
      r <- maybe_r
      return $ l r

instance Monad m => Monad (MaybeT m) where
  -- return :: a -> MaybeT m a
  return x = MaybeT $ return (Just x)
  -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT x) >>= f = MaybeT $ do
    y <- x
    case y of
      Nothing -> return Nothing
      Just z  -> runMaybeT (f z)

----------------------------------
-- eval3
----------------------------------


eval3 :: Expr -> MaybeT (State Assignment) Integer
eval3 (Literal n) = return n
-- lookup v a -- lookup returns Nothing if not present
--eval3 (Var v)     = MaybeT $ lookup <$> v
eval3 (Var v) = MaybeT $ do
  a <- get
  return $ lookup v a
eval3 (Op o x y)  = do
  u <- eval3 x
  v <- eval3 y
  case o of
    Add      -> return (u + v)
    Subtract -> return (u - v)
    Multiply -> return (u * v)
    Divide   -> if v == 0 then emptyResult
                          else return (u `div` v)
eval3 (Assign name expr) = MaybeT $ do
  v <- runMaybeT (eval3 expr)
  a <- get
  case v of
    Nothing -> emptyResultState
    Just value -> do
      let update pair@(eachName, eachValue) = if eachName == name
                                                then (eachName, value)
                                                else pair
      put $ fmap update a
      return $ Just value
eval3 (Sequence [x]) = eval3 x
eval3 (Sequence (x:xs)) = do
  eval3 x
  eval3 (Sequence xs)

emptyResult :: MaybeT (State Assignment) Integer
emptyResult = MaybeT $ State $ \s -> (Nothing, s)

emptyResultState :: State Assignment (Maybe Integer)
emptyResultState = State $ \s -> (Nothing, s)
