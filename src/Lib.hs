module Lib
    (
      Expr(..)
      , Op(..)
      , Name
      , Assignment
      , eval
      , evalWithReader
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

data Op = Add | Subtract | Multiply | Divide

type Assignment = [(Name, Integer)]

eval :: Expr -> Assignment -> Maybe Integer
eval (Literal n) _ = return n
eval (Var v)     a = lookup v a -- lookup returns Nothing if not present
eval (Op o x y)  a = do u <- eval x a
                        v <- eval y a
                        case o of
                          Add      -> return (u + v)
                          Subtract -> return (u - v)
                          Multiply -> return (u * v)
                          Divide   -> if v == 0 then Nothing
                                                else return (u `div` v)


evalWithReader :: Expr -> Reader Assignment (Maybe Integer)
evalWithReader (Literal n) = return (Just n)
evalWithReader (Var v)     = do
  a <- ask
  return $ lookup v a -- lookup returns Nothing if not present
evalWithReader (Op o x y) = do u <- evalWithReader x
                               v <- evalWithReader y
                               case (u, v) of
                                 (Nothing, _) -> return Nothing
                                 (_, Nothing) -> return Nothing
                                 (Just u', Just v') ->
                                   case o of
                                     Add      -> return (Just (u' + v'))
                                     Subtract -> return (Just (u' - v'))
                                     Multiply -> return (Just (u' * v'))
                                     Divide   -> if v' == 0 then return Nothing
                                                            else return (Just (u' `div` v'))

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
    Divide   -> if v == 0 then divisionByZeroResult
                          else return (u `div` v)

divisionByZeroResult :: MaybeT (State Assignment) Integer
divisionByZeroResult = MaybeT $ State $ \s -> (Nothing, s)
