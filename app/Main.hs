module Main where

import Lib
import Control.Monad.Reader (runReader)

oldmain :: IO ()
oldmain = do
  let assignment = [
          ("x", 0)
          , ("y", 1)
        ]
      expr = Op Subtract (Var "x") (Var "y")
      result = eval expr assignment
  putStrLn $ "result: " ++ (show result)

main2 :: IO ()
main2 = do
  let assignment = [
          ("x", 0)
          , ("y", 1)
        ]
      expr = Op Add (Var "x") (Var "y")
      result = runReader (evalWithReader expr) assignment
  putStrLn $ "result: " ++ (show result)

main :: IO ()
main = do
  let assignment = [
          ("x", 0)
          , ("y", 1)
        ]
      expr = Op Add (Var "x") (Var "y")
      result = runState (runMaybeT $ eval3 expr) assignment
  putStrLn $ "result: " ++ (show result)
