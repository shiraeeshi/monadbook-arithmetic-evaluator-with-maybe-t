module Main where

import Lib
import Control.Monad.Reader (runReader)

main :: IO ()
main = do
  let assignment = [
          ("x", 0)
          , ("y", 1)
          , ("z", 1)
        ]
      expr = Sequence [
          Assign "x" (Literal 1)
          , Assign "y" (Literal 5)
          , Assign "z" (Op Add (Var "x") (Var "y"))
          , Op Add (Var "x") (Var "z")
        ]
      expr2 = Sequence [
          Assign "x" (Literal 100)
          , Op Add (Var "x") (Var "y")
        ]
      result = runState (runMaybeT $ eval3 expr) assignment
  putStrLn $ "result: " ++ (show result)
