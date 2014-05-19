module Main where

import Explosion (parseBiggest, produceString)
import Pipes.Parse (evalStateT)

test1 :: IO ()
test1 = do
    biggest <- evalStateT parseBiggest produceString
    print $ show biggest

main :: IO ()
main = test1
