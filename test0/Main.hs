module Main where

import Explosion (produceInts')
import Pipes.Prelude (fold)

test0 :: IO ()
test0 = do
    biggest <- fold max 0 id produceInts'
    print $ show biggest

main :: IO ()
main = test0
