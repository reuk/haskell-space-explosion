{-# LANGUAGE DeriveGeneric #-}

module Main where

import Prelude hiding (mapM_)

import Pipes.Parse (evalStateT, foldAll, foldAllM)
import Pipes
import System.Environment
import System.IO hiding (writeFile)
import Pipes.Aeson.Unchecked
import qualified Pipes.ByteString as P
import Lens.Family.State.Strict (zoom)
import GHC.Generics
import Data.Aeson
import Data.Array.IO
import Control.Applicative
import Data.Foldable

--  data and instance declarations - all fairly standard stuff

data RayTrace = RayTrace (C3 Double) [Impulse]
    deriving (Eq, Show, Generic)

instance FromJSON RayTrace
instance ToJSON RayTrace

impulses :: RayTrace -> [Impulse]
impulses (RayTrace _ i) = i

data Impulse = Impulse  {   time :: Double
                        ,   amplitude :: C3 Double
                        }   deriving (Eq, Show, Generic)

instance FromJSON Impulse
instance ToJSON Impulse

data C3 a = C3 a a a 
    deriving (Eq, Show, Generic)

instance (FromJSON a) => FromJSON (C3 a)
instance (ToJSON a) => ToJSON (C3 a)

instance Functor C3 where
    fmap f (C3 a b c) = C3 (f a) (f b) (f c)

instance Applicative C3 where
    pure x = C3 x x x
    (C3 f g h) <*> (C3 x y z) = C3 (f x) (g y) (h z)

instance Foldable C3 where
    foldr f b (C3 x y z) = f x $ f y $ f z b

instance (Num a) => Num (C3 a) where
    (+) = (<*>) . (<$>) (+)
    (-) = (<*>) . (<$>) (-)
    (*) = (<*>) . (<$>) (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
    
--  functions

lastSampleTimeForFile :: String -> IO Double
lastSampleTimeForFile f = 
    withFile f ReadMode $ \hIn -> 
    evalStateT (zoom decoded (foldAll maxSample 0 id)) (P.fromHandle hIn)

maxSample :: Double -> RayTrace -> Double
maxSample = flip $ max . time . last . impulses

--  the following four functions eat a lot of memory and I don't know why

channelForFile :: FilePath -> Int -> Double -> IO (IOArray Int (C3 Double))
channelForFile f l sr = withFile f ReadMode $ \hIn -> do
    t <- newArray (0, l) (pure 0)
    evalStateT (parseChannel t sr) (P.fromHandle hIn)

parseChannel t sr = 
    zoom decoded $ foldAllM 
        (\ _ rt -> channelUpdateLoop sr rt t) 
        (return ()) 
        (\_ -> return t)

channelUpdateLoop :: Double -> RayTrace -> IOArray Int (C3 Double) -> IO ()
channelUpdateLoop sr rt arr = 
    mapM_ (sampleUpdate sr arr) (impulses rt)

sampleUpdate :: Double -> IOArray Int (C3 Double) -> Impulse -> IO ()
sampleUpdate sr arr impulse = do
    let s = secondsToSamples sr $ time impulse
    p <- readArray arr s
    writeArray arr s (amplitude impulse + p)

secondsToSamples :: Integral b => Double -> Double -> b
secondsToSamples sampleRate t = round (sampleRate * t)

main :: IO ()
main = do
    args <- getArgs
    case length args of
        1 -> do
            let sr = 44100.0
            let fp = head args
            l <- lastSampleTimeForFile fp
            t <- channelForFile fp (secondsToSamples sr l) sr
            putStrLn "done"
        _ ->
            putStrLn "program takes one argument"
