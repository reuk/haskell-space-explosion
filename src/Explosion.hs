module Explosion where

import Data.ByteString.Lazy (intercalate)
import Pipes.Parse (foldAll, Parser, Producer)
import Pipes.ByteString (ByteString, fromLazy)
import Pipes.Aeson (DecodingError)
import Pipes.Aeson.Unchecked (decoded)
import Data.ByteString.Lazy.Char8 (pack)
import Lens.Family (view)
import Lens.Family.State.Strict (zoom)

intList :: [Int]
intList = [1..1000000]

produceString :: Producer ByteString IO ()
produceString = fromLazy $ intercalate (pack " ") $ map (pack . show) intList

produceInts :: Producer Int IO (Either (DecodingError, Producer ByteString IO ()) ())
produceInts = view decoded produceString

produceInts' :: Producer Int IO ()
produceInts' = produceInts >> return ()

parseBiggest :: Parser ByteString IO Int
parseBiggest = zoom decoded (foldAll max 0 id)
