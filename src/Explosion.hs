module Explosion where

import Pipes.Parse (foldAll, Parser, Producer)
import Pipes.ByteString (ByteString, fromLazy)
import Pipes.Aeson (DecodingError)
import Pipes.Aeson.Unchecked (decoded)
import Data.List (intercalate)
import Data.ByteString.Lazy.Char8 (pack)
import Lens.Family (view)
import Lens.Family.State.Strict (zoom)

produceString :: Producer ByteString IO ()
produceString = fromLazy $ pack $ intercalate " " $ map show [1..10000000]

produceInts :: Producer Int IO (Either (DecodingError, Producer ByteString IO ()) ())
produceInts = view decoded produceString

produceInts' :: Producer Int IO ()
produceInts' = produceInts >> return ()

parseBiggest :: Parser ByteString IO Int
parseBiggest = zoom decoded (foldAll max 0 id)
