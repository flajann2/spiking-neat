{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Data.Stream.Zio where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.String
import Data.ByteString (ByteString)
import System.IO
import System.Exit
import System.Environment
import System.ZMQ4.Monadic
import Data.Serialize ( decode, encode, Serialize )
import qualified Data.ByteString.Char8 as CS
import GHC.Generics (Generic)

import SSMonad ( SS )
import SSNumeric ( SSNumeric )
import GHC.Plugins (assertPprMaybe)
-- import Data.HashMap.Internal.Array (new)

newtype Topic    = Topic    String deriving (Show, Generic, Serialize)
newtype Address  = Address  String deriving (Show, Generic, Serialize)
newtype Port     = Port     Int    deriving (Show, Generic, Serialize)
newtype NameID   = NameID   String deriving (Show, Generic, Serialize)
newtype Sequence = Sequence Int    deriving (Show, Generic, Serialize)

data Payload = Payload     NameID Sequence [SSNumeric]
             | EndOfStrean NameID
             | NoData      NameID
             | Header      NameID Address Sequence Topic Port
             deriving (Show, Generic, Serialize)

sendZioStream :: NameID -> Address -> Topic -> (Payload -> Payload) -> SS ()
sendZioStream (NameID nid) (Address addr) (Topic topic) f = do
  return ()
  where
    serPayload :: Payload -> ByteString
    serPayload = encode

recvZioStream :: Address -> Topic -> (Payload -> Bool) -> SS Payload
recvZioStream (Address addr) (Topic topic) f = undefined
  where
    deserPayload :: ByteString -> Either String Payload
    deserPayload = decode

---- import Data.Serialize
---- import Data.ByteString (ByteString)
---- import qualified Data.ByteString.Char8 as BS
---- import GHC.Generics (Generic)
---- 
---- data MyData = MyData Int String deriving (Show, Generic)
---- 
---- instance Serialize MyData
---- 
---- serializeList :: [MyData] -> ByteString
---- serializeList = encode
---- 
---- deserializeList :: ByteString -> Either String [MyData]
---- deserializeList = decode
---- 
---- main :: IO ()
---- main = do
----     let myList = [MyData 1 "Hello", MyData 2 "World"]
----     let serialized = serializeList myList
----     putStrLn $ "Serialized data: " ++ BS.unpack serialized
----     
----     case deserializeList serialized of
----         Left err -> putStrLn $ "Error during deserialization: " ++ err
----         Right deserialized -> putStrLn $ "Deserialized data: " ++ show deserialized
