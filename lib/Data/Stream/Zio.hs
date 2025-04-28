{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances, StandaloneDeriving #-}

module Data.Stream.Zio ( module Data.Stream.Zio.Dealer
                       , module Data.Stream.Zio.Router
                       ) where

import Control.Applicative
import Control.Monad (forever)
import Control.Exception (bracket)
import Data.Monoid
import Data.String
import Data.ByteString (ByteString)
import System.IO
import System.Exit
import System.Environment ()
import System.ZMQ4
import Data.Serialize ( decode, encode, Serialize )
import qualified Data.ByteString.Char8 as CS
import GHC.Generics (Generic)

import Data.ULID
import Data.WideWord.Word128
import SSMonad ( SS, getConfig )
import SSNumeric ( SSNumeric )
import GHC.Plugins (assertPprMaybe)

import Data.Stream.Zio.Dealer
import Data.Stream.Zio.Router

-- import Data.HashMap.Internal.Array (new)

-- | Generate unique ULIDs as Word128 integers
--   for Sequence numbers
newSequence :: IO Sequence
newSequence = do
  i <- fmap integerToWW $ ulidToInteger <$> getULID
  return $ Sequence i
  where
    integerToWW i = fromInteger i

newtype Topic    = Topic    String  deriving (Show, Generic, Serialize)
newtype Address  = Address  String  deriving (Show, Generic, Serialize)
newtype Port     = Port     Int     deriving (Show, Generic, Serialize)

-- | NameID is the identifier for a stream instance.
--   TODO: Finbd a better name for this! StreamID?
newtype NameID   = NameID   String  deriving (Show, Generic, Serialize)

-- | Sequence ID for making sortable "UUIDs"
--   If the use gets more specific,
--   one should derive a new type from this.
newtype Sequence = Sequence Word128 deriving (Show, Generic)

deriving instance Serialize Word128 => Serialize Sequence

-- | Payload defines the handshake flow.
data Payload a = Payload     NameID Sequence a
               | Header      NameID Address Sequence Topic Port
               | StartStream NameID
               | Endtrean    NameID
               | NoData      NameID
               deriving (Show, Generic)

deriving instance (Serialize a, Serialize Word128) => Serialize (Payload a)

sendZioStream :: forall a1. (Serialize a1, Serialize Word128) => NameID
              -> Address
              -> Topic
              -> (Payload a1 -> Payload a1)
              -> SS ()
sendZioStream (NameID nid) (Address addr) (Topic topic) f = do
  cfg <- getConfig
  return ()
  where
    serPayload :: forall a2. (Serialize a2) => (Payload a2) -> ByteString
    serPayload = encode

recvZioStream ::  forall a1. (Serialize a1) => Address
              -> Topic
              -> (Payload a1 -> Bool)
              -> SS (Payload a1)
recvZioStream (Address addr) (Topic topic) f = undefined 
--  cfg <- getConfig
--  return _
--  where
--    deserPayload ::  forall a2. (Serialize a2) => ByteString -> Either String (Payload a2)
--    deserPayload = decode

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
