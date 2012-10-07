{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Types
    ( PersonId (..)
    , Person (..)
    , PersonSummary (..)
    , Singleton (..)
    ) where

import Data.Aeson (ToJSON (..), FromJSON (..), (.:), (.=), object, Value (Object, Array))
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Vector as V

newtype PersonId = PersonId Text
    deriving (ToJSON, FromJSON, Ord, Eq)

data Person = Person Text Int
instance ToJSON Person where
    toJSON (Person name age) = object
        [ "name" .= name
        , "age" .= age
        ]
instance FromJSON Person where
    parseJSON (Object o) = Person
        <$> o .: "name"
        <*> o .: "age"
    parseJSON _ = fail "Expected an object"

data PersonSummary = PersonSummary PersonId Text
instance ToJSON PersonSummary where
    toJSON (PersonSummary pid name) = object
        [ "id" .= pid
        , "name" .= name
        ]

newtype Singleton a = Singleton { unSingleton :: a }
instance ToJSON a => ToJSON (Singleton a) where
    toJSON = Array . V.singleton . toJSON . unSingleton
instance FromJSON a => FromJSON (Singleton a) where
    parseJSON (Array a) =
        case V.toList a of
            [x] -> Singleton <$> parseJSON x
            _ -> fail "Not a single-element array"
    parseJSON _ = fail "Not an array"
