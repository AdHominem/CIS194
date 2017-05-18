{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module JSONParser where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics

jsonFile :: FilePath
jsonFile = "data.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

data User = User {
    userName    :: String,
    email       :: String
    } deriving (Show)

data Entry = Entry {
    name          :: String,
    version       :: String,
    mainFile      :: Maybe String,
    license       :: Maybe String,
    npmVersion    :: String,
    nodeVersion   :: Maybe String,
    npmUser       :: User
    } deriving (Show)

instance FromJSON User where
  parseJSON = withObject "user" $ \o -> do
    userName     <- o .: "name"
    email        <- o .: "email"
    return User{..}

instance FromJSON Entry where
  parseJSON = withObject "entry" $ \o -> do
    value           <- o .: "value"
    name            <- value .: "name"
    version         <- value .: "version"
    mainFile        <- value .:? "main"
    license         <- value .:? "license"
    npmVersion      <- value .: "_npmVersion"
    nodeVersion     <- value .:? "_nodeVersion"
    npmUser         <- value .: "_npmUser"
    return Entry{..}

instance ToJSON User where
 toJSON (User name email) =
    object [ "name"  .= name
           , "email" .= email ]

instance ToJSON Entry where
 toJSON (Entry name version mainFile license npmVersion nodeVersion npmUser) =
    object [ "name"         .= name
           , "version"      .= version
           , "mainFile"     .= mainFile
           , "license"      .= license
           , "npmVersion"   .= npmVersion
           , "nodeVersion"  .= nodeVersion
           , "npmUser"      .= npmUser ]

main :: IO ()
main = do
 d <- (eitherDecode <$> getJSON) :: IO (Either String [Entry])
 case d of
  Left err -> putStrLn err
  Right ps -> print ps

-- ToDo Dependencies and Maintainers
