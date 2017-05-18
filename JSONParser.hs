{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module JSONParser where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty
import Data.Text
import Data.Foldable
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Text.Read (readMaybe)

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
    description   :: Maybe String,
    version       :: String,
    mainFile      :: Maybe String,
    --author        :: User,
    --license       :: Maybe String,
    npmVersion    :: Maybe String,
    nodeVersion   :: Maybe String,
    npmUser       :: Maybe User
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
    description     <- value .:? "description"
    version         <- value .: "version"
    mainFile        <- value .:? "main"
    --author          <- value .: "author"
    --license         <- value .:? "license"
    npmVersion      <- value .:? "_npmVersion"
    nodeVersion     <- value .:? "_nodeVersion"
    npmUser         <- value .:? "_npmUser"
    return Entry{..}

instance ToJSON User where
 toJSON (User name email) =
    object [ "name"  .= name
           , "email" .= email ]

instance ToJSON Entry where
 toJSON (Entry name description version mainFile npmVersion nodeVersion npmUser) =
    object [ "name"         .= name
           , "description"  .= description
           , "version"      .= version
           , "mainFile"     .= mainFile
           --, "author"       .= author
           --, "license"      .= license
           , "npmVersion"   .= npmVersion
           , "nodeVersion"  .= nodeVersion
           , "npmUser"      .= npmUser ]

main :: IO ()
main = do
 d <- (eitherDecode <$> getJSON) :: IO (Either String [Entry])
 case d of
  Left err -> putStrLn err
  Right ps -> B.writeFile "result.json" (encodePretty ps)

-- ToDo Licenses, Author, Dependencies and Maintainers