{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module JSONParser where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Aeson.Encode.Pretty
import Data.Text
import Data.Foldable
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Text.Read (readMaybe)

jsonFile :: FilePath
jsonFile = "dependencies.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

data User = User {
    userName    :: Maybe String,
    email       :: Maybe String
} deriving Show

data Entry = Entry {
    name          :: String,
    description   :: Maybe String,
    version       :: String,
    dependencies  :: Maybe Value,
    mainFile      :: Maybe Value,
    author        :: Maybe Value,
    maintainers   :: Maybe [User],
    license       :: Maybe Value,
    licenses      :: Maybe Value,
    npmVersion    :: Maybe Value,
    nodeVersion   :: Maybe String,
    npmUser       :: Maybe User
} deriving Show

data License = License {
    licenseType :: String,
    url         :: String
} deriving Show

instance FromJSON User where
  parseJSON = withObject "user" $ \o -> do
    userName     <- o .:? "name"
    email        <- o .:? "email"
    return User{..}

instance FromJSON Entry where
  parseJSON = withObject "entry" $ \o -> do
    value           <- o .: "value"
    name            <- value .: "name"
    description     <- value .:? "description"
    version         <- value .: "version"
    dependencies    <- value .:? "dependencies"
    mainFile        <- value .:? "main"
    author          <- value .:? "author"
    maintainers     <- value .:? "maintainers"
    license         <- value .:? "license"
    licenses        <- value .:? "licenses"
    npmVersion      <- value .:? "_npmVersion"
    nodeVersion     <- value .:? "_nodeVersion"
    npmUser         <- value .:? "_npmUser"
    return Entry{..}

instance FromJSON License where
  parseJSON = withObject "license" $ \o -> do
    licenseType <- o .: "type"
    url         <- o .: "url"
    return License{..}

instance ToJSON User where
 toJSON (User name email) =
    object [ "name"  .= name
           , "email" .= email ]

instance ToJSON Entry where
 toJSON (Entry name description version dependencies mainFile author maintainers license licenses npmVersion nodeVersion npmUser) =
    object [ "name"         .= name
           , "description"  .= description
           , "version"      .= version
           , "dependencies" .= dependencies
           , "mainFile"     .= mainFile
           , "author"       .= author
           , "maintainers"  .= maintainers
           , "license"      .= license
           , "licenses"     .= licenses
           , "npmVersion"   .= npmVersion
           , "nodeVersion"  .= nodeVersion
           , "npmUser"      .= npmUser ]

instance ToJSON License where
 toJSON (License licenseType url) =
    object [ "type" .= licenseType
           , "url"  .= url ]

printE :: [Entry] -> IO()
printE [x]    = B.appendFile "result.json" (encodePretty x)
printE (x:xs) = B.appendFile "result.json" (encodePretty x) >> printE xs

printAsJSON :: [Entry] -> IO()
printAsJSON entries = B.writeFile "result.json" (encodePretty entries)

main :: IO ()
main = do
 d <- (eitherDecode <$> getJSON) :: IO (Either String [Entry])
 case d of
  Left err -> putStrLn err
  Right ps -> printAsJSON ps