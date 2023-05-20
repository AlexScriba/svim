{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import Control.Monad.State (StateT (runStateT), get, gets, lift, put, runStateT)
import Data.ByteString.UTF8 (fromString, toString)
import Data.Yaml (FromJSON, ParseException, ToJSON, decodeEither', encode, object, parseJSON, toJSON, withObject, (.!=), (.:))
import qualified Data.Yaml as Yml ((.=))
import Lens.Micro.Mtl (use)
import Lens.Micro.TH (makeLenses)
import System.Directory (doesFileExist)

data ConfigData where
    ConfigData ::
        { _getCommand :: String
        , _numRecent :: Int
        , _saved :: [String]
        , _paths :: [String]
        , _recent :: [String]
        } ->
        ConfigData
    deriving (Show)

data Config where
    Config ::
        { _confData :: ConfigData
        , _rootDir :: String
        } ->
        Config
    deriving (Show)

makeLenses ''ConfigData
makeLenses ''Config

splitConf :: Config -> (ConfigData, FilePath)
splitConf Config{_confData = conf, _rootDir = dir} = (conf, dir)

type ConfigIO a = StateT Config IO a

io :: IO a -> ConfigIO a
io = lift

instance FromJSON ConfigData where
    parseJSON = withObject "Config" $ \o -> do
        command <- o .: "command"
        numR <- o .: "num_recent"
        s <- o .: "saved" .!= []
        p <- o .: "paths" .!= []
        r <- o .: "recent" .!= []
        return $ ConfigData command numR s p r

instance ToJSON ConfigData where
    toJSON config =
        object
            [ "command" Yml..= _getCommand config
            , "num_recent" Yml..= _numRecent config
            , "saved" Yml..= _saved config
            , "paths" Yml..= _paths config
            ]

getConfig :: FilePath -> IO (Either ParseException ConfigData)
getConfig dir = do
    let path = dir ++ "/config.yaml"
    input <- fromString <$> readFile path
    return $ decodeEither' input

saveConfig :: FilePath -> ConfigData -> IO ()
saveConfig dir conf = do
    let contents = toString $ encode conf
        path = dir ++ "/config.yaml"

    writeFile path contents
