{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

import Control.Monad.State (StateT (runStateT), get, gets, lift, put, runStateT)
import Data.ByteString.UTF8 (fromString, toString)
import Data.List.Split (split, splitOn)
import Data.Maybe (listToMaybe)
import Data.Yaml
import Data.Yaml.Pretty (encodePretty)
import System.Directory (
    canonicalizePath,
    doesFileExist,
    doesPathExist,
    getHomeDirectory,
 )
import System.Directory.Internal.Prelude (IOMode (WriteMode), hFlush, stdout)
import System.Environment
import System.Exit (ExitCode (..))
import System.IO (
    IOMode (AppendMode, ReadMode),
    hClose,
    hGetContents,
    hPutStrLn,
    openFile,
    withFile,
 )
import System.Process (system)
import Text.Read (choice, readListPrecDefault, readMaybe)

type ConfigPath = String

type Path = String

data ConfigData where
    ConfigData ::
        { getCommand :: String
        , numRecent :: Int
        , saved :: [String]
        , paths :: [String]
        } ->
        ConfigData
    deriving (Show)

data Config where
    Config ::
        { confData :: ConfigData
        , rootDir :: String
        } ->
        Config
    deriving (Show)

splitConf :: Config -> (ConfigData, Path)
splitConf Config{confData = conf, rootDir = dir} = (conf, dir)

type ConfigIO a = StateT Config IO a

instance FromJSON ConfigData where
    parseJSON = withObject "Config" $ \o -> do
        command <- o .: "command"
        numR <- o .: "num_recent"
        s <- o .: "saved" .!= []
        p <- o .: "paths" .!= []
        return $ ConfigData command numR s p

instance ToJSON ConfigData where
    toJSON config =
        object
            [ "command" .= getCommand config
            , "num_recent" .= numRecent config
            , "saved" .= saved config
            , "paths" .= paths config
            ]

data Action where
    SetCommand :: String -> Action
    SetNumRecent :: String -> Action
    Open :: Path -> Action
    OpenAndSave :: String -> Path -> Action
    GetRecent :: Action
    GetFavourite :: Action
    DeleteFavourite :: Action
    Error :: String -> Action
    deriving (Read, Show)

parseArgs :: [String] -> Action
parseArgs [] = GetRecent
parseArgs ("--set-editor" : cmd : _) = SetCommand cmd
parseArgs ("--set-num-recent" : num : _) = SetNumRecent num
parseArgs ("-s" : name : path : _) = OpenAndSave name path
parseArgs ("-f" : _) = GetFavourite
parseArgs ("-d" : _) = DeleteFavourite
parseArgs (path : _) = Open path

execute :: Action -> ConfigIO ()
execute action = case action of
    SetCommand cmd -> setConfigCommand cmd
    SetNumRecent num -> setNumRecent num
    Open path -> handleOpen path
    OpenAndSave name path -> handleOpenAndSave name path
    GetFavourite -> handleGetFavourite
    DeleteFavourite -> handleDeleteFavourite
    GetRecent -> handleGetRecent
    Error msg -> lift $ handleError msg

setConfigCommand :: String -> ConfigIO ()
setConfigCommand cmd = do
    (conf, confPath) <- gets splitConf
    let newConf = conf{getCommand = cmd}

    lift $ saveConfig confPath newConf

setNumRecent :: String -> ConfigIO ()
setNumRecent num = do
    (conf, confPath) <- gets splitConf

    case (readMaybe num :: Maybe Int) of
        Nothing -> execute $ Error "Invalid number"
        Just n ->
            let newConf = conf{numRecent = n}
             in lift $ saveConfig confPath newConf

handleGetRecent :: ConfigIO ()
handleGetRecent = do
    (conf, savePath) <- gets splitConf
    prev <- lift $ getRecents savePath

    case prev of
        [] -> lift $ putStrLn "No Recent Projects"
        prevPaths -> do
            choice <- lift $ getSelection $ pathToOption <$> prev

            let index = subtract 1 <$> choice
                pathChoice = index >>= getMaybe prev

            case pathChoice of
                Just path -> execute $ Open path
                Nothing -> execute $ Error "Invalid Selection"

handleOpen :: String -> ConfigIO ()
handleOpen path = do
    (config, savePath) <- gets splitConf
    absolutePath <- lift $ canonicalizePath path

    let command = "cd " ++ path ++ " && " ++ getCommand config
    exitCode <- lift $ system command

    case exitCode of
        ExitSuccess -> lift $ saveRecents config savePath absolutePath
        ExitFailure code -> lift $ putStrLn $ "Code: " ++ show code

handleOpenAndSave :: String -> String -> ConfigIO ()
handleOpenAndSave name path = do
    config <- get
    absPath <- lift $ canonicalizePath path

    let (configData, savePath) = splitConf config

        prevSaved = saved configData
        prevPaths = paths configData

        combs =
            (name, absPath)
                : filter
                    (\(s, p) -> s /= name)
                    (zip prevSaved prevPaths)

        newConfig =
            configData
                { saved = fst <$> combs
                , paths = snd <$> combs
                }

    lift $ saveConfig savePath newConfig
    execute $ Open path

handleGetFavourite :: ConfigIO ()
handleGetFavourite = do
    (conf, savePath) <- gets splitConf
    case saved conf of
        [] -> lift $ putStrLn "No Saved Projects"
        savedNames -> do
            choice <- lift $ getSelection savedNames

            let index = subtract 1 <$> choice
                path = index >>= getMaybe (paths conf)

            case path of
                Nothing -> execute $ Error "Invalid Selection"
                Just p -> do
                    execute $ Open p

handleDeleteFavourite :: ConfigIO ()
handleDeleteFavourite = do
    (conf, savePath) <- gets splitConf
    case saved conf of
        [] -> lift $ putStrLn "No Saved Projects"
        savedNames -> do
            choice <- lift $ getSelection (saved conf)

            let index = subtract 1 <$> choice

            case choice of
                Nothing -> execute $ Error "Invalid Selection"
                Just i -> do
                    let newSaved = drop i (saved conf)
                        newPath = drop i (paths conf)
                        newConfig =
                            conf
                                { saved = newSaved
                                , paths = newPath
                                }

                    lift $ saveConfig savePath newConfig

handleError :: String -> IO ()
handleError msg = putStrLn $ "Error: " ++ msg

getSelection :: [String] -> IO (Maybe Int)
getSelection options = do
    let nums = map (\x -> "[" ++ show x ++ "] ") [1 ..]
        printouts = zipWith (++) nums options
    putStrLn "Recent Projects:"
    mapM_ putStrLn printouts

    putStr "> "
    hFlush stdout
    readMaybe <$> getLine

getRecents :: String -> IO [String]
getRecents dir = do
    let path = dir ++ "recents.txt"

    fileExists <- doesFileExist path
    if fileExists
        then do
            contents <- readFile path
            length contents `seq` return ()
            return $ filter (/= "") $ lines contents
        else return []

saveRecents :: ConfigData -> String -> String -> IO ()
saveRecents config savePath path = do
    prevSaved <- getRecents savePath

    let newSaved = take (numRecent config) $ path : filter (/= path) prevSaved
        filename = savePath ++ "recents.txt"
        contents = unlines newSaved

    writeFile filename contents

getMaybe :: [a] -> Int -> Maybe a
getMaybe lst index = listToMaybe (drop index lst)

pathToOption :: String -> String
pathToOption input = last $ splitOn "/" input

getConfig :: String -> IO (Either ParseException ConfigData)
getConfig dir = do
    let path = dir ++ "/config.yaml"
    input <- fromString <$> readFile path
    return $ decodeEither' input

saveConfig :: Path -> ConfigData -> IO ()
saveConfig dir conf = do
    let contents = toString $ encode conf
        path = dir ++ "/config.yaml"

    writeFile path contents

main :: IO ()
main = do
    args <- getArgs
    configPath <- (++ "/.config/svim/") <$> getHomeDirectory

    maybeConf <- getConfig configPath
    print maybeConf
    case maybeConf of
        Left err -> putStrLn "Could not read config."
        Right confD -> do
            let action = parseArgs args
                defaultConfig = Config{confData = confD, rootDir = configPath}
            runStateT (execute action) defaultConfig
            return ()
