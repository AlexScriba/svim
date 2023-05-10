{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Core where

import Control.Monad.State (StateT (runStateT), get, gets, lift, put, runStateT)
import Data.ByteString.UTF8 (fromString, toString)
import Data.List.Split (split, splitOn)
import Data.Maybe (listToMaybe)
import Data.Yaml (FromJSON, ParseException, ToJSON, decodeEither', encode, object, parseJSON, toJSON, withObject, (.!=), (.:))
import qualified Data.Yaml as Yml ((.=))
import Data.Yaml.Pretty (encodePretty)
import Lens.Micro (set)
import Lens.Micro.Mtl (use, (.=))
import Lens.Micro.TH (makeLenses)
import Menu (PathOption (..), getMenuSelection)
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
        { _getCommand :: String
        , _numRecent :: Int
        , _saved :: [String]
        , _paths :: [String]
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

splitConf :: Config -> (ConfigData, Path)
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
        return $ ConfigData command numR s p

instance ToJSON ConfigData where
    toJSON config =
        object
            [ "command" Yml..= _getCommand config
            , "num_recent" Yml..= _numRecent config
            , "saved" Yml..= _saved config
            , "paths" Yml..= _paths config
            ]

data Action where
    SetCommand :: String -> Action
    SetNumRecent :: String -> Action
    Open :: Path -> Action
    OpenAndSave :: String -> Path -> Action
    GetRecent :: Action
    GetFavourite :: Action
    DeleteFavourite :: Action
    Terminate :: Action
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
    Terminate -> return ()
    Error msg -> io $ handleError msg

setConfigCommand :: String -> ConfigIO ()
setConfigCommand cmd = do
    (confData . getCommand) .= cmd
    config <- use confData
    confPath <- use rootDir

    io $ saveConfig confPath config

setNumRecent :: String -> ConfigIO ()
setNumRecent num =
    case (readMaybe num :: Maybe Int) of
        Nothing -> execute $ Error "Invalid number"
        Just n -> do
            (confData . numRecent) .= n
            config <- use confData
            confPath <- use rootDir
            io $ saveConfig confPath config

handleGetRecent :: ConfigIO ()
handleGetRecent = do
    savePath <- use rootDir
    prevPaths <- io $ getRecents savePath

    case prevPaths of
        [] -> io $ putStrLn "No Recent Projects"
        paths -> do
            let names = pathToName <$> paths
            choice <- io $ getMenuSelection $ zipWith PathOption names paths

            case choice of
                Just path -> execute $ Open path
                Nothing -> execute Terminate

handleOpen :: String -> ConfigIO ()
handleOpen path = do
    absolutePath <- io $ canonicalizePath path

    cmd <- use $ confData . getCommand
    config <- use confData
    savePath <- use rootDir

    let command = "cd " ++ path ++ " && " ++ cmd
    exitCode <- io $ system command

    case exitCode of
        ExitSuccess -> saveRecents absolutePath
        ExitFailure code -> io $ putStrLn $ "Code: " ++ show code

handleOpenAndSave :: String -> String -> ConfigIO ()
handleOpenAndSave name path = do
    absPath <- io $ canonicalizePath path

    prevSaved <- use $ confData . saved
    prevPaths <- use $ confData . paths

    let combs =
            (name, absPath)
                : filter
                    (\(s, p) -> s /= name)
                    (zip prevSaved prevPaths)

    (confData . saved) .= map fst combs
    (confData . paths) .= map snd combs

    savePath <- use rootDir
    config <- use confData

    io $ saveConfig savePath config
    execute $ Open path

handleGetFavourite :: ConfigIO ()
handleGetFavourite = do
    savedNames <- use $ confData . saved
    savedPaths <- use $ confData . paths

    case savedNames of
        [] -> io $ putStrLn "No Saved Projects"
        names -> do
            choice <-
                io $
                    getMenuSelection $
                        zipWith PathOption savedNames savedPaths

            case choice of
                Nothing -> execute Terminate
                Just p -> execute $ Open p

handleDeleteFavourite :: ConfigIO ()
handleDeleteFavourite = do
    savedNames <- use $ confData . saved
    savedPaths <- use $ confData . paths

    case savedNames of
        [] -> io $ putStrLn "No Saved Projects"
        names -> do
            choice <- io $ getSelection names

            case subtract 1 <$> choice of
                Nothing -> execute $ Error "Invalid Selection"
                Just i -> do
                    (confData . saved) .= drop i names
                    (confData . paths) .= drop i savedPaths

                    savePath <- use rootDir
                    config <- use confData

                    io $ saveConfig savePath config

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

saveRecents :: String -> ConfigIO ()
saveRecents path = do
    savePath <- use rootDir
    numRec <- use $ confData . numRecent

    prevSaved <- io $ getRecents savePath

    let newSaved = take numRec $ path : filter (/= path) prevSaved
        filename = savePath ++ "recents.txt"
        contents = unlines newSaved

    io $ writeFile filename contents

getMaybe :: [a] -> Int -> Maybe a
getMaybe lst index = listToMaybe (drop index lst)

pathToName :: FilePath -> String
pathToName input = last $ splitOn "/" input

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