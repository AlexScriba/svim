{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

import Data.Bifunctor (bimap)
import Data.ByteString.UTF8 (fromString, toString)
import Data.List.Split (splitOn)
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

cPath :: IO String
cPath = (++ "/.config/svim/") <$> getHomeDirectory

type ConfigPath = String

type Path = String

data Config where
    Config ::
        { getCommand :: String
        , numRecent :: Int
        , saved :: [String]
        , paths :: [String]
        } ->
        Config

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        command <- o .: "command"
        numR <- o .: "num_recent"
        s <- o .: "saved" .!= []
        p <- o .: "paths" .!= []
        return $ Config command numR s p

instance ToJSON Config where
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

execute :: Config -> Path -> Action -> IO ()
execute config configPath action = case action of
    SetCommand cmd -> setConfigCommand config configPath cmd
    SetNumRecent num -> setNumRecent config configPath num
    Open path -> handleOpen config configPath path
    OpenAndSave name path -> handleOpenAndSave config configPath name path
    GetFavourite -> handleGetFavourite config configPath
    DeleteFavourite -> handleDeleteFavourite config configPath
    GetRecent -> handleGetRecent config configPath
    Error msg -> handleError msg

setConfigCommand :: Config -> Path -> String -> IO ()
setConfigCommand conf confPath cmd = do
    let newConf = conf{getCommand = cmd}

    saveConfig confPath newConf

setNumRecent :: Config -> Path -> String -> IO ()
setNumRecent conf confPath num = case (readMaybe num :: Maybe Int) of
    Nothing -> execute conf confPath $ Error "Invalid number"
    Just n ->
        let newConf = conf{numRecent = n}
         in saveConfig confPath newConf

handleGetRecent :: Config -> String -> IO ()
handleGetRecent config savePath = do
    prev <- getRecents savePath

    case prev of
        [] -> putStrLn "No Recent Projects"
        prevPaths -> do
            choice <- getSelection $ pathToOption <$> prev

            let index = subtract 1 <$> choice
                pathChoice = index >>= getMaybe prev

            case pathChoice of
                Just path -> execute config savePath $ Open path
                Nothing -> execute config savePath $ Error "Invalid Selection"

handleOpen :: Config -> String -> String -> IO ()
handleOpen config savePath path = do
    absolutePath <- canonicalizePath path

    let command = "cd " ++ path ++ " && " ++ getCommand config
    exitCode <- system command

    case exitCode of
        ExitSuccess -> saveRecents config savePath absolutePath
        ExitFailure code -> putStrLn $ "Code: " ++ show code

handleOpenAndSave :: Config -> String -> String -> String -> IO ()
handleOpenAndSave config savePath name path = do
    absPath <- canonicalizePath path

    let prevSaved = saved config
        prevPaths = paths config

        combs =
            (name, absPath)
                : filter
                    (\(s, p) -> s /= name)
                    (zip prevSaved prevPaths)

        newConfig =
            config
                { saved = fst <$> combs
                , paths = snd <$> combs
                }

    saveConfig savePath newConfig
    execute newConfig savePath $ Open path

handleGetFavourite :: Config -> Path -> IO ()
handleGetFavourite conf savePath = do
    case saved conf of
        [] -> putStrLn "No Saved Projects"
        savedNames -> do
            choice <- getSelection savedNames

            let index = subtract 1 <$> choice
                path = index >>= getMaybe (paths conf)

            case path of
                Nothing -> execute conf savePath $ Error "Invalid Selection"
                Just p -> do
                    execute conf savePath $ Open p

handleDeleteFavourite :: Config -> Path -> IO ()
handleDeleteFavourite conf savePath = do
    case saved conf of
        [] -> putStrLn "No Saved Projects"
        savedNames -> do
            choice <- getSelection (saved conf)

            let index = subtract 1 <$> choice

            case choice of
                Nothing -> execute conf savePath $ Error "Invalid Selection"
                Just i -> do
                    let newSaved = drop i (saved conf)
                        newPath = drop i (paths conf)
                        newConfig =
                            conf
                                { saved = newSaved
                                , paths = newPath
                                }

                    saveConfig savePath newConfig

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

saveRecents :: Config -> String -> String -> IO ()
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

getConfig :: String -> IO (Either ParseException Config)
getConfig dir = do
    let path = dir ++ "/config.yaml"
    input <- fromString <$> readFile path
    return $ decodeEither' input

saveConfig :: Path -> Config -> IO ()
saveConfig dir conf = do
    let contents = toString $ encode conf
        path = dir ++ "/config.yaml"

    writeFile path contents

main :: IO ()
main = do
    args <- getArgs
    configPath <- cPath

    config <- getConfig configPath
    case config of
        Left err -> putStrLn "Could not read config."
        Right conf -> do
            let action = parseArgs args
            execute conf configPath action
