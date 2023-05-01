{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}

import Ar (ArchiveEntry (filename))
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import System.Directory (canonicalizePath, doesFileExist, doesPathExist)
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
import Text.Read (choice, readMaybe)

cPath = "/Users/alexscriba/.config/svim/"

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

defaultConfig :: Config
defaultConfig =
    Config
        { getCommand = "nvim"
        , numRecent = 10
        , saved = []
        , paths = []
        }

parseArgs :: [String] -> Action
parseArgs [] = GetRecent
parseArgs ("--set-editor" : name : _) = SetCommand name
parseArgs ("--set-num-recent" : num : _) = SetNumRecent num
parseArgs ("-s" : name : path : _) = OpenAndSave name path
parseArgs ("-f" : _) = GetFavourite
parseArgs ("-d" : _) = DeleteFavourite
parseArgs (path : _) = Open path

execute :: Action -> IO ()
execute action = case action of
    Open path -> handleOpen cPath path
    GetRecent -> handleGetRecent cPath
    OpenAndSave name path -> handleOpenAndSave cPath name path
    Error msg -> handleError msg

handleGetRecent :: String -> IO ()
handleGetRecent configPath = do
    prev <- getRecents configPath

    case prev of
        [] -> putStrLn "No Recent Projects"
        prevPaths -> do
            choice <- getSelection $ pathToOption <$> prev

            let index = subtract 1 <$> choice
                pathChoice = index >>= getMaybe prev

            case pathChoice of
                Just path -> execute $ Open path
                Nothing -> execute $ Error "Invalid Selection"

handleOpen :: String -> String -> IO ()
handleOpen configPath path = do
    absolutePath <- canonicalizePath path

    let command = "cd " ++ path ++ " && " ++ getCommand defaultConfig
    exitCode <- system command

    case exitCode of
        ExitSuccess -> saveRecents cPath absolutePath
        ExitFailure code -> putStrLn $ "Code: " ++ show code

handleOpenAndSave :: String -> String -> String -> IO ()
handleOpenAndSave configPath name path = putStrLn $ name ++ " " ++ path

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

saveRecents :: String -> String -> IO ()
saveRecents savePath path = do
    prevSaved <- getRecents savePath

    let newSaved = take (numRecent defaultConfig) $ path : filter (/= path) prevSaved
        filename = savePath ++ "recents.txt"
        contents = unlines newSaved

    writeFile filename contents

getMaybe :: [a] -> Int -> Maybe a
getMaybe lst index = listToMaybe (drop index lst)

pathToOption :: String -> String
pathToOption input = last $ splitOn "/" input

main :: IO ()
main = do
    args <- getArgs

    let action = parseArgs args

    execute action
