{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Core where

import Config
import Data.List.Split (split, splitOn)
import Data.Maybe (listToMaybe)
import Data.Yaml.Pretty (encodePretty)
import Lens.Micro (set)
import Lens.Micro.Mtl (use, (.=))
import Menu (PathOption (..), getMenuSelection)
import System.Directory (
    canonicalizePath,
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

data Action where
    SetCommand :: String -> Action
    SetNumRecent :: String -> Action
    Open :: FilePath -> Action
    OpenAndSave :: String -> FilePath -> Action
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
    recents <- use $ confData . recent

    case recents of
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

    let command = "cd " ++ path ++ " && " ++ cmd
    exitCode <- io $ system command

    case exitCode of
        ExitSuccess -> do
            recents <- use $ confData . recent
            numRecent <- use $ confData . numRecent

            let newRecents = take numRecent $ absolutePath : recents

            (confData . recent) .= newRecents

            conf <- use confData
            savePath <- use rootDir

            io $ saveConfig savePath conf
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

getMaybe :: [a] -> Int -> Maybe a
getMaybe lst index = listToMaybe (drop index lst)

pathToName :: FilePath -> String
pathToName input = last $ splitOn "/" input
