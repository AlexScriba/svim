import Config (Config (..), ConfigData (..), getConfig)
import Control.Monad.State (StateT (runStateT))
import Core (execute, parseArgs)
import System.Directory (getHomeDirectory)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
    args <- getArgs
    configPath <- (++ "/.config/svim/") <$> getHomeDirectory

    maybeConf <- getConfig configPath
    case maybeConf of
        Left err -> putStrLn "Could not read config."
        Right confD -> do
            let action = parseArgs args
                defaultConfig = (Config{_confData = confD, _rootDir = configPath})
            runStateT (execute action) defaultConfig
            return ()
