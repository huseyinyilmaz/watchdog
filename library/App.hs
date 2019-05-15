module App (main) where
import Env (AppT(..),
            AppConfig(..),
            Options(..),
            HasAppConfig(..),
            getCommand
           )
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad(void, forever)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newMVar, tryTakeMVar, putMVar)
import Control.Monad.Reader(ask)
import Control.Lens(view)
-- import qualified Data.Text as T
import System.FSNotify
import System.Process(readProcessWithExitCode)
import System.Exit(ExitCode(..))
import Options.Applicative


_watchDir :: (Show t, HasAppConfig t) => t -> IO() -> IO ()
_watchDir config cmd= do
  putStrLn "Running watchdog with following config:"
  putStrLn $ show config
  cmd -- Run command the first time.
  lock <- newMVar ()
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    void $ watchTree
      mgr          -- manager
      "."          -- directory to watch
      (const True) -- predicate
      (\_-> void $ forkIO $ do
          took <- tryTakeMVar lock
          case took of
            Just () -> do
              putStrLn "Change detected.."
              cmd -- action
              threadDelay (1 * 1000 * 1000)
              putStrLn "Command completed"
              putMVar lock ()
            _ -> putStrLn "Already running. Skipping the update."
      )
    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000

executeCommand :: AppConfig -> IO ()
executeCommand config = do
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode cmd args []
  putStrLn "===================================================="
  case exitCode of
    ExitSuccess -> do
      putStr stdOut
      putStr stdErr
    ExitFailure code -> do
      putStrLn "Runtime Error"
      putStrLn ("Command Exited with exit code " <> show code)
      putStrLn "------------------------------------"
      putStrLn "Standard Out:"
      putStr stdOut
      putStrLn "------------------------------------"
      putStrLn "Standard Error:"
      putStr stdErr
      putStrLn "------------------------------------"
  putStrLn "===================================================="
  where commands = (view getCommand (config::AppConfig))
        cmd = head commands
        args = tail commands


app :: AppT IO ()
app = do
  config <- ask
  liftIO $ _watchDir config $ executeCommand config

parseStringList :: Monad m => String -> m [String]
parseStringList = return . words

multiStringArgument :: Mod ArgumentFields [String] -> Parser [String]
multiStringArgument desc = concat <$> some single
  where single = argument (str >>= parseStringList) desc

optionsParser :: Parser Options
optionsParser = do
  Options
    <$> (multiStringArgument $ (metavar "command"))

parseCLI :: IO Options
parseCLI = execParser (withInfo optionsParser )
  where
    headerMod = header "Execute a command on file system changes in current directory."
    footerMod = footer "Example: $ watchdog echo There was a change on current directory"
    mods = headerMod <> footerMod <> noIntersperse <> forwardOptions
    withInfo opts = info (helper <*> opts) $ mods


main :: IO ()
main = do
  options <- parseCLI
  let config = AppConfig options
  result <- runExceptT $ runReaderT (unAppT app) config
  case result of
    Left e -> putStrLn ("Error!!!" <> (show e))
    Right () -> putStrLn "DONE!!!"
