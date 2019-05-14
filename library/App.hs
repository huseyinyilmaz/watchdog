module App (main) where
import Env (AppT(..),
            AppConfig(..),
            Options(..),
            HasAppConfig(..)
           )
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad(void, forever)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newMVar, tryTakeMVar, putMVar)
import Control.Monad.Reader(ask)
-- import qualified Data.Text as T
import System.FSNotify
import System.Process(readProcess)
import Options.Applicative


_watchDir :: (Show t, HasAppConfig t) => t -> IO() -> IO ()
_watchDir config action= do
  lock <- newMVar ()
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    void $ watchTree
      mgr          -- manager
      "."
      -- ((T.unpack . source) project)          -- directory to watch

      (const True) -- predicate
      (\_-> void $ forkIO $ do
          took <- tryTakeMVar lock
          case took of
            Just () -> do
              putStrLn "Change detected.."
              action -- action
              putStrLn $ show config
              threadDelay (1 * 1000 * 1000)
              putStrLn "Command completed"
              putMVar lock ()
            _ -> putStrLn "Already running. Skipping the update."
      )
    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000

app :: AppT IO ()
app = do
  config <- ask
  liftIO $ _watchDir config $ putStrLn "running"

parseStringList :: Monad m => String -> m [String]
parseStringList = return . words

multiStringArgument :: Mod ArgumentFields [String] -> Parser [String]
multiStringArgument desc = concat <$> some single
  where single = argument (str >>= parseStringList) desc

optionsParser :: Parser Options
optionsParser = do
  Options
    <$> (switch $ long "verbose")
    <*> (multiStringArgument $ (metavar "command"))

parseCLI :: IO Options
parseCLI = execParser (withInfo optionsParser "Watchdog!!")
  where
    withInfo opts h = info (helper <*> opts) $ (header h <> noIntersperse <> forwardOptions)

main :: IO ()
main = do
  options <- parseCLI
  let config = AppConfig options
  result <- runExceptT $ runReaderT (unAppT app) config
  case result of
    Left e -> putStrLn ("Error!!!!!XX" <> (show e))
    Right () -> putStrLn "DONE!!!"
  -- where
  --   config = AppConfig $ Options True "test"
