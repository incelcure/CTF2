import Application ()
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Casino.User
import Data.String.Interpolate
import Database.Persist.Postgresql
import Foundation
import System.Environment (getEnv)
import Yesod.Core
import Yesod.Static

main :: IO ()
main = do
  let openConnectionCount = 5
  connectionString <- do
    host <- fromMaybe "localhost" <$> lookupEnv "POSTGRES_HOST"
    port <- fromMaybe "5432" <$> lookupEnv "POSTGRES_PORT"
    user <- fromMaybe "ctf" <$> lookupEnv "POSTGRES_USER"
    db <- fromMaybe "ctf" <$> lookupEnv "POSTGRES_DB"
    password <- getEnv "POSTGRES_PASSWORD"
    return
      [i|host=#{host} port=#{port} user=#{user} dbname=#{db} password=#{password}|]

  staticDir <- fromMaybe "./static" <$> lookupEnv "STATIC_PATH"
  d <- maybe False (`elem` ["1", "true", "t"]) <$> lookupEnv "DEBUG"
  when d $ putStrLn "DEBUG"
  putStrLn staticDir
  stat <- if d then staticDevel staticDir else static staticDir

  secr <- getEnv "PROVIDER_SECRET"
  runStderrLoggingT $ withPostgresqlPool connectionString openConnectionCount $ \p -> liftIO $ do
    _ <- runResourceT $ flip runSqlPool p $ do
      runMigration migrateAll
      us :: [Entity CasinoUser] <- selectList [] []
      when (null us) $ insert_ $ CasinoUser "test" 100 50 []
    warp 3031 $ App p (fromString secr) stat
