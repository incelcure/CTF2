import Application ()
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Casino.User
import Data.String.Interpolate
import Database.Persist.Postgresql
import Foundation
import System.Environment (getEnv)
import Yesod.Core

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

  secret <- getEnv "PROVIDER_SECRET"
  runStderrLoggingT $ withPostgresqlPool connectionString openConnectionCount $ \pool -> liftIO $ do
    _ <- runResourceT $ flip runSqlPool pool $ do
      runMigration migrateAll
      us :: [Entity CasinoUser] <- selectList [] []
      when (null us) $ insert_ $ CasinoUser "test" 100 50
    warp 3031 $ App pool (fromString secret)
