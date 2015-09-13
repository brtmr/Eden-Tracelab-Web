{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- Backend for retrieving trace data from the database. -}
import Database.PostgreSQL.Simple.Time
import Data.Monoid (mconcat)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Network.Wai.Middleware.RequestLogger
import Control.Monad
import Web.Scotty

myConnectInfo :: ConnectInfo
myConnectInfo = defaultConnectInfo {
    connectPassword = "bohCh0mu"
    }

main = do

    conn <- connect myConnectInfo

    scotty 3000 $ do
        middleware logStdoutDev
        post "/traces" $ do
            json $ [("file1" :: String), "file2", "file3" ]

getTraceList :: Connection -> IO()
getTraceList conn = do
    xs <- query_ conn "SELECT filename,creation_date FROM TRACES"
    forM_ xs $ \(name, date) -> do
        print $ Text.unpack (name :: Text.Text)
        print $ (date :: LocalTimestamp)
