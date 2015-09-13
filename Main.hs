{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

{- Backend for retrieving trace data from the database. -}
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Monoid (mconcat)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Time
import Network.Wai.Middleware.RequestLogger
import Web.Scotty
import qualified Data.Text as Text

import Data.Aeson.Types (ToJSON, (.=), object, toJSON)

myConnectInfo :: ConnectInfo
myConnectInfo = defaultConnectInfo {
    connectPassword = "bohCh0mu"
    }

main = do

    conn <- connect myConnectInfo

    scotty 3000 $ do
        middleware logStdoutDev
        post "/traces" $ do
            traceListAction conn

{-
 - ## TRACEINFO SECTION ##
 -}
data TraceInfo = TraceInfo {
    id :: Int,
    filename :: String,
    date :: String
    } deriving (Show)

instance ToJSON TraceInfo where
    toJSON (TraceInfo id fn date) =
        object [
            "id"       .= id,
            "filename" .= fn,
            "date"     .= date
        ]
--Scotty Action
traceListAction :: Connection -> ActionM()
traceListAction conn = do
    traceList <- liftIO $ fmap
        (map (\(id,filename,date) ->
            TraceInfo id (Text.unpack filename) (show date))) $ getTraceList conn
    json $ traceList

--returns a list of traces, so that the user can pick which one to examine.
getTraceList :: Connection -> IO [(Int, Text.Text, LocalTimestamp)]
getTraceList conn = query_ conn [sql|SELECT trace_id,filename,creation_date FROM TRACES|]

{-
 - ## MACHINEEVENT SECTION
 -}
