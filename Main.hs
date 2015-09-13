{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

{- Backend for retrieving trace data from the database. -}
import Bachelor.Types
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
        post "/events" $ do
            id       <- param "id"
            t        <- param "type"
            start    <- param "start"
            end      <- param "end"
            minduration <- param "minduration"
            getEvents conn id t start end minduration

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
 - ## EVENT RETRIEVAL SECTION
 -}
getEvents :: Connection -> Int -> String -> Int -> Int -> Int -> ActionM()
getEvents conn id t start end minduration = do
    text $ "works."

getMachineEvents :: Connection -> Int -> Int -> Int -> Int ->
    IO [(Int,Int,Int,Int)]
getMachineEvents conn trace_id start end minduration = do
    evs <- query conn [sql|SELECT * FROM MACHINE_EVENTS
        WHERE STARTTIME > ?
        AND   DURATION  > ?
        and   STARTTIME < ?
        AND   MACHINE_ID in
        (SELECT MACHINE_ID FROM MACHINES WHERE
        TRACE_ID = ?)|] (start, minduration, end, trace_id)
    return evs
