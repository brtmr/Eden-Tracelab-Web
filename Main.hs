{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

{- Backend for retrieving trace data from the database. -}
import Bachelor.Types
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.List
import Data.Monoid (mconcat)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Time
import Network.Wai.Middleware.RequestLogger
import Web.Scotty
import qualified Data.Text as Text

import Data.Aeson.Types (ToJSON, (.=), object, toJSON)

type TraceId   = Int
type ThreadId  = Int
type MachineId = Int
type ProcessId = Int

myConnectInfo :: ConnectInfo
myConnectInfo = defaultConnectInfo {
    connectPassword = "bohCh0mu"
    }

main = do
    conn <- connect myConnectInfo
    scotty 3000 $ do
        middleware logStdoutDev
        --for serving the view
        --index
        get "/" $ do
            file "./view/index.html"
        --javscript stuff
        get "/js/edentv.js" $ do
            file "./view/js/edentv.js"
        get "/js/d3.min.js" $ do
            file "./view/js/d3.min.js"
        get "/js/jquery-1.11.3.min.js" $ do
            file "./view/js/jquery-1.11.3.min.js"
        get "/js/data.js" $ do
            file "./view/js/data.js"
        --css
        get "/css/style.css" $ do
            file "./view/css/style.css"
        --favicons
        get "/img/favicon-96x96.png" $ do
            addHeader "Content-type" "image/png"
            file "./view/img/favicon-96x96.png"
        get "/img/favicon-196x196.png" $ do
            addHeader "Content-type" "image/png"
            file "./view/img/favicon-196x196.png"
        get "/img/favicon-16x16.png" $ do
            addHeader "Content-type" "image/png"
            file "./view/img/favicon-16x16.png"
        get "/img/favicon-32x32.png" $ do
            addHeader "Content-type" "image/png"
            file "./view/img/favicon-32x32.png"

        --JSON API.
        post "/traces" $ do
            traceListAction conn
        post "/mevents" $ do
            id       <- param "id"
            start    <- param "start"
            end      <- param "end"
            minduration <- param "minduration"
            evs <- liftIO $ getMachineEvents conn id start end minduration
            json $ evs
        post "/traceinfo" $ do
            id <- param "id"
            traceInfo <- liftIO $ getTraceInfo conn id
            json $ traceInfo
        post "/duration" $ do
            id <- param "id"
            dur <- liftIO $ getTraceDuration conn id
            liftIO $ putStrLn $ show dur
            json $ [dur]

{-
 - TRACEDURATION SECTION.
 - -}

getTraceDuration :: Connection -> Int -> IO Int
getTraceDuration conn trace_id = do
    dur <- query conn [sql|SELECT MAX(STARTTIME+DURATION) FROM (MACHINE_EVENTS JOIN MACHINES
        ON MACHINE_EVENTS.MACHINE_ID = MACHINES.MACHINE_ID)
        WHERE MACHINES.MACHINE_ID IN
        (SELECT MACHINE_ID FROM MACHINES WHERE
        TRACE_ID = ?)|] $ Only trace_id
    return $ (fromOnly.head) dur

{-
 - ## TRACEINFO SECTION ##
 -}
data TraceEntry = TraceEntry {
    id :: Int,
    filename :: String,
    date :: String
    } deriving (Show)

instance ToJSON TraceEntry where
    toJSON (TraceEntry id fn date) =
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
            TraceEntry id (Text.unpack filename) (show date))) $ getTraceList conn
    json $ traceList

--returns a list of traces, so that the user can pick which one to examine.
getTraceList :: Connection -> IO [(Int, Text.Text, LocalTimestamp)]
getTraceList conn = query_ conn [sql|SELECT trace_id,filename,creation_date FROM TRACES|]

{-
 - ## EVENT RETRIEVAL SECTION
 -}
getMachineEvents :: Connection -> Int -> Int -> Int -> Int ->
    IO [(Int,Int,Int,Int)]
getMachineEvents conn trace_id start end minduration = do
    evs <- query conn [sql|SELECT NUM, STARTTIME, DURATION, STATE FROM (MACHINE_EVENTS JOIN MACHINES
    ON MACHINE_EVENTS.MACHINE_ID = MACHINES.MACHINE_ID)
    WHERE STARTTIME > ?
    AND   DURATION  > ?
    AND   STARTTIME < ?
    AND   MACHINES.MACHINE_ID in
    (SELECT MACHINE_ID FROM MACHINES WHERE
    TRACE_ID = ?)|] (start, minduration, end, trace_id)
    return evs

{-
 - EVENT INFO SECTION
 - returns the entire trace structure as a tree, without any events.
 -}
type TraceInfo = [(MachineId,[(ProcessId,[ThreadId])])]

getTraceInfo :: Connection -> TraceId -> IO TraceInfo
getTraceInfo conn traceId = do
    res <- query conn [sql|SELECT P.MACHINE_NUM, P.PROCESS_NUM, NUM as THREAD_NUM FROM
        THREADS
        JOIN (SELECT M.MACHINE_NUM, PROCESS_ID, NUM AS PROCESS_NUM from
            PROCESSES
            JOIN (SELECT MACHINES.MACHINE_ID, MACHINES.NUM as MACHINE_NUM FROM
                (SELECT * FROM TRACES WHERE TRACE_ID = ?) T
                JOIN MACHINES
                ON T.TRACE_ID = MACHINES.TRACE_ID) M
            ON M.MACHINE_ID = PROCESSES.MACHINE_ID) P
        ON P.PROCESS_ID = THREADS.PROCESS_ID|] $ Only traceId
    return $ mkTraceInfo res

--takes a flattened trace structure, and returns a hierarchical one.
mkTraceInfo :: [(MachineId, ProcessId, ThreadId)] -> TraceInfo
mkTraceInfo xs =
    map (\mid -> (mid,map (\(m,p,ts) -> (p,ts))
        $ filter (\(m,p,ts) -> m==mid) threads)) $ firstone xs
    where
        threads = map (\(m,p) -> (m,p,(map (\(_,_,x) -> x) $
            filter (\(m',p',_) -> (m',p') == (m,p)) xs))) $ firsttwo xs

firsttwo :: Eq a => [(a,a,a)] -> [(a,a)]
firsttwo xs = nub $ map (\(a,b,c) -> (a,b)) xs

firstone :: Eq a => [(a,a,a)] -> [a]
firstone xs = nub $ map (\(a,b,c) -> a) xs
