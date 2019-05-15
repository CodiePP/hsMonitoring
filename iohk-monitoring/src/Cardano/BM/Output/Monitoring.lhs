
\subsection{Cardano.BM.Output.Monitoring}
\label{module:Cardano.BM.Output.Monitoring}



%if style == newcode
\begin{code}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Cardano.BM.Output.Monitoring
    (
      Monitor
    , effectuate
    , realizefrom
    , unrealize
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar,
                     modifyMVar_, readMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import qualified Data.HashMap.Strict as HM
import           Data.Text (pack)
import qualified Data.Text.IO as TIO
import           Data.Time.Calendar (toModifiedJulianDay)
import           Data.Time.Clock (UTCTime (..), getCurrentTime)
import           GHC.Clock (getMonotonicTimeNSec)
import           System.IO (stderr)

import           Cardano.BM.Configuration.Model (Configuration, getMonitors)
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MessageCounter (resetCounters, sendAndResetAfter,
                     updateMessageCounters)
import           Cardano.BM.Data.MonitoringEval
import           Cardano.BM.Data.Severity (Severity (..))
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

\subsubsection{Structure of Monitoring}\label{code:Monitor}\index{Monitor}
\begin{code}
type MonitorMVar a = MVar (MonitorInternal a)
newtype Monitor a = Monitor
    { getMon :: MonitorMVar a }

data MonitorInternal a = MonitorInternal
    { monQueue   :: TBQ.TBQueue (Maybe (LogObject a))
    }

\end{code}

\subsubsection{Relation from context name to monitoring state}\label{code:MonitorMap}\label{code:MonitorState}
We remember the state of each monitored context name.
\begin{code}
data MonitorState = MonitorState {
      _expression  :: MEvExpr
    , _actions     :: [MEvAction]
    , _environment :: Environment
    } deriving Show
type MonitorMap = HM.HashMap LoggerName MonitorState

\end{code}

\subsubsection{Monitor view is an effectuator}\index{Monitor!instance of IsEffectuator}
Function |effectuate| is called to pass in a |LogObject| for monitoring.
\begin{code}
instance IsEffectuator Monitor a where
    effectuate monitor item = do
        putStrLn "Monitor___effectuate.."
        mon <- readMVar (getMon monitor)
        nocapacity <- atomically $ TBQ.isFullTBQueue (monQueue mon)
        if nocapacity
        then handleOverflow monitor
        else atomically $ TBQ.writeTBQueue (monQueue mon) $ Just item

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: Monitor's queue full, dropping log items!\n"

\end{code}

\subsubsection{|Monitor| implements |Backend| functions}\index{Monitor!instance of IsBackend}

|Monitor| is an |IsBackend|
\begin{code}
instance IsBackend Monitor a where
    typeof _ = MonitoringBK

    realize _ = error "Monitoring cannot be instantiated by 'realize'"

    realizefrom config sbtrace _ = do
        putStrLn "Monitor___realizefrom.."
        monref <- newEmptyMVar
        let monitor = Monitor monref
        queue <- atomically $ TBQ.newTBQueue 512
        dispatcher <- spawnDispatcher queue config sbtrace
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        putMVar monref $ MonitorInternal
                        { monQueue = queue
                        -- , monState = mempty
                        }
        return monitor

    unrealize _ = return ()

\end{code}

\subsubsection{Asynchrouniously reading log items from the queue and their processing}
\begin{code}
spawnDispatcher :: TBQ.TBQueue (Maybe (LogObject a))
                -> Configuration
                -> Trace.Trace IO a
                -> IO (Async.Async ())
spawnDispatcher mqueue config sbtrace = do
    putStrLn "Monitor__spawnDispatcher..0"
    now <- getCurrentTime
    putStrLn "Monitor__spawnDispatcher..1"
    let messageCounters = resetCounters now
    putStrLn "Monitor__spawnDispatcher..2"
    countersMVar <- newMVar messageCounters
    putStrLn "Monitor__spawnDispatcher..3"
    _timer <- Async.async $ sendAndResetAfter
                                sbtrace
                                "#messagecounters.monitoring"
                                countersMVar
                                60000   -- 60000 ms = 1 min
                                Warning -- Debug
    putStrLn "Monitor__spawnDispatcher..4"
    Async.async (initMap >>= qProc countersMVar)
  where
    qProc counters state = do
        putStrLn "Monitor__qProc..0"
        maybeItem <- atomically $ TBQ.readTBQueue mqueue
        putStrLn "Monitor__qProc..1"
        case maybeItem of
            Just (logvalue@(LogObject _ _ _)) -> do
                state' <- evalMonitoringAction state logvalue
                -- increase the counter for the type of message
                modifyMVar_ counters $ \cnt -> return $ updateMessageCounters cnt logvalue
                qProc counters state'
            Nothing -> return ()  -- stop here
    initMap = do
        putStrLn "Monitor__initMap..0"
        ls <- getMonitors config
        let res = HM.fromList $ map (\(n, (e,as)) -> (n, MonitorState e as HM.empty)) $ HM.toList ls
        print res
        putStrLn "Monitor__initMap..1"
        return res
\end{code}

\subsubsection{Evaluation of monitoring action}\label{code:evalMonitoringAction}
Inspect the log message and match it against configured thresholds. If positive,
then run the action on the current state and return the updated state.
\begin{code}

{-
evalMonitoringAction begin..
MonitorMap: fromList
                [
                    (
                        "complex.monitoring",
                        MonitorState {
                            _expression = (mean >= (59)),
                            _actions = ["AlterMinSeverity \"complex.monitoring\" Debug"],
                            _environment = fromList []
                        }
                    )
                ]
LogName: complex.monitoring
YEP, starting...
expr: (mean >= (59))
acts: ["AlterMinSeverity \"complex.monitoring\" Debug"]
env0: fromList []
env': fromList [("monitMe",78.47320342980716),("timestamp",63357312390289 ns)]
Nope, didn't evaluate, exit...





Compare "mean" (GE, 59),



evaluate :: Environment -> MEvExpr -> Bool
evaluate  fromList [
             ("monitMe",78.47320342980716),
             ("timestamp",63357312390289 ns)
          ]
         expr =
    case expr of
        -- Compare "mean" (GE, 59),
           Compare vn     (op, m2) -> case (HM.lookup vn ev) of
                                          Nothing -> False
                                          Just m1 -> (fromOperator op) m1 m2


evalMonitoringAction begin..
MonitorMap: fromList [("complex.monitoring",MonitorState {_expression = (monitMe >= (59)), _actions = ["AlterMinSeverity \"complex.monitoring\" Debug"], _environment = fromList []})]
LogName: complex.monitoring
YEP, starting...
expr: (monitMe >= (59))
acts: ["AlterMinSeverity \"complex.monitoring\" Debug"]
env0: fromList []
env': fromList [("monitMe",66.53025739757501),("timestamp",73715294875565 ns)]
OK, it's evaluated, continue...
now: 310890038392221
env'': fromList [
            ("lastalert", 310890038392221 ns),
            ("monitMe",   66.53025739757501),
            ("timestamp", 73715294875565 ns)
       ]
alert! complex.monitoring ["AlterMinSeverity \"complex.monitoring\" Debug"] fromList [("lastalert",310890038392221 ns),("monitMe",66.53025739757501),("timestamp",73715294875565 ns)]
M






YEP, starting...
expr: (monitMe >= (59))
acts: ["AlterMinSeverity \"complex.monitoring\" Debug"]
env0: fromList []
env': fromList [("monitMe",83.69744448948583),("timestamp",74867659005875 ns)]
OK, it's evaluated, continue...
now: 312042403561678
env'': fromList [("lastalert",312042403561678 ns),("monitMe",83.69744448948583),("timestamp",74867659005875 ns)]
alert! complex.monitoring ["AlterMinSeverity \"complex.monitoring\" Debug"] fromList [("lastalert",312042403561678 ns),("monitMe",83.69744448948583),("timestamp",74867659005875 ns)]

fromList [
    (
        "complex.monitoring",
        MonitorState {
            _expression = (monitMe >= (59)),
            _actions = ["AlterMinSeverity \"complex.monitoring\" Debug"],
            _environment = fromList [
                                (
                                    "lastalert",312042403561678 ns
                                ),
                                (
                                    "monitMe",83.69744448948583
                                ),
                                (
                                    "timestamp",74867659005875 ns
                                )
                           ]
        }
    )
]





-}

evalMonitoringAction :: MonitorMap -> LogObject a -> IO MonitorMap
evalMonitoringAction mmap logObj@(LogObject logname _ _) = do
    putStrLn "evalMonitoringAction begin.."
    putStrLn $ "MonitorMap: " <> show mmap
    TIO.putStrLn $ "LogName: " <> logname
    case HM.lookup logname mmap of
        Nothing -> do
            putStrLn "NOPE, return the same mmap..."
            return mmap
        Just mon@(MonitorState expr acts env0) -> do
            putStrLn "YEP, starting..."
            putStrLn $ "expr: " <> show expr
            putStrLn $ "acts: " <> show acts 
            putStrLn $ "env0: " <> show env0
            let env' = updateEnv env0 logObj
            putStrLn $ "env': " <> show env'
            if evaluate env' expr
            then do
                putStrLn "OK, it's evaluated, continue..."
                now <- getMonotonicTimeNSec
                putStrLn $ "now: " <> show now
                let env'' = HM.insert "lastalert" (Nanoseconds now) env'
                putStrLn $ "env'': " <> show env''
                TIO.putStrLn $ "alert! " <> logname <> " " <> (pack $ show acts) <> " " <> (pack $ show env'')
                let finaRes = HM.insert logname mon{_environment=env''} mmap
                print finaRes
                putStrLn "``````````````end``````````````"
                return finaRes
            else do
                putStrLn "Nope, didn't evaluate, exit..."
                return mmap
  where
    utc2ns (UTCTime days secs) =
        let yearsecs :: Rational
            yearsecs = 365 * 24 * 3600
            rdays,rsecs :: Rational
            rdays = toRational $ toModifiedJulianDay days
            rsecs = toRational secs
            s2ns = 1000000000
        in
        Nanoseconds $ round $ (fromRational $ s2ns * rsecs + rdays * yearsecs :: Double)
    updateEnv env (LogObject _ _ (ObserveOpen _)) = env
    updateEnv env (LogObject _ _ (ObserveDiff _)) = env
    updateEnv env (LogObject _ _ (ObserveClose _)) = env
    updateEnv env (LogObject _ lometa (LogValue vn val)) =
        let addenv = HM.fromList [ (vn, val)
                                 , ("timestamp", utc2ns (tstamp lometa))
                                 ]
        in
        HM.union addenv env
    updateEnv env (LogObject _ lometa (LogMessage _logitem)) =
        let addenv = HM.fromList [ ("severity", (Severity (severity lometa)))
                                --  , ("selection", (liSelection logitem))
                                --  , ("message", (liPayload logitem))
                                 , ("timestamp", utc2ns (tstamp lometa))
                                 ]
        in
        HM.union addenv env
    updateEnv env (LogObject _ lometa (AggregatedMessage vals)) =
        let addenv = ("timestamp", utc2ns (tstamp lometa)) : aggs2measurables vals []
        in
        HM.union (HM.fromList addenv) env
      where
        aggs2measurables [] acc = acc
        aggs2measurables ((n, AggregatedEWMA ewma):r) acc = aggs2measurables r $ (n <> ".avg", avg ewma) : acc
        aggs2measurables ((n, AggregatedStats s):r) acc = aggs2measurables r $
              (n <> ".mean", PureD . meanOfStats $ fbasic s)
            : (n <> ".flast", flast s)
            : (n <> ".fcount", PureI . fromIntegral . fcount $ fbasic s)
            : acc
    -- catch all
    updateEnv env _ = env
\end{code}
