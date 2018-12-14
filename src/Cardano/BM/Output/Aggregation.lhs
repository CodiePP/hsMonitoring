
\subsection{Cardano.BM.Output.Aggregation}
\label{module:Cardano.BM.Output.Aggregation}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.Aggregation
    ( setup
    , pass
    , takedown
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar,
                     putMVar, readMVar, tryTakeMVar, withMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad (void)
import           Control.Monad.Catch (throwM)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import           Cardano.BM.Aggregated (Aggregated (..), updateAggregation)
import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data.Counter (Counter (..), CounterState (..), nameCounter)
import           Cardano.BM.Data.LogItem

\end{code}
%endif

The aggregation is a singleton.
\begin{code}
type AggregationMVar = MVar AggregationInternal
newtype Aggregation = Aggregation
    { getAg :: AggregationMVar }

-- Our internal state
data AggregationInternal = AggregationInternal
    { agQueue    :: TBQ.TBQueue (Maybe NamedLogItem)
    , agDispatch :: Async.Async ()
    }

\end{code}

\todo[inline]{TODO inspect function to get the Aggregated of a specific name
maybe
inspect :: TBQ.Queue -> Text -> IO ()
inspect q agg name = send to queue a special message asking for the map
}

\begin{code}
setup :: Configuration -> TBQ.TBQueue (Maybe NamedLogItem) -> IO Aggregation
setup _ switchboardQueue = do
    aggref <- newEmptyMVar
    aggregationQueue <- atomically $ TBQ.newTBQueue 2048
    dispatcher <- spawnDispatcher HM.empty aggregationQueue switchboardQueue
    putMVar aggref $ AggregationInternal aggregationQueue dispatcher
    return $ Aggregation aggref

\end{code}

Pass the item to the Aggregation queue.
\begin{code}
pass :: Aggregation -> NamedLogItem -> IO ()
pass agg item = do
    ag <- readMVar (getAg agg) -- this will block. would it be better if the Queue was setup in the Switchboard
    -- so as to have it here as an argument?
    let aggregationQueue = agQueue ag
    atomically $ TBQ.writeTBQueue aggregationQueue $ Just item

spawnDispatcher :: HM.HashMap Text Aggregated
                -> TBQ.TBQueue (Maybe NamedLogItem)
                -> TBQ.TBQueue (Maybe NamedLogItem)
                -> IO (Async.Async ())
spawnDispatcher aggMap aggregationQueue switchboardQueue = Async.async $ qProc aggMap
  where
    qProc aggregatedMap = do
        maybeItem <- atomically $ TBQ.readTBQueue aggregationQueue
        case maybeItem of
            Just item -> do
                let (updatedMap, msgs) =
                        update (lnItem item) (lnName item) aggregatedMap
                -- send Aggregated messages back to Switchboard
                sendAggregated msgs switchboardQueue (lnName item)
                qProc updatedMap
            -- if Nothing is in the queue then every backend is terminated
            -- and Aggregation stops.
            Nothing -> return () -- maybe check the Queue for messages came after Nothing

    update :: LogObject
           -> LoggerName
           -> HM.HashMap Text Aggregated
           -> (HM.HashMap Text Aggregated, [LogObject])
    update (LP (LogValue iname value)) logname agmap =
        let name = logname <> "." <> iname
            maybeAggregated = updateAggregation value $ HM.lookup name agmap
            aggregatedMessage = case maybeAggregated of
                                    Nothing ->
                                        []
                                    Just aggregated ->
                                        [AggregatedMessage iname aggregated]
        in
        -- use of HM.alter so that in future we can clear the Agrregated
        -- by using as alter's arg a function which returns Nothing.
        (HM.alter (const $ maybeAggregated) name agmap, aggregatedMessage)
    update (ObserveDiff counterState) logname agmap =
        let
            counters = csCounters counterState
            (mapNew, msgs) = updateCounter counters logname agmap []
        in
            (mapNew, reverse msgs)
    -- TODO for text messages aggregate on delta of timestamps
    update _ _ agmap = (agmap, [])

    updateCounter :: [Counter]
                  -> LoggerName
                  -> HM.HashMap Text Aggregated
                  -> [LogObject]
                  -> (HM.HashMap Text Aggregated, [LogObject])
    updateCounter [] _ aggrMap msgs = (aggrMap, msgs)
    updateCounter (counter : cs) logname aggrMap msgs =
        let
            name = cName counter
            fullname = logname <> "." <> name
            maybeAggregated = updateAggregation (cValue counter) $ HM.lookup fullname aggrMap
            aggregatedMessage = case maybeAggregated of
                                    Nothing ->
                                        error "This should not have happened!"
                                    Just aggregated ->
                                        AggregatedMessage ((nameCounter counter) <> "." <> name) aggregated
            updatedMap = HM.alter (const $ maybeAggregated) fullname aggrMap
        in
            updateCounter cs logname updatedMap (aggregatedMessage :msgs)

    sendAggregated :: [LogObject] -> TBQ.TBQueue (Maybe NamedLogItem) -> Text -> IO ()
    sendAggregated [] _ _ = return ()
    sendAggregated (aggregatedMsg@(AggregatedMessage _ _) : ms) sbQueue logname = do
        -- forward the aggregated message to Switchboard
        atomically $ TBQ.writeTBQueue sbQueue $
            Just $ LogNamed
                        { lnName = logname <> ".aggregated"
                        , lnItem = aggregatedMsg
                        }
        sendAggregated ms sbQueue logname
    -- ingnore messages that are not of type AggregatedMessage
    sendAggregated (_ : ms) sbQueue logname =
        sendAggregated ms sbQueue logname


\end{code}

\begin{code}
takedown :: Aggregation -> IO ()
takedown aggregation = do
    (dispatcher, queue) <- withMVar (getAg aggregation) (\ag ->
                           return (agDispatch ag, agQueue ag))
    -- send terminating item to the queue
    atomically $ TBQ.writeTBQueue queue Nothing
    -- wait for the dispatcher to exit
    res <- Async.waitCatch dispatcher
    either throwM return res
    (clearMVar . getAg) aggregation

clearMVar :: MVar a -> IO ()
clearMVar = void . tryTakeMVar

\end{code}
