
\label{code:Control.Tracer.Observe}

%if style == newcode
\begin{code}
{-|
Module: Control.Tracer.Observe

Functions useful for observing and measuring actions.
-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Tracer.Observe
    (
    -- * observing
      ObserveIndicator (..)
    , Observable (..)
    -- * example
    , example
    ) where

import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)

import           Control.Tracer (Tracer (..), showTracing, stdoutTracer, traceWith)

\end{code}
%endif

\subsection{Examples}
Observe the duration of an action using the timedBracketObserve:

\begin{code}
data AddSub a = Add a
              | Sub a
              deriving Show

type Time = Word64

example :: IO ()
example = do
    let -- a Tracer handling the observations
        trObserve :: Tracer IO (Observable Time)
        trObserve = showTracing stdoutTracer
        -- a transformer which enriches observations with time measurement
        transform :: Tracer IO (Observable Time) -> Tracer IO ObserveIndicator
        transform trace = Tracer $ \observeIndicator -> do
            now <- getMonotonicTimeNSec
            traceWith trace $ Obs observeIndicator now
        trObserve' = transform trObserve

    -- observe add
    traceWith trObserve' ObserveBefore
    _ <- actionAdd tr
    traceWith trObserve' ObserveAfter

    -- observe sub
    traceWith trObserve' ObserveBefore
    _ <- actionSub tr
    traceWith trObserve' ObserveAfter

  where
    tr :: Tracer IO (AddSub Int)
    tr = showTracing stdoutTracer
    actionAdd :: Tracer IO (AddSub Int) -> IO Int
    actionAdd trace = do
        let res = 1+2
        traceWith trace $ Add res
        return res
    actionSub :: Tracer IO (AddSub Int) -> IO Int
    actionSub trace = do
        let res = 1-2
        traceWith trace $ Sub res
        return res

instance Show (Observable Time) where
  show (Obs ind time) = show ind ++ " " ++ show time

\end{code}

\subsection{Observe}
\subsubsection{ObserveIndicator}\label{code:ObserveIndicator}\index{ObserveIndicator}
Data structure that indicates the beginning and the end of an observation.
\begin{code}
data ObserveIndicator = ObserveBefore | ObserveAfter
                      deriving Show

\end{code}

\subsubsection{Observable}\label{code:Observable}\index{Observable}
Data structure which holds the observation along with the indicator
of the observation.
\begin{code}
data Observable t = Obs ObserveIndicator t

\end{code}
