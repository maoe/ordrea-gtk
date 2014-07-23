{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FRP.Ordrea.Gtk
  ( Gtk
  , runGtk, runGtkWith
  , defaultGtkMain
  , liftSGen

  , liftGtk
  , liftGtkAsync

  , performGtk
  , performGtkAsync

  , on
  , after
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer

import FRP.Ordrea
import FRP.Ordrea.IO
import Graphics.UI.Gtk hiding (after, on)
import qualified Graphics.UI.Gtk as Gtk

newtype Gtk a = Gtk { unGtk :: WriterT (Event ()) SignalGen a }
  deriving
    ( Functor, Applicative, Monad, MonadFix
    , MonadIO, MonadWriter (Event ())
    )

runGtk :: Gtk a -> IO ()
runGtk = runGtkWith 10

runGtkWith :: Int -> Gtk a -> IO ()
runGtkWith timeout (Gtk w) = do
  action <- start $ do
    (_, event) <- runWriterT w
    return $ eventToBehavior event
  void $ timeoutAdd (True <$ action) timeout

liftSGen :: SignalGen a -> Gtk a
liftSGen = Gtk . lift

defaultGtkMain :: Gtk a -> IO ()
defaultGtkMain gtk = do
  initGUI
  void . forkIO $ runGtk gtk
  mainGUI

performGtk :: Event (IO a) -> Gtk ()
performGtk = liftSGen . perform postGUISync >=> registerGtk

performGtkAsync :: Event (IO a) -> Gtk ()
performGtkAsync =
  liftSGen . perform (postGUIAsync . void) >=> registerGtk

registerGtk :: Event a -> Gtk ()
registerGtk event = tell $ () <$ event

liftGtk :: MonadIO m => IO a -> m a
liftGtk = liftIO . postGUISync

liftGtkAsync :: MonadIO m => IO a -> m ()
liftGtkAsync = liftIO . postGUIAsync . void

type Sink m a = a -> m ()

-- | Perform an action in response to a GTK signal.
on
  :: MonadIO m
  => object
  -> Signal object (m a)
  -> (Sink m b -> m a)
  -> Gtk (ConnectId object, Event b)
on object signal = connectSignal (Gtk.on object signal)

-- | Perform an action in response to a GTK signal like @on@, but the signal
-- is executed after GTK's default handler has run.
after
  :: MonadIO m
  => object
  -> Signal object (m a)
  -> (Sink m b -> m a)
  -> Gtk (ConnectId object, Event b)
after object signal = connectSignal (Gtk.after object signal)

connectSignal
  :: MonadIO m
  => (m a -> IO (ConnectId object))
  -> (Sink m b -> m a)
  -> Gtk (ConnectId object, Event b)
connectSignal f callback = do
  extern <- liftIO newExternalEvent
  connId <- liftGtk $ f $ callback $ liftIO . triggerExternalEvent extern
  event <- liftSGen $ externalE extern
  return (connId, event)
