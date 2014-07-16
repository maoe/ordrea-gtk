module FRP.Ordrea.Gtk
  ( liftGtk
  , liftGtkAsync

  , performGtk
  , performGtkAsync

  , runGUI
  , defaultMain

  , on

  , module FRP.Ordrea
  , module GtkExport
  ) where
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans

import FRP.Ordrea
import FRP.Ordrea.IO
import Graphics.UI.Gtk as GtkExport hiding (Signal, on)
import qualified Graphics.UI.Gtk as Gtk

performGtk :: Event (IO a) -> SignalGen (Event a)
performGtk = perform postGUISync

performGtkAsync :: Event (IO ()) -> SignalGen (Event ())
performGtkAsync = perform postGUIAsync

liftGtk :: MonadIO m => IO a -> m a
liftGtk = liftIO . postGUISync

liftGtkAsync :: MonadIO m => IO () -> m ()
liftGtkAsync = liftIO . postGUIAsync

runGUI :: SignalGen (Event ()) -> IO ()
runGUI gtkGen = do
  gtkAction <- start $ eventToSignal <$> gtkGen
  void $ timeoutAdd (True <$ gtkAction) 10

defaultMain :: SignalGen (Event ()) -> IO ()
defaultMain app = do
  initGUI
  void . forkIO $ runGUI app
  mainGUI

on
  :: MonadIO m
  => object
  -> Gtk.Signal object (m a)
  -> m a
  -> SignalGen (ConnectId object, Event a)
on object signal callback = do
  extE <- liftIO newExternalEvent
  connId <- liftGtk $ Gtk.on object signal $ do
      a <- callback
      liftIO $ triggerExternalEvent extE a
      return a
  event <- externalE extE
  return (connId, event)
