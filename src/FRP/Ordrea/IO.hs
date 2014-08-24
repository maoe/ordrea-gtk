module FRP.Ordrea.IO
  ( perform
  , performSync
  , performAsync
  ) where
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Trans
import Data.Monoid

import FRP.Ordrea

perform :: (a -> IO b) -> Event a -> SignalGen (Event b)
perform f event = generatorE $ liftIO . f <$> event

performSync :: Event (IO a) -> SignalGen (Event a)
performSync = perform id

performAsync :: Event (IO a) -> SignalGen (Event a)
performAsync ioE = do
  extE <- liftIO newExternalEvent
  chan <- liftIO newChan
  liftIO $ void . forkIO $
    join (readChan chan) >>= triggerExternalEvent extE
  writeE <- performSync $ writeChan chan <$> ioE
  resultE <- externalE extE
  return $ justE $ mconcat
    [ Nothing <$ writeE
    , Just <$> resultE
    ]
