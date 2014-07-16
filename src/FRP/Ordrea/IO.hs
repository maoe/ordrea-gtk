module FRP.Ordrea.IO
  ( perform
  , performIO
  , performIOAsync
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

performIO :: Event (IO a) -> SignalGen (Event a)
performIO = perform id

performIOAsync :: Event (IO a) -> SignalGen (Event a)
performIOAsync ioE = do
  extE <- liftIO newExternalEvent
  chan <- liftIO newChan
  liftIO $ void . forkIO $
    join (readChan chan) >>= triggerExternalEvent extE
  writeE <- performIO $ writeChan chan <$> ioE
  resultE <- externalE extE
  return $ justE $ mconcat
    [ Nothing <$ writeE
    , Just <$> resultE
    ]
