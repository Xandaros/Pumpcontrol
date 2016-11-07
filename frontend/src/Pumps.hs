module Pumps where

import Control.Monad (join)
import Control.Monad.Trans (liftIO)
import Data.Maybe (isJust, fromJust)

import Control.Error.Safe
import Reflex.Dom

import Types

loadPumps :: MonadWidget t m => m (Event t (Maybe [PostReturn Pump]))
loadPumps = do
    ev' <- getPostBuild
    webview <- askWebView
    host <- liftIO $ getLocationHost webview
    ev  <- performRequestAsyncWithError (const (request host) <$> ev')
    return $ join . fmap decodeXhrResponse . rightMay  <$> ev
    where
        request host = xhrRequest "GET" ("http://" ++ host ++ "/pumps") def

pumps :: MonadWidget t m => m (Event t (m (Dynamic t [PostReturn Pump])))
pumps = do
    initPs <- loadPumps
    let events = catMaybesE initPs
    dynEv events

catMaybesE :: Reflex t => Event t (Maybe a) -> Event t a
catMaybesE = (fromJust <$>) . ffilter isJust

dynEv :: MonadWidget t m => Event t a -> m (Event t (m (Dynamic t a)))
dynEv events = do
    firstEvent <- headE events
    pure $ flip holdDyn events <$> firstEvent
