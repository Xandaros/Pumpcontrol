module Render where

import Control.Monad (join)
import Control.Monad.Trans (liftIO)
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

import Reflex.Dom

import Types

bodyWidget :: MonadWidget t m => Dynamic t [PostReturn Pump] -> m ()
bodyWidget pumps = do
    sidebarEntries <- (catMaybes . fmap sidebarEntry) `mapDyn` pumps
    sidebarEvs <- sidebar sidebarEntries
    elClass "div" "content" $ content (NE.head <$> sidebarEvs)
    bareJS "$(document).ready(function(){\
        \$(\".button-collapse\").sideNav();\
    \})"

headWidget :: MonadWidget t m => m ()
headWidget = do
    css "static/icons.css"
    css "static/materialize.min.css"
    css "static/style.css"
    where
        css href = elAttr "link" ("rel" =: "stylesheet" <> "href" =: href) $ return ()

content :: MonadWidget t m => Event t Int -> m ()
content events = do
    sidebarToggleBtn
    txt <- holdDyn "None" (show <$> events)
    dynText txt

preloader :: MonadWidget t m => m ()
preloader =
        divClass "preloader-wrapper big active" $ do
            divClass "spinner-layer spinner-blue" spinner
            divClass "spinner-layer spinner-red" spinner
            divClass "spinner-layer spinner-yellow" spinner
            divClass "spinner-layer spinner-green" spinner

    where
        circle, spinner :: MonadWidget t m => m ()
        circle = divClass "circle" $ return ()
        spinner = do
            divClass "circle-clipper left" circle
            divClass "gap-patch" circle
            divClass "circle-clipper right" circle

sidebarToggleBtn :: MonadWidget t m => m ()
sidebarToggleBtn = elAttr "a" ("href" =: "#" <> "data-activates" =: "slide-out" <> "class" =: "button-collapse hide-on-large-only") $
    elClass "i" "material-icons" (text "menu")

sidebar :: MonadWidget t m => Dynamic t [m a] -> m (Event t (NonEmpty a))
sidebar elements = do
    links <- (sequence . fmap sidebarLink) `mapDyn` elements
    event <- fmap mergeList `mapDyn` links
    elAttr "ul" ("id" =: "slide-out" <> "class" =: "side-nav fixed") $ switchPromptly never =<< dyn event
    where
    sidebarLink :: MonadWidget t m => m a -> m (Event t a)
    sidebarLink elem = do
        (l, e) <- el "li" $ customLink elem
        let event = const e <$> _link_clicked l
        pure event

    customLink :: MonadWidget t m => m a -> m (Link t, a)
    customLink inner = first (Link . domEvent Click) <$> element
        where
            element = elAttr' "a" ("class" =: "pointer") inner

sidebarEntry :: MonadWidget t m => PostReturn Pump -> Maybe (m Int)
sidebarEntry pump = (text ((name.datas) pump) *>) <$> (pure <$> ident pump)


bareJS :: MonadWidget t m => String -> m ()
bareJS js = elAttr "script" ("type" =: "text/javascript") $ text js
