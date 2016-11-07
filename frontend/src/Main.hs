{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (void, join)
import Reflex.Dom

import Render
import Pumps

main :: IO ()
main = do
    let widget = join $ widgetHold preloadScreen <$> body
    mainWidgetWithHead headWidget (void widget)

body :: MonadWidget t m => m (Event t (m ()))
body = do
    ps <- pumps
    let widget = join . fmap bodyWidget <$> ps
    pure widget

preloadScreen :: MonadWidget t m => m ()
preloadScreen = do
    divClass "valign-wrapper full-height" $
        divClass "center-block valign" preloader
