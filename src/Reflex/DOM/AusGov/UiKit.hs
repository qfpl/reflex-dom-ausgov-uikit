{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Reflex.DOM.AusGov.UiKit
  ( defaultColourConfig
  , css
  ) where

import           Prelude

import           Clay                             (Css)
import           Control.Monad.Reader             (runReaderT)

import           Reflex.DOM.AusGov.UiKit.Body     (bodyCss)
import           Reflex.DOM.AusGov.UiKit.Core     (ColourConfig, defaultColourConfig)
import           Reflex.DOM.AusGov.UiKit.SkipLink (skipLinkCss)

-- TODO: Print CSS
-- TODO: ReaderT Config

css :: ColourConfig -> Css
css c = flip runReaderT c . sequence_ $ [bodyCss, skipLinkCss]
