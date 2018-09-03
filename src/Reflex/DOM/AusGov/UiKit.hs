{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Reflex.DOM.AusGov.UiKit
  ( defaultColourConfig
  , css
  ) where

import           Prelude                      hiding (rem)

import           Clay
import           Control.Lens                 (to, (^.))
import           Control.Lens.TH              (makeLenses)
import           Data.Colour.SRGB             (Colour, RGB (RGB), sRGB24read,
                                               toSRGB24)
import           Data.Semigroup               ((<>))

import           Reflex.DOM.AusGov.UiKit.Body (bodyCss)
import           Reflex.DOM.AusGov.UiKit.Core (ColourConfig, clayColorG,
                                               colourConfigBg,
                                               colourConfigFgText,
                                               defaultColourConfig,
                                               fontgridMdNospace, fontsNormal,
                                               marginUni, paddingUni, sronly)

-- TODO: Print CSS
-- TODO: ReaderT Config

css :: ColourConfig -> Css
css c = do
  bodyCss c
  ".au-skip-link" ? do
    fontgridMdNospace
    uncurry fontFamily fontsNormal
    color (c ^. colourConfigFgText . clayColorG)
  ".au-skip-link__link" ? do
      sronly
      let linkStyle = do
            clip auto
            height auto
            marginUni (px 0)
            overflow visible
            position absolute
            width auto
            color  (c ^. colourConfigBg . clayColorG)
            backgroundColor  (c ^. colourConfigFgText . clayColorG)
            top (rem 1)
            left (rem 1)
            paddingUni (rem 1.5)
      hover & linkStyle
      focus & linkStyle
