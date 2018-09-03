{-# LANGUAGE OverloadedStrings #-}
module Reflex.DOM.AusGov.UiKit.Body where

import           Prelude                       hiding (rem)

import           Clay
import           Control.Lens                  (Getter, to, (^.))

import           Reflex.DOM.AusGov.UiKit.Class (CssClass, cssClass, toSelector, toRefinement)
import           Reflex.DOM.AusGov.UiKit.Core  (ColourConfig, clayColorG,
                                                colourConfigBg,
                                                colourConfigDarkBg,
                                                colourConfigDarkFgText,
                                                colourConfigFgText, fontgridSm,
                                                fontsNormal, marginUni)

auBodyC , auBodyCDark :: CssClass
auBodyC = cssClass "au-body"
auBodyCDark = cssClass "au-body--dark"

bodyCss :: ColourConfig -> Css
bodyCss c = do
  toSelector auBodyC ? do
    backgroundColor (c ^. colourConfigBg . clayColorG)
    color (c ^. colourConfigFgText . clayColorG)
    fontgridSm
    uncurry fontFamily fontsNormal
    "font-size-adjust" -: "100%"
    marginUni (px 0)

    toRefinement auBodyCDark & do
      backgroundColor (c ^. colourConfigDarkBg . clayColorG)
      color (c ^. colourConfigDarkFgText . clayColorG)
