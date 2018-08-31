{-# LANGUAGE OverloadedStrings #-}
module Reflex.DOM.AusGov.UiKit.Body where

import           Prelude                       hiding (rem)

import           Clay

import           Reflex.DOM.AusGov.UiKit.Class (CssClass, cssClass,
                                                toRefinement)
import           Reflex.DOM.AusGov.UiKit.Core  (ColourConfig, toClayColor,
                                                colourConfigFgText, fontsNormal)


auBodyC , auBodyCDark :: CssClass
auBodyC = cssClass "au-body"
auBodyCDark = cssClass "au-body--dark"

bodyCss :: ColourConfig -> Css
bodyCss c = do
  toRefinement auBodyC ? do
    backgroundColor (c ^. colourConfigBg . clayColorG)
    color (c ^. colourConfigFgText . clayColorG)
    fontgridSm
    uncurry fontFamily fontsNormal
    "font-size-adjust" -: "100%"
    marginUni (px 0)

    toRefinement auBodyCDark & do
      backgroundColor (c ^. colourConfigDarkBg . to clayColor)
      color (c ^. colourConfigDarkFgText . to toClayColor)
