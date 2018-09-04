{-# LANGUAGE OverloadedStrings #-}
module Reflex.DOM.AusGov.UiKit.Body (bodyCss, auBodyC, auBodyCDark) where

import           Prelude                       hiding (rem)

import           Clay
import           Control.Lens                  (Getter, to, view, (^.))

import           Reflex.DOM.AusGov.UiKit.Class (CssClass, cssClass,
                                                toRefinement, toSelector)
import           Reflex.DOM.AusGov.UiKit.Core  (ColourTheme, HasColourConfig,
                                                UiKitCss, clayColorG,
                                                colourConfigDarkTheme,
                                                colourConfigLightTheme,
                                                colourThemeBg,
                                                colourThemeFgAction,
                                                colourThemeFgText, fontgridSm,
                                                fontsNormal, liftCss, marginUni)

-- TODO: Print Styles

auBodyC , auBodyCDark :: CssClass
auBodyC = cssClass "au-body"
auBodyCDark = cssClass "au-body--dark"

bodyCss :: HasColourConfig c => UiKitCss c
bodyCss = do
  lT  <- view $ colourConfigLightTheme
  dT  <- view $ colourConfigDarkTheme
  liftCss $ toSelector auBodyC ? do
    bodyColours lT
    fontgridSm
    uncurry fontFamily fontsNormal
    "font-size-adjust" -: "100%"
    marginUni (px 0)

    "a" & do
      color (lT ^. colourThemeFgAction . clayColorG)

    toRefinement auBodyCDark & do
      bodyColours dT

bodyColours :: ColourTheme -> Css
bodyColours t = do
  color (t ^. colourThemeFgText . clayColorG)
  backgroundColor (t ^. colourThemeBg . clayColorG)
