{-# LANGUAGE OverloadedStrings #-}
module Reflex.DOM.AusGov.UiKit.SkipLink (skipLinkCss, auSkipLinkC, auSkipLink__LinkC) where

import           Prelude                      hiding (rem)

import           Clay
import           Control.Lens                 (Getter, to, view, (^.))


import           Reflex.DOM.AusGov.UiKit.Class (CssClass, cssClass,
                                                toRefinement, toSelector)
import           Reflex.DOM.AusGov.UiKit.Core (HasColourConfig, UiKitCss,
                                               clayColorG, colourThemeBg,
                                               colourThemeFgText, colourConfigLightTheme,
                                               fontgridMdNospace, fontsNormal,
                                               liftCss, marginUni, paddingUni,
                                               sronly)

auSkipLinkC , auSkipLink__LinkC :: CssClass
auSkipLinkC = cssClass "au-skip-link"
auSkipLink__LinkC = cssClass "au-skip-link__link"

skipLinkCss :: HasColourConfig c => UiKitCss c
skipLinkCss = do
  lT <- view $ colourConfigLightTheme
  liftCss $ toSelector auSkipLinkC ? do
    fontgridMdNospace
    uncurry fontFamily fontsNormal
    color (lT ^. colourThemeFgText . clayColorG)
  liftCss $ toSelector auSkipLink__LinkC ? do
      sronly
      let linkStyle = do
            clip auto
            height auto
            marginUni (px 0)
            overflow visible
            position absolute
            width auto
            color (lT ^. colourThemeBg . clayColorG)
            backgroundColor (lT ^. colourThemeFgText . clayColorG)
            top (rem 1)
            left (rem 1)
            paddingUni (rem 1.5)
      hover & linkStyle
      focus & linkStyle
