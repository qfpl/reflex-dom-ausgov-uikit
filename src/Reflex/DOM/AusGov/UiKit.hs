{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Reflex.DOM.AusGov.UiKit
  ( defaultColourConfig
  , css
  ) where

import Prelude hiding (rem)

import Clay
import Control.Lens ((^.), to)
import Control.Lens.TH (makeLenses)
import Data.Colour.SRGB (Colour, RGB(RGB), toSRGB24, sRGB24read)

-- TODO

data ColourConfig = ColourConfig
  { _colourConfigFgAction     :: Colour Double
  , _colourConfigFgFocus      :: Colour Double
  , _colourConfigFgText       :: Colour Double
  , _colourConfigBg           :: Colour Double
  , _colourConfigDarkFgAction :: Colour Double
  , _colourConfigDarkFgFocus  :: Colour Double
  , _colourConfigDarkFgText   :: Colour Double
  , _colourConfigDarkBg       :: Colour Double
  , _colourConfigError        :: Colour Double
  , _colourConfigSuccess      :: Colour Double
  , _colourConfigWarning      :: Colour Double
  , _colourConfigInfo         :: Colour Double
  }
makeLenses ''ColourConfig

defaultColourConfig :: ColourConfig
defaultColourConfig = ColourConfig
  { _colourConfigFgAction     = sRGB24read "00698F"
  , _colourConfigFgFocus      = sRGB24read "9263DE"
  , _colourConfigFgText       = sRGB24read "313131"
  , _colourConfigBg           = sRGB24read "FFFFFF"
  , _colourConfigDarkFgAction = sRGB24read "61DAFF"
  , _colourConfigDarkFgFocus  = sRGB24read "C390F9"
  , _colourConfigDarkFgText   = sRGB24read "FFFFFF"
  , _colourConfigDarkBg       = sRGB24read "135E70"
  , _colourConfigError        = sRGB24read "FF635C"
  , _colourConfigSuccess      = sRGB24read "0CAC78"
  , _colourConfigWarning      = sRGB24read "F69900"
  , _colourConfigInfo         = sRGB24read "00BFE9"
  }

clayColor :: (RealFrac a, Floating a) => Colour a -> Color
clayColor c = rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)
  where
    (RGB r g b) = toSRGB24 c

css :: ColourConfig -> Css
css c = do
  ".au-body" ? do
    backgroundColor (c ^. colourConfigBg . to clayColor)
    color (c ^. colourConfigFgText . to clayColor)
    fontgridSm
    uncurry fontFamily fontsNormal
  ".au-body" # ".au-body--dark" ? do
    backgroundColor (c ^. colourConfigDarkBg . to clayColor)
    color (c ^. colourConfigDarkFgText . to clayColor)

fontgrid :: Size LengthUnit -> Size LengthUnit -> Css
fontgrid fs lh = fontSize fs *> lineHeight lh

fontsNormal = (["-apple-system","BlinkMacSystemFont","Segoe UI","Roboto","Oxygen","Ubuntu","Cantarell","Fira Sans","Droid Sans","Helvetica Neue"],[sansSerif])
fontsMonospace = (["Lucida Sans Typewriter","Lucida Console","Monaco","Bitstream Vera Sans Mono"],[monospace])

mediaXs = (px 576)
mediaSm = (px 768)
mediaMd = (px 992)
mediaLg = (px 1200)

maxWidth = (em 42)

spacingUnit = 4

auBorderRadius = (px spacingUnit)

-- The AU space is unnecessary here because reflex doesn't work in browsers
-- that don't have rem anyway. :)
-- https://caniuse.com/#feat=rem
xxxlRem :: Size LengthUnit
xxxlRem = rem 3
xxlRem :: Size LengthUnit
xxlRem  = rem 2.5
xlRem :: Size LengthUnit
xlRem   = rem 2
lgRem :: Size LengthUnit
lgRem   = rem 1.5
mdRem :: Size LengthUnit
mdRem   = rem 1.25
smRem :: Size LengthUnit
smRem   = rem 1
xsRem :: Size LengthUnit
xsRem   = rem 0.87

fontgridXxxl :: Css
fontgridXxxl = fontgrid xxxlRem (rem 1.5)

fontgridXxl :: Css
fontgridXxl = fontgrid xxlRem (rem 1.5)

fontgridXl :: Css
fontgridXl = fontgrid xlRem (rem 1.5)

fontgridLg :: Css
fontgridLg = fontgrid lgRem (rem 1.5)

fontgridMd :: Css
fontgridMd = fontgrid mdRem (rem 1.6)

fontgridSm :: Css
fontgridSm = fontgrid smRem (rem 1.5)

fontgridXs :: Css
fontgridXs = fontgrid xsRem (rem 1.4)

fontgridXxxlHeading :: Css
fontgridXxxlHeading = fontgrid xxxlRem (rem 1.25)

fontgridXxlHeading :: Css
fontgridXxlHeading = fontgrid xxlRem (rem 1.3)

fontgridXlHeading :: Css
fontgridXlHeading = fontgrid xlRem (rem 1.25)

fontgridLgHeading :: Css
fontgridLgHeading = fontgrid lgRem (rem 1.33)

fontgridMdHeading :: Css
fontgridMdHeading = fontgrid mdRem (rem 1.2)

fontgridSmHeading :: Css
fontgridSmHeading = fontgrid smRem (rem 1.25)

fontgridXsHeading :: Css
fontgridXsHeading = fontgrid xsRem (rem 1.14)

fontgridXxxlNospace :: Css
fontgridXxxlNospace = fontgrid xxxlRem (rem 1)

fontgridXxlNospace :: Css
fontgridXxlNospace = fontgrid xxlRem (rem 1)

fontgridXlNospace :: Css
fontgridXlNospace = fontgrid xlRem (rem 1)

fontgridLgNospace :: Css
fontgridLgNospace = fontgrid lgRem (rem 1)

fontgridMdNospace :: Css
fontgridMdNospace = fontgrid mdRem (rem 1)

fontgridSmNospace :: Css
fontgridSmNospace = fontgrid smRem (rem 1)

fontgridXsNospace :: Css
fontgridXsNospace = fontgrid xsRem (rem 1.14)
