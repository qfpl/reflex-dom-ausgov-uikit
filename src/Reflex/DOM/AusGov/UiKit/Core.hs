{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Reflex.DOM.AusGov.UiKit.Core where

import           Prelude              hiding (rem)

import           Clay                 hiding (id)
import           Clay.Stylesheet      (StyleM)
import           Control.Lens         (Getter, to, (^.))
import           Control.Lens.TH      (makeClassy)
import           Control.Monad.Reader (ReaderT, lift)
import           Data.Colour.SRGB     (Colour, RGB (RGB), sRGB24read, toSRGB24)
import           Data.Text            (Text)

class Monad m => HasCss m where
  liftCss :: StyleM a -> m a

instance HasCss StyleM where
  liftCss = id

instance HasCss m => HasCss (ReaderT c m) where
  liftCss = lift . liftCss

type UiKitStyleM c = ReaderT c StyleM
type UiKitCss c    = UiKitStyleM c ()

data ColourTheme = ColourTheme
  { _colourThemeFgAction     :: Colour Double
  , _colourThemeFgFocus      :: Colour Double
  , _colourThemeFgText       :: Colour Double
  , _colourThemeBg           :: Colour Double
  }
makeClassy ''ColourTheme

data ColourConfig = ColourConfig
  { _colourConfigLightTheme   :: ColourTheme
  , _colourConfigDarkTheme    :: ColourTheme
  , _colourConfigError        :: Colour Double
  , _colourConfigSuccess      :: Colour Double
  , _colourConfigWarning      :: Colour Double
  , _colourConfigInfo         :: Colour Double
  }
makeClassy ''ColourConfig


defaultColourConfig :: ColourConfig
defaultColourConfig = ColourConfig
  { _colourConfigLightTheme = ColourTheme
    { _colourThemeFgAction  = sRGB24read "00698F"
    , _colourThemeFgFocus   = sRGB24read "9263DE"
    , _colourThemeFgText    = sRGB24read "313131"
    , _colourThemeBg        = sRGB24read "FFFFFF"
    }
  , _colourConfigDarkTheme = ColourTheme
    { _colourThemeFgAction = sRGB24read "61DAFF"
    , _colourThemeFgFocus  = sRGB24read "C390F9"
    , _colourThemeFgText   = sRGB24read "FFFFFF"
    , _colourThemeBg       = sRGB24read "135E70"
    }
  , _colourConfigError   = sRGB24read "FF635C"
  , _colourConfigSuccess = sRGB24read "0CAC78"
  , _colourConfigWarning = sRGB24read "F69900"
  , _colourConfigInfo    = sRGB24read "00BFE9"
  }

clayColorG :: (RealFrac a, Floating a) => Getter (Colour a) Color
clayColorG = to toClayColor

toClayColor :: (RealFrac a, Floating a) => Colour a -> Color
toClayColor c = rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)
  where
    (RGB r g b) = toSRGB24 c

marginUni :: Size a -> Css
marginUni s = margin s s s s

paddingUni :: Size a -> Css
paddingUni s = padding s s s s

borderNone :: Css
borderNone = borderWidth (px 0)

fontgrid :: Size LengthUnit -> Size LengthUnit -> Css
fontgrid fs lh = fontSize fs *> lineHeight lh

sronly :: Css
sronly = do
  position absolute
  width (px 1)
  height (px 1)
  paddingUni (px 0)
  marginUni (px (-1))
  overflow hidden
  clip (rect (px 0) (px 0) (px 0) (px 0))
  borderNone

-- TODO :: Clay doesn't export GenericFontFamily type. Why?
fontsNormal = (
  ["-apple-system","BlinkMacSystemFont","Segoe UI","Roboto","Oxygen","Ubuntu","Cantarell","Fira Sans","Droid Sans","Helvetica Neue"] :: [Text],
  [sansSerif]
  )
fontsMonospace = (
  ["Lucida Sans Typewriter","Lucida Console","Monaco","Bitstream Vera Sans Mono"] :: [Text],
  [monospace]
  )

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
