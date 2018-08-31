module Reflex.DOM.AusGov.UiKit.Class (CssClass, cssClass, toRefinement) where

import           Clay      (Selector, byClass)
import           Data.Text (Text)

newtype CssClass = CssClass Text

cssClass :: Text -> CssClass
cssClass = CssClass

toSelector :: CssClass -> Selector
toSelector (CssClass t) = byClass t
