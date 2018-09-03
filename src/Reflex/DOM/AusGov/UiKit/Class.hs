module Reflex.DOM.AusGov.UiKit.Class (CssClass, cssClass, toRefinement, toSelector) where

import           Clay          (Refinement, Selector, byClass, star)
import           Clay.Selector (with)
import           Data.Text     (Text)

newtype CssClass = CssClass Text

cssClass :: Text -> CssClass
cssClass = CssClass

toSelector :: CssClass -> Selector
toSelector = with star . toRefinement

toRefinement :: CssClass -> Refinement
toRefinement (CssClass t) = byClass t
