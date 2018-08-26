{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Text.Lazy as TL
import Clay as Css
import Reflex.Dom

import Reflex.DOM.AusGov.UiKit (defaultColourConfig, css)

frontend :: (Widget x (), Widget x ())
frontend = (head', body)
  where
    head' = do
      el "title" $ text "hUIKit - Design System Demo"
      el "style" . text . TL.toStrict . Css.render $ css defaultColourConfig
    body = elClass "div" "au-body au-body--dark" $ do
      el "h1" $ text "Testing display of HTML elements"
      el "h2" $ text "This is 2nd level heading"
      el "p" $ text "This is a test paragraph."
      el "h3" $ text "This is 3rd level heading"
      el "p" $ text "This is a test paragraph."
      el "h4" $ text "This is 4th level heading"
      el "p" $ text "This is a test paragraph."
      el "h5" $ text "This is 5th level heading"
      el "p" $ text "This is a test paragraph."
      el "h6" $ text "This is 6th level heading"
      el "p" $ text "This is a test paragraph."

      el "h2" $ text "Basic block level elements"

      el "p" $ do
        text "This is a normal paragraph ("
        el "code" $ text "p"
        text " element). To add some length to it, let us mention that this page was primarily written for testing the effect of "
        el "strong" $ text "user style sheets"
        text "."
      el "p" $ text "You can use it for various other purposes as well, like just checking how your browser displays various HTML elements by default. It can also be useful when testing conversions from HTML format to other formats, since some elements can go wrong then."
      el "p" $ text "This is another paragraph. I think it needs to be added that the set of elements tested is not exhaustive in any sense. I have selected those elements for which it can make sense to write user style sheet rules, in my opinion."
      el "div" $ do
        text $ "This is a "
        el "code" $ text "div"
        text " element. Authors may use such elements instead of paragraph markup for various reasons. (End of "
        el "code" $ text "div"
        text ")."
      el "blockquote" . el "p" $ do
        text "This is a block quotation containing a single paragraph. Well, not quite, since this is not "
        el "em" $ text "really"
        text " quoted text, but I hope you understand the point. After all, this page does not use HTML markup very normally anyway."
      pure ()

main :: IO ()
main = mainWidgetWithHead (fst frontend) (snd frontend)
