module Main where

import Prelude

import CSS as CSS
import CSS.Cursor as CSS.Cursor
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Example.Sankey (init)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

data Action = Initialize

component ∷ ∀ query input output m. MonadEffect m ⇒ H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  initialState _ = unit

  render :: ∀ mon slot s. MonadEffect mon => s -> H.ComponentHTML Action slot mon
  render _ =
    HH.div
      [ HP.id "sample"
      , HCSS.style do
          CSS.width (CSS.pct 100.0)
          CSS.display CSS.flex
          CSS.justifyContent CSS.spaceBetween
      ]
      [
        -- This below div is for rendering of the diagram.
        HH.div
          [ HP.id "myDiagramDiv"
          , HCSS.style do
              CSS.flexGrow 1.0
              CSS.height (CSS.px 1000.0)
              CSS.backgroundColor (CSS.white)
              CSS.position CSS.relative
              -- CSS.Webkit.tapHighlightColor (CSS.rgba 255 255 255 0)
              CSS.cursor CSS.Cursor.default
          ]
          []
      ]
  handleAction = case _ of
    Initialize -> do
      liftEffect init

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

