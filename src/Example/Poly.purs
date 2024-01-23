module Example.Poly where

import Prelude hiding (top, bottom)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import GoJS.Diagram.Types (Diagram_)
import GoJS.GraphObject.Types (Link_)
import GoJS.Key (KeyProperty(..))
import GoJS.Unsafe.Set (setUnsafe)
import Went.Diagram.Make (MakeDiagram, addGroupTemplate, addLinkTemplate, addNodeTemplate)
import Went.Diagram.Make as Diagram
import Went.Geometry (Margin(..), Size(..), Spot(..), bottom, center, top)
import Went.GraphObject (Auto', Curve(..), MadeGraphObject, SegmentOrientation(..), Spot', Table', Vertical', group, link, node, panel, placeholder, shape, textBlock)
import Went.GraphObject.Shape.Arrowhead (Arrowhead(Feather))
import Went.GraphObject.Shape.Figure (Figure(..))
import Went.GraphObject.Shape.Figure as Figure
import Went.Layout (Arrangement(..), treeLayout)
import Went.Model (binding, graphLinksModel)
import Went.Settable (set)

type NodeData =
  ( key :: String
  , group :: String
  , isGroup :: Boolean
  , color :: String
  , category :: String
  , input :: Boolean
  )

type LinkData =
  (from :: String, to :: String)

p :: Array (Record NodeData)
p =
  [ { key: "p", group: "", isGroup: true, color: "lightgreen", category: "", input: false }
  , { key: "A", group: "p", isGroup: false, color: "darkgreen", category: "", input: false }
  , { key: "a1", group: "p", isGroup: false, color: "darkgreen", category: "Invisible", input: true }
  , { key: "a2", group: "p", isGroup: false, color: "darkgreen", category: "Invisible", input: true }
  , { key: "a3", group: "p", isGroup: false, color: "darkgreen", category: "Invisible", input: true }
  , { key: "B", group: "p", isGroup: false, color: "darkgreen", category: "", input: false }
  , { key: "b1", group: "p", isGroup: false, color: "darkgreen", category: "Invisible", input: true }
  , { key: "b2", group: "p", isGroup: false, color: "darkgreen", category: "Invisible", input: true }
  , { key: "C", group: "p", isGroup: false, color: "darkgreen", category: "", input: false }
  , { key: "c1", group: "p", isGroup: false, color: "darkgreen", category: "Invisible", input: true }
  ]

q :: Array (Record NodeData)
q =
  [ { key: "q", group: "", isGroup: true, color: "pink",   category: "", input: true }
  , { key: "1", group: "q", isGroup: false, color: "darkred", category: "", input: true }
  , { key: "1a", group: "q", isGroup: false, color: "darkred", category: "Invisible", input: false }
  , { key: "1b", group: "q", isGroup: false, color: "darkred", category: "Invisible", input: false }
  , { key: "1c", group: "q", isGroup: false, color: "darkred", category: "Invisible", input: false }
  , { key: "2", group: "q", isGroup: false, color: "darkred", category: "", input: true }
  , { key: "2a", group: "q", isGroup: false, color: "darkred", category: "Invisible", input: false }
  , { key: "2b", group: "q", isGroup: false, color: "darkred", category: "Invisible", input: false }
  , { key: "3", group: "q", isGroup: false, color: "darkred", category: "", input: true }
  , { key: "3a", group: "q", isGroup: false, color: "darkred", category: "Invisible", input: false }
  ]

nodeData :: Array (Record NodeData)
nodeData = p <> q

pLinks :: Array (Record LinkData)
pLinks =
  [ { from: "A", to: "a1" }
  , { from: "A", to: "a2" }
  , { from: "A", to: "a3" }
  , { from: "B", to: "b1" }
  , { from: "B", to: "b2" }
  , { from: "C", to: "c1" }
  ]

qLinks :: Array (Record LinkData)
qLinks =
  [ { from: "1", to: "1a" }
  , { from: "1", to: "1b" }
  , { from: "1", to: "1c" }
  , { from: "2", to: "2a" }
  , { from: "2", to: "2b" }
  , { from: "3", to: "3a" }
  ]

linkData :: Array (Record LinkData)
linkData = pLinks <> qLinks

makePort align =
  shape RoundedRectangle $ do
    set
      { fill: "transparent"
      , strokeWidth: 0.0
      , width: 12.0
      , height: 12.0
      , alignment: align
      , portId: "mappingPort"
      , cursor: "pointer"
      , mouseEnter: \_ port _ -> setUnsafe port { fill: "rgba(255,0,255,0.5)" }
      , mouseLeave: \_ port _ -> setUnsafe port { fill: "transparent" }
      }
    binding @"toLinkable" @"input" Nothing Nothing
    binding @"fromLinkable" @"input" (Just not) Nothing

tablePanel = node @Auto' $ do
  panel @Table' $ do
    panel @Auto' $ do
      shape Circle $ do
        set { alignment: center, desiredSize: SizeEach { w: 20.0, h: 20.0 } }
        binding @"fill" @"color" Nothing Nothing
      textBlock "" $ do
        set { alignment: Spot { x: 0.5, y: 1.4, offsetx: 0.0, offsety: 0.0 } }
        binding @"text" @"key" Nothing Nothing
  makePort bottom

visibleNodeTemplate = node @Auto' $ do
  panel @Spot' $ do
    shape Circle $ do
      set { alignment: center, desiredSize: SizeEach { w: 20.0, h: 20.0 } }
      binding @"fill" @"color" Nothing Nothing
    textBlock "" $ do
      set { alignment: Spot { x: 0.5, y: 1.4, offsetx: 0.0, offsety: 0.0 } }
      binding @"text" @"key" Nothing Nothing
  makePort bottom

invisibleNodeTemplate = node @Auto' $ do
  makePort top

groupTemplate = group @Vertical' $ do
  panel @Auto' $ do
    shape RoundedRectangle $ do
      binding @"fill" @"color" Nothing Nothing
    placeholder $ do
      set { padding: MarginAll 5.0 }
  textBlock "" $ do
    set { alignment: Spot { x: 0.5, y: 1.0, offsetx: 0.0, offsety: 0.0 }, font: "Bold 12pt math, sans-serif" }
    binding @"text" @"key" Nothing Nothing
  treeLayout $ do
    set { angle: 270.0, arrangement: ArrangementHorizontal, nodeSpacing: 25.0 }

linkTemplate :: MadeGraphObject LinkData Link_ Link_
linkTemplate = link $ do
  shape Figure.None $ do
    set { isPanelMain: true, stroke: "black" }
  shape Figure.None $ do
    set { toArrow: Feather }
  set { curve: Bezier , segmentOrientation: OrientUpright , curviness: -20.0 }
  pure unit


diag :: MakeDiagram NodeData LinkData Diagram_ Unit
diag = do
  addNodeTemplate "" visibleNodeTemplate
  addNodeTemplate "Invisible" invisibleNodeTemplate
  addGroupTemplate "" groupTemplate
  addLinkTemplate "" linkTemplate
  set
    { "undoManager.isEnabled": true
    }
  graphLinksModel $
        set
          { nodeDataArray: nodeData
          , linkDataArray: linkData
          , nodeIsGroupProperty: Property "isGroup"
          }

init ∷ Effect Unit
init = do
  Diagram.make "myDiagramDiv" diag
  Console.log "🍝 a"