module Example.Flowchart where

import Prelude hiding (top, bottom)

import Data.Maybe (Maybe(..), maybe)
import Data.Number (nan)
import Data.String (indexOf, take)
import Data.String.CodeUnits (length)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console as Console
import GoJS.Diagram.DiagramEvent.Properties (_subject)
import GoJS.Diagram.DiagramEvent.Properties as DiagramEvent
import GoJS.Diagram.InputEvent.Properties (_diagram)
import GoJS.Diagram.Properties (_isModified, _isReadOnly)
import GoJS.Diagram.Types (DiagramEvent_, Diagram_, Palette_, InputEvent_)
import GoJS.Geometry.Point.Static as Point
import GoJS.GraphObject.Panel.Methods (findObject_)
import GoJS.GraphObject.Panel.Part.Link.Properties (_fromNode)
import GoJS.GraphObject.Panel.Part.Properties (_category)
import GoJS.GraphObject.Types (Link_, Node_, Panel_, Shape_)
import GoJS.Key (KeyProperty(..))
import GoJS.Unsafe.Set (setUnsafe)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (removeAttribute, setAttribute)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (setTitle, title, toDocument)
import Web.HTML.Window (document)
import Went.Diagram.EnumValue.GestureBehavior (GestureBehavior(..))
import Went.Diagram.EnumValue.InitialAnimationStyle as InitialAnimationStyle
import Went.Diagram.EnumValue.MouseWheelBehavior (MouseWheelBehavior(..))
import Went.Diagram.Event (DiagramEvent(..))
import Went.Diagram.Make (MakeDiagram, addDiagramListener, addLinkTemplate, addNodeTemplate, attach, defineFigureGenerator)
import Went.Diagram.Make as Diagram
import Went.Geometry (Geometry(..), Margin(..), PathSegment(..), Point, Size(..), Spot(..), bottom, bottomRight, center, left, pathFigure, right, top)
import Went.GraphObject (Auto', Curve(..), MadeGraphObject, Routing(..), Spot', Stretch(..), Table', Wrap(..), link, node, panel, shape, textBlock)
import Went.GraphObject.Shape.Arrowhead as Arrowhead
import Went.GraphObject.Shape.Figure (Figure(..))
import Went.GraphObject.Shape.Figure as Figure
import Went.GraphObject.TextBlock.TextAlign (TextAlign(..))
import Went.Model (binding, binding1, binding1TwoWay, bindingOfObject, graphLinksModel)
import Went.Settable (set)

type LinkData =
  ( points :: Array Point
  , from :: Int
  , fromPort :: String
  , to :: Int
  , toPort :: String
  , visible :: Boolean
  , text :: String
  )

type NodeData =
  ( text :: String
  , loc :: String
  , figure :: Figure
  , category :: String
  , key :: Int
  )

textStyle = set
  { font: "bold 11pt Lato, Helvetica, Arial, sans-serif"
  , stroke: "#F8F8F8"
  }

makePort name align spot output input =
  shape RoundedRectangle $ do
    set
      { fill: "transparent"
      , strokeWidth: 0.0
      , width: if horizontal then nan else 8.0
      , height: if not horizontal then nan else 8.0
      , alignment: align
      , stretch: if horizontal then Horizontal else Vertical
      , portId: name
      , fromSpot: spot
      , fromLinkable: output
      , toSpot: spot
      , toLinkable: input
      , cursor: "pointer"
      , mouseEnter: \(e :: InputEvent_ Diagram_) port _ -> do
          when (not $ e # _diagram >>> _isReadOnly) $ do
            setUnsafe port { fill: "rgba(255,0,255,0.5)" }
      , mouseLeave: \_ port _ -> setUnsafe port { fill: "transparent" }
      }
  where
  horizontal = false -- align == top || align == bottom

nodeStyle = do
  binding @"location" @"loc" (Just Point.parse_) (Just Point.stringify_)
  set { locationSpot: center }

default :: MadeGraphObject NodeData Node_ Node_
default = node @Table' $ do
  nodeStyle
  panel @Auto' $ do
    shape Rectangle do
      set { fill: "#282c34", stroke: "#00A9C9", strokeWidth: 3.5 }
      binding1 @"figure"
    textBlock "" do
      textStyle
      set
        { margin: MarginAll 8.0
        , maxSize: SizeEach { w: 160.0, h: nan }
        , wrap: WrapFit
        , editable: true
        }
      binding1 @"text"
  makePort "T" top TopSide false true
  makePort "L" left LeftSide true true
  makePort "R" right RightSide true true
  makePort "B" bottom BottomSide true false

conditional :: MadeGraphObject NodeData Node_ Node_
conditional = node @Table' $ do
  nodeStyle
  panel @Table' $ do
    panel @Auto' $ do
      shape Diamond $ do
        set { fill: "#282c34", stroke: "#00A9C9", strokeWidth: 3.5 }
        binding1 @"figure"
      textBlock "" $ do
        textStyle
        set
          { margin: MarginAll 8.0
          , maxSize: SizeEach { w: 160.0, h: nan }
          , wrap: WrapFit
          , editable: true
          }
        binding1TwoWay @"text"
    makePort "T" top top false true
    makePort "L" left left true true
    makePort "R" right right true true
    makePort "B" bottom bottom true false

-- start :: MadeGraphObject NodeData Node_ Node_
start = node @Table' $ do
  nodeStyle
  panel @Spot' $ do
    shape Circle $ do
      set { desiredSize: SizeBoth 70.0, fill: "#282c34", stroke: "#09d3ac", strokeWidth: 3.5 }
    textBlock "Start" $ do
      textStyle
      binding1 @"text"
  makePort "L" left left true false
  makePort "R" right right true false
  makePort "B" bottom bottom true false

end :: MadeGraphObject NodeData Node_ Node_
end = node @Table' $ do
  nodeStyle
  panel @Spot' $ do
    shape Circle $ do
      set { desiredSize: SizeBoth 60.0, fill: "#282c34", stroke: "#DC3C00", strokeWidth: 3.5 }
    textBlock "End" $ do
      textStyle
      binding1 @"text"
  makePort "L" top top false true
  makePort "R" left left false true
  makePort "B" right right false true
  pure unit

comment :: MadeGraphObject NodeData Node_ Node_
comment = node @Auto' $ do
  nodeStyle
  shape (Custom "File") $ do
    set { fill: "#282c34", stroke: "#DEE0A3", strokeWidth: 3.0 }
  textBlock "" $ do
    textStyle
    set
      { margin: MarginAll 8.0
      , maxSize: SizeEach { w: 200.0, h: nan }
      , wrap: WrapFit
      , textAlign: Center
      , editable: true
      }
    binding @"text" @"text" (Just identity) (Just identity)

linkTemplate = link $ do
  set
    { routing: AvoidsNodes
    , curve: JumpOver
    , corner: 5.0
    , toShortLength: 4.0
    , relinkableFrom: true
    , relinkableTo: true
    , reshapable: true
    , resegmentable: true
    , mouseEnter: \e link _ -> link # findObject_ @Shape_ "HIGHLIGHT" >>= case _ of
        Just highlighted -> setUnsafe highlighted { stroke: "rgba(30,144,255,0.2)" }
        Nothing -> pure unit
    , mouseLeave: \e link _ -> link # findObject_ @Shape_ "HIGHLIGHT" >>= case _ of
        Just highlighted -> setUnsafe highlighted { stroke: "transparent" }
        Nothing -> pure unit
    , selectionAdorned: false
    }
  binding1 @"points"
  shape Figure.None $ do
    set { isPanelMain: true, strokeWidth: 8.0, stroke: "transparent", name: "HIGHLIGHT" }
  shape Figure.None $ do
    set { isPanelMain: true, stroke: "gray", strokeWidth: 2.0 }
    bindingOfObject @"stroke" "isSelected" (Just $ \sel -> if sel then "dodgerblue" else "gray") Nothing -- ofObject
  shape Figure.None $ do
    set { toArrow: Arrowhead.Standard, strokeWidth: 0.0, fill: "gray" }
  panel @Auto' $ do
    set { visible: false, name: "LABEL", segmentIndex: 2, segmentFraction: 0.5 }
    binding1TwoWay @"visible"
    shape RoundedRectangle $ do
      set { fill: "#F8F8F8", strokeWidth: 0.0 }
    textBlock "Yes" $ do
      set
        { textAlign: Center
        , font: "10pt helvetica, arial, sans-serif"
        , stroke: "#333333"
        , editable: true
        }
      binding1TwoWay @"text"

nodeData :: Array (Record NodeData)
nodeData =
  [ { figure: Figure.None, category: "Comment", key: -13, "loc": "360 -10", "text": "Kookie Brittle" }
  , { figure: Figure.None, category: "Start", key: -1, "loc": "175 0", "text": "Start" }
  , { figure: Figure.None, category: "", key: 0, "loc": "-5 75", "text": "Preheat oven to 375 F" }
  , { figure: Figure.None, category: "", key: 1, "loc": "175 100", "text": "In a bowl, blend: 1 cup margarine, 1.5 teaspoon vanilla, 1 teaspoon salt" }
  , { figure: Figure.None, category: "", key: 2, "loc": "175 200", "text": "Gradually beat in 1 cup sugar and 2 cups sifted flour" }
  , { figure: Figure.None, category: "", key: 3, "loc": "175 290", "text": "Mix in 6 oz (1 cup) Nestle's Semi-Sweet Chocolate Morsels" }
  , { figure: Figure.None, category: "", key: 4, "loc": "175 380", "text": "Press evenly into ungreased 15x10x1 pan" }
  , { figure: Figure.None, category: "", key: 5, "loc": "355 85", "text": "Finely chop 1/2 cup of your choice of nuts" }
  , { figure: Figure.None, category: "", key: 6, "loc": "175 450", "text": "Sprinkle nuts on top" }
  , { figure: Figure.None, category: "", key: 7, "loc": "175 515", "text": "Bake for 25 minutes and let cool" }
  , { figure: Figure.None, category: "", key: 8, "loc": "175 585", "text": "Cut into rectangular grid" }
  , { figure: Figure.None, category: "End", key: -2, "loc": "175 660", "text": "Enjoy!" }
  ]

linkData :: Array (Record LinkData)
linkData =
  [ { from: 1, to: 2, fromPort: "B", toPort: "T", points: [], visible: false, text: "" }
  , { from: 2, to: 3, fromPort: "B", toPort: "T", points: [], visible: false, text: "" }
  , { from: 3, to: 4, fromPort: "B", toPort: "T", points: [], visible: false, text: "" }
  , { from: 4, to: 6, fromPort: "B", toPort: "T", points: [], visible: false, text: "" }
  , { from: 6, to: 7, fromPort: "B", toPort: "T", points: [], visible: false, text: "" }
  , { from: 7, to: 8, fromPort: "B", toPort: "T", points: [], visible: false, text: "" }
  , { from: 8, to: -2, fromPort: "B", toPort: "T", points: [], visible: false, text: "" }
  , { from: -1, to: 0, fromPort: "B", toPort: "T", points: [], visible: false, text: "" }
  , { from: -1, to: 1, fromPort: "B", toPort: "T", points: [], visible: false, text: "" }
  , { from: -1, to: 5, fromPort: "B", toPort: "T", points: [], visible: false, text: "" }
  , { from: 5, to: 4, fromPort: "B", toPort: "T", points: [], visible: false, text: "" }
  , { from: 0, to: 4, fromPort: "B", toPort: "T", points: [], visible: false, text: "" }
  ]

templates
  :: { "" :: MadeGraphObject NodeData Node_ Node_
     , "Conditional" :: MadeGraphObject NodeData Node_ Node_
     , "Start" :: MadeGraphObject NodeData Node_ Node_
     , "End" :: MadeGraphObject NodeData Node_ Node_
     , "Comment" :: MadeGraphObject NodeData Node_ Node_
     }
templates =
  { "": default
  , "Conditional": conditional
  , "Start": start
  , "End": end
  , "Comment": comment
  }

diag :: MakeDiagram NodeData LinkData Diagram_ Unit
diag = do
  defineFigureGenerator "File" $ \_s w h ->
    let
      fig = pathFigure
        { sx: (0.0 * w)
        , sy: 0.0
        , filled: true
        , segments:
            [ Line (0.75 * w) 0.0
            , Line w (0.25 * h)
            , Line w h
            , Close $ Line 0.0 h
            ]
        }
      fig2 = pathFigure
        { sx: (0.75 * w)
        , sy: 0.0
        , filled: false
        , segments:
            [ Line (0.75 * w) (0.25 * h)
            , Line w (0.25 * h)
            ]
        }
      spot1 = Spot { x: 0.0, y: 0.25, offsetx: 0.0, offsety: 0.0 }
      spot2 = bottomRight
    in
      Geometry { figures: [ fig, fig2 ], spot1, spot2 }
  addNodeTemplate "" default
  addNodeTemplate "Conditional" conditional
  addNodeTemplate "Start" start
  addNodeTemplate "End" end
  addNodeTemplate "Comment" comment
  addLinkTemplate "" linkTemplate
  -- TODO: See whether https://github.com/rowtype-yoga/purescript-untagged-union  should be used here!!
  graphLinksModel $ do
    set
      { nodeDataArray: nodeData
      , linkDataArray: linkData
      , linkFromPortIdProperty: Property "fromPort"
      , linkToPortIdProperty: Property "toPort"
      }
  set
    { "toolManager.linkingTool.temporaryLink.routing": Orthogonal
    , "toolManager.relinkingTool.temporaryLink.routing": Orthogonal
    }
  attach
    { allowZoom: true
    , "animationManager.isEnabled": true
    , "toolManager.mouseWheelBehavior": WheelScroll
    , "toolManager.gestureBehavior": GestureZoom
    , "animationManager.initialAnimationStyle": InitialAnimationStyle.None
    , "undoManager.isEnabled": true
    }
  -- TODO: Rethink/incorporate in Tool rework

  addDiagramListener Modified $ \e -> do
    doc <- window >>= document
    let diagramIsModified = e # DiagramEvent._diagram @Diagram_ >>> _isModified
    getElementById "SaveButton" (toNonElementParentNode <<< toDocument $ doc) >>= case _ of
      Just elem ->
        if diagramIsModified then
          removeAttribute "disabled" elem
        else
          setAttribute "disabled" "" elem
      Nothing -> pure unit
    docTitle <- title doc
    case diagramIsModified, (indexOf (Pattern "*") docTitle) of
      true, Nothing -> setTitle (docTitle <> "*") doc
      false, (Just _) -> setTitle (take (length docTitle - 1) docTitle) doc
      _, _ -> pure unit
  addDiagramListener LinkDrawn showLinkLabel
  addDiagramListener LinkRelinked showLinkLabel
  where
  showLinkLabel :: DiagramEvent_ Link_ -> Effect Unit
  showLinkLabel e = case (e # _subject) of
    Just sub -> sub # findObject_ @Panel_ "LABEL" >>= case _ of
      Just lab -> maybe (pure unit) (\fromNode -> setUnsafe lab {visible: (fromNode # _category) == "Conditional"}) (sub # _fromNode @Node_)
      Nothing -> pure unit
    Nothing -> pure unit

pal :: MakeDiagram NodeData LinkData Palette_ Unit
pal = do
  attach { "animationManager.initialAnimationStyle": InitialAnimationStyle.None }
  addNodeTemplate "" default
  addNodeTemplate "Conditional" conditional
  addNodeTemplate "Start" start
  addNodeTemplate "End" end
  addNodeTemplate "Comment" comment
  addLinkTemplate "" linkTemplate
  graphLinksModel $ do
    set
      { nodeDataArray:
          [ { key: -1, category: "Start", text: "Start", figure: Figure.None, loc: "" }
          , { key: -0, category: "", text: "Step", figure: Figure.None, loc: "" }
          , { key: 2, category: "Conditional", text: "???", figure: Figure.None, loc: "" }
          , { key: 1, category: "End", text: "End", figure: Figure.None, loc: "" }
          , { key: 3, category: "Comment", text: "Comment", figure: Figure.None, loc: "" }
          ]
      , linkFromPortIdProperty: Property "fromPort"
      , linkToPortIdProperty: Property "toPort"
      }

init ∷ Effect Unit
init = do
  Diagram.make "myDiagramDiv" diag
  Diagram.makePalette "myPaletteDiv" pal
  Console.log "🍝 a"
