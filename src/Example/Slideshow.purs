module Example.Slideshow where

-- import Prelude
-- import Went.GraphObject.Make

-- import Data.Array as Array
-- import Data.Foldable (for_)
-- import Data.Maybe (Maybe(..))
-- import Data.Number (nan)
-- import Data.Symbol (class IsSymbol)
-- import Effect (Effect)
-- import Effect.Console as Console
-- import GoJS.Diagram.CommandHandler.Methods (scrollToPart_)
-- import GoJS.Diagram.InputEvent.Properties (_diagram)
-- import GoJS.Diagram.Methods (findNodeForKey_)
-- import GoJS.Diagram.Properties (_commandHandler, _model)
-- import GoJS.Diagram.Types (class IsPanel, Auto', Basic', Diagram_, InputEvent_, Node_, Spot', Table', TextBlock_)
-- import GoJS.Geometry.Point.Constructors (newPoint)
-- import GoJS.Geometry.Point.Methods as Point
-- import GoJS.Geometry.Types (Point_)
-- import GoJS.GraphObject.Panel.Part.Properties (_location)
-- import GoJS.GraphObject.Panel.Properties (_data)
-- import GoJS.GraphObject.Properties (_part)
-- import GoJS.Model.Methods (addNodeData_)
-- import GoJS.Model.Properties (_nodeDataArray)
-- import GoJS.Model.Types (TreeModel_)
-- import GoJS.Unsafe.Set (setUnsafe)
-- import Prim.Row (class Cons)
-- import Type.Data.List (type (:>))
-- import Went.Diagram.EnumValue.GestureBehavior (GestureBehavior(..))
-- import Went.Diagram.EnumValue.InitialAnimationStyle as InitialAnimationStyle
-- import Went.Diagram.EnumValue.MouseWheelBehavior (MouseWheelBehavior(..))
-- import Went.Diagram.Make (MakeDiagram, addLinkTemplate, addNodeTemplate, attach)
-- import Went.Diagram.Make as Diagram
-- import Went.Geometry.Margin (Margin(..))
-- import Went.Geometry.Size (Size(..))
-- import Went.Geometry.Spot (Spot(..))
-- import Went.Geometry.Spot as Spot
-- import Went.GraphObject.Binding (binding)
-- import Went.GraphObject.EnumValue.Routing (Routing(..))
-- import Went.GraphObject.EnumValue.Wrap (Wrap(..))
-- import Went.Layout.EnumValue.TreeAlignment (Alignment(..))
-- import Went.Layout.EnumValue.TreeArrangement (Arrangement(..))
-- import Went.Model.Make (model)
-- import Went.Settable (set)
-- -- TODO: Rewrite
-- type LinkData =
--   ( visible :: Boolean
--   )

-- type NodeData =
--   ( title :: String
--   , text1 :: String
--   , text2 :: String
--   , text3 :: String
--   , text4 :: String
--   , category :: String
--   , key :: Int
--   , parent :: Int
--   )

-- columnCount :: Int
-- columnCount = 3

-- addX :: Number -> Point_ -> Point_
-- addX x p = Point._add p (newPoint x 0.0)
-- addY :: Number -> Point_ -> Point_
-- addY y p = Point._add p (newPoint 0.0 y)

-- titleFont :: String
-- titleFont = "100 25pt Inter, sans-serif"

-- plainFont :: String
-- plainFont = "100 17pt Inter, sans-serif"

-- monoFont :: String
-- monoFont = "100 12pt Sometype Mono, sans-serif"

-- titleBlock = do
--   set
--     { font: titleFont
--     , stroke: "black"
--     , columnSpan: columnCount
--     }
--   binding @"text" @"title" @String @String Nothing Nothing


-- slideTextBlock :: forall rest1036 rd1043 parents1046 p. IsPanel p => Boolean
--                                    -> MakeGraphObject (TextBlock_ :> parents1046)
--                                         NodeData
--                                         p
--                                         Unit
-- slideTextBlock mono = do
--   set
--     { font: if mono then monoFont else plainFont
--     , stroke: "black"
--     }
--   binding @"text" @"text1" @String @String Nothing Nothing

-- row rowIndex = set
--   { row: rowIndex
--   , margin: MarginAll 2.0
--   , wrap: WrapFit
--   , editable: true
--   }

-- column columnIndex = set
--   { column: columnIndex
--   , margin: MarginAll 2.0
--   , wrap: WrapFit
--   , editable: true
--   }

-- shapeStyle = do
--   shape "RoundedRectangle" do
--     set { fill: "bisque", stroke: "burlywood", strokeWidth: 3.5 }

-- slideHeight :: Number
-- slideHeight = 600.0
-- slideWidth :: Number
-- slideWidth = slideHeight * 1.5

-- slideSize = set { desiredSize: SizeEach { h: slideHeight, w: slideWidth } }
-- slideSizeTall = set { desiredSize: SizeEach { h: slideHeight * 1.9, w: slideWidth } }


-- -- TODO:
-- -- 1. Act on notes
-- -- 2. Separate purescript-gojs , purescript-went and purescript-went-example libraries into 
-- -- local folders and refer to them in code for the presentation
-- -- 3. Have a setup with other examples running
-- -- 4. Figure out license setting

-- slideData k = case k of
--   1 ->
--     { toAdd:
--         [ 
--             { category: "Slide2"
--             , title: "Welcome to the frontend"
--             , text1:
--                 """
-- Went is a library for writing GoJS directly in PureScript.
-- """
--             , text2:
--                 """
-- Slides:
-- - GoJS: what it is and how it works
-- - PureScript: why it's good
-- - PureScript + GoJS: Why this 
-- library exists
-- """
--             , text3:
-- """
-- Live coding:
-- - What Went development aims
-- to look like
-- - Write up a simple interactive
-- diagram
-- """
--             , text4: ""
--             , key: k
--             , parent: k - 1
--             }
--         ]
--     , toScrollTo: k
--     }
--   2 ->
--     { toAdd:
--         [ 
--             { category: "BranchingSlide"
--             , title: "What is GoJS?"
--             , text1:
-- """
-- A framework for creating dynamic and interactive diagrams programatically
-- """
--             , text2: "gojs.png"
--             , text3: "Why GoJS? Well..."
--             , text4: ""
--             , key: k
--             , parent: k - 1
--             }
--         ]
--     , toScrollTo: k
--     }
--   3 ->
--     { toAdd:
--         [ 
--             { key: k
--             , category: "PostBranchTop"
--             , title: "How does it work?"
--             , text1: 
-- """
-- - Makes heavy use of object-orientation
-- - Supports TypeScript
-- - Depends on a canvas in a div to render diagrams
-- """
--             , text2: "The most basic concepts are: Diagrams and GraphObjects"
--             , text3: ""
--             , text4: ""
--             , parent: k - 1
--             }
--         , 
--             { key: k + 1
--             , category: "PicSlide"
--             , title: "\"GraphObjects are the building blocks of GoJS\""
--             , text1: "graphobjecthierarchy.png"
--             , text2: ""
--             , text3: ""
--             , text4: ""
--             , parent: k - 1
--             }
--         ]
--     , toScrollTo: k
--     }
--   4 ->
--     { toAdd:
--         [ 
--             { category: "PicSlide"
--             , title: "Visualizing GoJS structure"
--             , text1: "visualtree.png"
--             , text2: ""
--             , text3: ""
--             , text4: ""
--             , key: k
--             , parent: k - 1
--             }
--         ]
--     , toScrollTo: k
--     }

--   5 ->
--     { toAdd:
--         [ 
--             { category: "PicSlide2"
--             , title: "Visualizing GoJS structure"
--             , text1: "visualtree.png"
--             , text2: ""
--             , text3: ""
--             , text4: ""
--             , key: k
--             , parent: k - 1
--             }
--         ]
--     , toScrollTo: k
--     }
--   6 -> 
--     { toAdd:
--         [ 
--             { category: "PicSlide3"
--             , title: "How it's usually done: nodeTemplate + models + bindings"
--             , text1: "nodeTemplateCode.png"
--             , text2: ""
--             , text3: ""
--             , text4: ""
--             , key: k
--             , parent: k - 1
--             }
--         ]
--     , toScrollTo: k
--     }
--   7 -> 
--     { toAdd:
--         [ 
--             { category: "PicSlide4"
--             , title: "This renders to:"
--             , text1: "rendered.png"
--             , text2: "That's all fine and dandy, but..."
--             , text3: ""
--             , text4: ""
--             , key: k
--             , parent: k - 1
--             }
--         ]
--     , toScrollTo: k
--     }
--   8 -> 
--     { toAdd:
--         [ 
--             { key: k + 1
--             , category: "VeryUnusualTop"
--             , title: ""
--             , text1: ""
--             , text2: ""
--             , text3: "shapeFillDoc.png"
--             , text4: "textBlockDoc.png"
--             , parent: k - 1
--             }
--         , 
--             { key: k
--             , category: "VeryUnusualBottom"
--             , title: "There's room for type safety"
--             , text1: ""
--             , text2:"nodeTemplateCodeAnnotated.png"
--             , text3: ""
--             , text4: ""
--             , parent: k - 1
--             }
--         ]
--     , toScrollTo: k
--     }
--   9 ->
--     { toAdd:
--         [ 
--             { category: "End"
--             , title: "Let's look at some code"
--             , text1:""
--             , text2: ""
--             , text3: ""
--             , text4: ""
--             , key: k
--             , parent: k - 1
--             }
--         ]
--     , toScrollTo: k
--     }
--   10 -> 
--     { toAdd:
--         [ 
--             { category: "Slide3"
--             , title: "Which brings us to PureScript"
--             , text1: "\"What if JavaScript were Haskell?\""
--             , text2:
-- """
-- -- Went code for the previous slide's nodeTemplate

-- -- Auto, fill, color, text and key are now type-level strings

-- node @Auto' do

-- ..shape "RoundedRectangle" do

-- ....set { fill: "white" }

-- ....binding @"fill" @"color" Nothing Nothing

-- ..textBlock "" do

-- ....set {margin: MarginAll 5.0}

-- ....binding @"text" @"key" Nothing Nothing
-- """
--             , text3: 
-- """
-- Why?

-- - To make GoJS directly 
-- available to purs apps

-- - To bring purs' benefits
-- to GoJS diagramming
-- """
--             , text4: ""
--             , key: k
--             , parent: k - 2
--             }
--         ]
--     , toScrollTo: k
--     }
--   11 ->
--     { toAdd:
--         [ 
--             { category: "PolySlide"
--             , title: "An idea for a diagram"
--             , text1: "A very visual math concept: \"polynomial functors\""
--             , text2: "poly.png"
--             , text3: "They form what's called a category"
--             , text4: ""
--             , key: k
--             , parent: k - 1
--             }
--         ]
--     , toScrollTo: k
--     }
--   12 ->
--     { toAdd:
--         [ 
--             { category: "PolySlide2"
--             , title: "A different kind of category theoretic diagram"
--             , text1: "These functors can be mapped to one another"
--             , text2: "polymap.png"
--             , text3: "Wouldn't it be nice to interact with this?"
--             , text4: ""
--             , key: k
--             , parent: k - 1
--             }
--         ]
--     , toScrollTo: k
--     }
--   _ ->
--     { toAdd:
--         [ 
--             { category: "End"
--             , title: "Let's try our hand at coding this with Went"
--             , text1:""
--             , text2: ""
--             , text3: ""
--             , text4: ""
--             , key: k
--             , parent: k - 1
--             }
--         ]
--     , toScrollTo: k
--     }

-- slideClick = \(event :: InputEvent_ Diagram_) but ->
--   case but # _part @(Node_ _) of
--     Nothing -> pure unit
--     Just thisSlide -> do
--       let
--         diagram = event # _diagram
--         model = diagram # _model @(TreeModel_ _)
--         newNodeKey = Array.length $ model # _nodeDataArray
--         parentkey = (thisSlide # _data).key
--         { toAdd, toScrollTo } = slideData newNodeKey
--       for_ toAdd \childdata -> do
--         case childdata of
--            d -> model # addNodeData_ d
--       diagram # findNodeForKey_ @(Node_ _) toScrollTo >>= case _ of
--         Just child -> do
--           if toScrollTo == 8 then
--             setUnsafe child { location: thisSlide # _location >>> addY (slideWidth + slideWidth / 9.0) } else
--             pure unit
--           setUnsafe child { location: thisSlide # _location >>> addX (slideWidth + slideWidth / 9.0) }
--           diagram # _commandHandler >>> scrollToPart_ child
--         Nothing -> pure unit

-- postBranchTopClick = \(event :: InputEvent_ Diagram_) but ->
--   case but # _part @(Node_ _) of
--     Nothing -> pure unit
--     Just thisSlide -> do
--       let
--         diagram = event # _diagram
--         model = diagram # _model @(TreeModel_ _
-- )
--         parentkey = (thisSlide # _data).key
--       diagram # findNodeForKey_ @(Node_ _) (parentkey + 1) >>= case _ of
--         Just child -> do
--           diagram # _commandHandler >>> scrollToPart_ child
--         Nothing -> pure unit

-- squareWithShapes = panel @Spot' $ do
--   set { row: 1, column: 2 }
--   shape "RoundedRectangle" $ set { fill: "burlywood", width: slideWidth / 3.0, height: slideWidth / 3.0, stroke: "transparent" }
--   panel @Auto' $ do
--     set { alignment: Spot { x: 0.2, y: 0.2, offsetx: 0.0, offsety: 0.0 } }
--     shape "Circle" $ do
--       set { fill: "darkgoldenrod", width:  slideWidth / 18.0, height: slideWidth / 18.0, stroke: "transparent" }
--     shape "LineRight" $ do
--       set { fill: "blanchedalmond", width: 25.0, height: 25.0, stroke: "blanchedalmond" }
--   panel @Auto' $ do
--     set { alignment: Spot { x: 0.8, y: 0.2, offsetx: 0.0, offsety: 0.0 } }
--     shape "Circle" $ do
--       set { fill: "darkgoldenrod", width:  slideWidth / 18.0, height: slideWidth / 18.0, stroke: "transparent" }
--     shape "LineDown" $ do
--       set { fill: "blanchedalmond", width: 25.0, height: 25.0, stroke: "blanchedalmond" }
--   panel @Auto' $ do
--     set { alignment: Spot { x: 0.8, y: 0.8, offsetx: 0.0, offsety: 0.0 } }
--     shape "Circle" $ do
--       set { fill: "darkgoldenrod", width:  slideWidth / 18.0, height: slideWidth / 18.0, stroke: "transparent" }
--     shape "LineLeft" $ do
--       set { fill: "blanchedalmond", width: 25.0, height: 25.0, stroke: "blanchedalmond" }
--   panel @Auto' $ do
--     set { alignment: Spot { x: 0.2, y: 0.8, offsetx: 0.0, offsety: 0.0 } }
--     shape "Circle" $ do
--       set { fill: "darkgoldenrod", width:  slideWidth / 18.0, height: slideWidth / 18.0, stroke: "transparent" }
--     shape "LineUp" $ do
--       set { fill: "blanchedalmond", width: 25.0, height: 25.0, stroke: "blanchedalmond" }

-- introSlide = node @Auto' $ do
--   shapeStyle
--   panel @Table' $ do
--     slideSize
--     set { columnCount: columnCount }
--     textBlock "" $ titleBlock *> row 0 *> set { margin: MarginEach { bottom: 0.0, top: 80.0, left: 0.0, right: 0.0 } }
--     picture "purescript.svg" $ do
--       set { row: 1, column: 0, scale: 0.25, margin: MarginEach { bottom: -100.0, top: -130.0, left: 30.0, right: 0.0 } }
--     squareWithShapes
--   nextButton slideClick true

-- slide = node @Auto' $ do
--   shapeStyle
--   panel @Table' $ do
--     slideSize
--     textBlock "" $ titleBlock *> row 0
--     textBlock "" $ slideTextBlock {- @"text1" -} false *> row 1 *> column 0
--     textBlock "" $ slideTextBlock {- @"text2" -} true *> row 1 *> column 1
--   nextButton slideClick true

-- slide2 = node @Auto' $ do
--   shapeStyle
--   panel @Table' $ do
--     slideSize
--     textBlock "" $ titleBlock *> row 0 *> set { margin: MarginEach { bottom: 0.0, top: 80.0, left: 0.0, right: 0.0 } }
--     textBlock "" $ slideTextBlock {- @"text1" -} false *> row 1 *> set {columnSpan: columnCount}
--     textBlock "" $ slideTextBlock {- @"text2" -} false *> row 2 *> column 0 *> set { margin: MarginEach { bottom: 0.0, top: -50.0, left: 0.0, right: 0.0 } }
--     textBlock "" $ slideTextBlock {- @"text3" -} false *> row 2 *> column 1 *> set { margin: MarginEach { bottom: 0.0, top: -50.0, left: 0.0, right: 0.0 } }
--   nextButton slideClick true

-- slide3 = node @Auto' $ do
--   shapeStyle
--   set {locationSpot: Spot {x: 0.5, y: -1.0, offsetx: 0.0, offsety: 0.0}}
--   panel @Table' $ do
--     slideSize
--     textBlock "" $ titleBlock *> row 0
--     textBlock "" $ slideTextBlock {- @"text1" -} false *> row 1 *> set {columnSpan: columnCount}
--     textBlock "" $ slideTextBlock {- @"text2" -} true *> row 2 *> column 0
--     textBlock "" $ slideTextBlock {- @"text3" -} false *> row 2 *> column 1
--   nextButton slideClick true

-- branchingSlide = node @Auto' $ do
--   shapeStyle
--   panel @Table' $ do
--     slideSize
--     textBlock "" $ titleBlock *> row 0
--     textBlock "" $ slideTextBlock {- @"text1" -} false *> row 1 *> set {columnSpan: columnCount}
--     picture "" $ do
--         binding @"source" @"text2" Nothing Nothing
--         set { row: 2, columnSpan: columnCount, desiredSize: SizeEach {w: slideWidth / 1.0, h: slideHeight / 1.6} }    
--     textBlock "" $ slideTextBlock {- @"text3" -} false *> row 3 *> set {columnSpan: columnCount}
--   nextButton slideClick true


-- postBranchTop = node @Auto' $ do
--   shapeStyle
--   panel @Table' $ do
--     slideSize
--     textBlock "" $ titleBlock *> row 0
--     textBlock "" $ slideTextBlock {- @"text1" -} false *> row 1 *> column 0
--     textBlock "" $ slideTextBlock {- @"text2" -} false  *> row 2 *> column 0
--   nextButton postBranchTopClick false

-- picSlide = node @Auto' $ do
--   shapeStyle
--   panel @Table' $ do
--     slideSize
--     set { columnCount: columnCount }
--     textBlock "" $ titleBlock *> row 0 *> set { margin: MarginEach { bottom: 0.0, top: 80.0, left: 0.0, right: 0.0 } }
--     picture "" $ do
--       binding @"source" @"text1" Nothing Nothing
--       set { row: 1, columnSpan: 3 }
--   nextButton slideClick true

-- picSlide2 = node @Auto' $ do
--   shapeStyle
--   panel @Table' $ do
--     slideSize
--     set { columnCount: columnCount }
--     textBlock "" $ titleBlock *> row 0 *> set { margin: MarginEach { bottom: 0.0, top: 80.0, left: 0.0, right: 0.0 } }
--     picture "" $ do
--       binding @"source" @"text1" Nothing Nothing
--       set { row: 1, columnSpan: 3, desiredSize: SizeEach {w: slideWidth / 1.3, h: slideHeight / 1.3}}
--   nextButton slideClick true

-- picSlide3 = node @Auto' $ do
--   shapeStyle
--   panel @Table' $ do
--     slideSize
--     set { columnCount: columnCount }
--     textBlock "" $ titleBlock *> row 0 *> set { margin: MarginEach { bottom: 0.0, top: 80.0, left: 0.0, right: 0.0 } }
--     picture "" $ do
--       binding @"source" @"text1" Nothing Nothing
--       set { row: 1, columnSpan: 3, desiredSize: SizeEach {w: slideWidth / 1.1, h: slideHeight / 1.3}}
--   nextButton slideClick true

-- picSlide4 = node @Auto' $ do
--   shapeStyle
--   panel @Table' $ do
--     slideSize
--     set { columnCount: columnCount }
--     textBlock "" $ titleBlock *> row 0 *> set { margin: MarginEach { bottom: 0.0, top: 80.0, left: 0.0, right: 0.0 } }
--     picture "" $ do
--       binding @"source" @"text1" Nothing Nothing
--       set { row: 1, columnSpan: 3, desiredSize: SizeEach {w: slideWidth / 1.7, h: slideHeight / 1.7}}
--     textBlock "" $ slideTextBlock {- @"text2" -} false *> row 2 *> set {columnSpan: columnCount}
--   nextButton slideClick true

-- veryUnusualBottom = node @Auto' $ do
--   shapeStyle
--   panel @Table' $ do
--     slideSize
--     set { columnCount: columnCount * 2}
--     textBlock "" $ titleBlock *> row 0 *> set { margin: MarginEach { bottom: 0.0, top: 80.0, left: 0.0, right: 0.0 } }
--     picture "" $ do
--       binding @"source" @"text2" Nothing Nothing
--       set { row: 1, columnSpan: 3, desiredSize: SizeEach {w: slideWidth / 1.1, h: slideHeight / 1.4 }}
--   nextButton2 postBranchTopClick false

-- veryUnusualTop = node @Auto' $ do
--   shapeStyle
--   panel @Table' $ do
--     slideSize
--     set { columnCount: columnCount * 2}
--     picture "" $ do
--       binding @"source" @"text3" Nothing Nothing
--       set { row: 1, column: 0, desiredSize: SizeEach {w: slideWidth / 2.5, h: slideHeight / 1.85}}
--     picture "" $ do
--       binding @"source" @"text4" Nothing Nothing
--       set { row: 1, column: 1, desiredSize: SizeEach {w: slideWidth / 2.5, h: slideHeight / 1.1}}
--   nextButton slideClick true

-- polySlide = node @Auto' $ do
--   shapeStyle
--   set {locationSpot: Spot {x: 0.5, y: -1.0, offsetx: 0.0, offsety: 0.0}}
--   panel @Table' $ do
--     slideSize
--     textBlock "" $ titleBlock *> row 0 *> set { margin: MarginEach { bottom: 0.0, top: 80.0, left: 0.0, right: 0.0 } }
--     textBlock "" $ slideTextBlock {- @"text1" -} false *> row 1 *> set {columnSpan: columnCount}
--     picture "" $ do
--         binding @"source" @"text2" Nothing Nothing
--         set { row: 2, column: 0, desiredSize: SizeEach {w: slideWidth / 1.1, h: 140.0}}
--     textBlock "" $ slideTextBlock {- @"text3" -} false *> row 3 *> set {columnSpan: columnCount}
--   nextButton slideClick true

-- polySlide2 = node @Auto' $ do
--   shapeStyle
--   set {locationSpot: Spot {x: 0.5, y: -1.0, offsetx: 0.0, offsety: 0.0}}
--   panel @Table' $ do
--     slideSize
--     textBlock "" $ titleBlock *> row 0 *> set { margin: MarginEach { bottom: 0.0, top: 80.0, left: 0.0, right: 0.0 } }
--     textBlock "" $ slideTextBlock {- @"text1" -} false *> row 1 *> set {columnSpan: columnCount}
--     picture "" $ do
--         binding @"source" @"text2" Nothing Nothing
--         set { row: 2, column: 0, desiredSize: SizeEach {w: slideWidth / 1.45, h: 160.0}}
--     textBlock "" $ slideTextBlock {- @"text3" -} false *> row 3 *> set {columnSpan: columnCount}
--   nextButton slideClick true

-- endSlide = node @Auto' $ do
--   shapeStyle
--   set {locationSpot: Spot {x: 0.5, y: -1.0, offsetx: 0.0, offsety: 0.0}}
--   panel @Table' $ do
--     slideSize
--     textBlock "" $ titleBlock *> row 1

-- nextButton click right = button @Basic' @Auto' $ do
--   textBlock "Next" $ set { angle: if right then 270.0 else 0.0 }
--   set
--     { name: "NextSlides"
--     , alignment: if right then Spot.right else Spot.bottom
--     , opacity: 0.7
--     , click: click
--     , width: if right then 25.0 else nan
--     , height: if right then nan else 25.0
--     }

-- nextButton2 click right = button @Basic' @Auto' $ do
--   textBlock "Next" $ set { angle: if right then 270.0 else 0.0 }
--   set
--     { 
--       name: "NextSlides"
--     , alignment: if right then Spot.right else Spot.top
--     , opacity: 0.7
--     , click: click
--     , width: if right then 25.0 else nan
--     , height: if right then nan else 25.0
--     }


-- linkTemplate = link $ do
--   set { routing: Orthogonal, layerName: "Background", corner: 5.0, opacity: 0.0 }
--   shape "None" $ do
--     set { strokeWidth: 1.5, stroke: "#F5F5F5" }

-- nodeData :: Array (Record NodeData)
-- nodeData =
--   [ { category: "Intro"
--     , title: "Went: GoJS diagrams in PureScript"
--     , text1: ""
--     , text2: ""
--     , text3: ""
--     , text4: ""
--     , key: 0
--     , parent: 0
--     }
--   ]

-- linkData :: Array (Record LinkData)
-- linkData =
--   []

-- diag :: MakeDiagram NodeData LinkData Diagram_ Unit
-- diag = do
--   addNodeTemplate "Intro" introSlide
--   addNodeTemplate "Slide" slide
--   addNodeTemplate "Slide2" slide2
--   addNodeTemplate "Slide3" slide3
--   addNodeTemplate "BranchingSlide" branchingSlide
--   addNodeTemplate "PostBranchTop" postBranchTop
--   addNodeTemplate "PicSlide" picSlide
--   addNodeTemplate "PicSlide2" picSlide2
--   addNodeTemplate "PicSlide3" picSlide3
--   addNodeTemplate "PicSlide4" picSlide4
--   addNodeTemplate "VeryUnusualBottom" veryUnusualBottom
--   addNodeTemplate "VeryUnusualTop" veryUnusualTop
--   addNodeTemplate "PolySlide" polySlide
--   addNodeTemplate "PolySlide2" polySlide2
--   addNodeTemplate "End" endSlide
--   addLinkTemplate "" linkTemplate
--   set
--     { layout: layout @"TreeLayout" $ do
--         set
--           { arrangement: ArrangementVertical
--           , angle: 0.0
--           , layerSpacing: 35.0
--           , alternateAngle: 90.0
--           , alternateLayerSpacing: 35.0
--           , alternateAlignment: AlignmentBus
--           , alternateNodeSpacing: 20.0
--           }
--     , model: model @"TreeModel" $
--         set
--           { nodeDataArray: nodeData
--           }
--     }
--   -- TODO: See whether https://github.com/rowtype-yoga/purescript-untagged-union  should be used here!!
--   attach
--     { allowZoom: true
--     , "animationManager.isEnabled": true
--     , "toolManager.mouseWheelBehavior": WheelScroll
--     , "toolManager.gestureBehavior": GestureZoom
--     , "animationManager.initialAnimationStyle": InitialAnimationStyle.None
--     , "undoManager.isEnabled": true
--     }

-- init ∷ Effect Unit
-- init = do
--   Diagram.make "myDiagramDiv" diag
--   Console.log "🍝 a"