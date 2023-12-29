module Example.OrgChartEditor where

import Prelude

import Data.Array ((!!))
import Data.Foldable (length)
import Data.Int (fromNumber, rem)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Number (nan)
import Data.String.CodePoints (indexOf, take)
import Data.String.CodePoints as String
import Data.String.Pattern (Pattern(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Console (log)
import GoJS.Collection (setFirst)
import GoJS.Diagram.CommandHandler.Methods (editTextBlock_, scrollToPart_)
import GoJS.Diagram.DiagramEvent.Properties as DiagramEvent
import GoJS.Diagram.InputEvent.Properties as InputEvent
import GoJS.Diagram.Methods (commitTransaction_, findNodeForData_, removeParts_, select_, startTransaction_)
import GoJS.Diagram.Properties (_commandHandler, _isModified, _model, _selection, _toolManager)
import GoJS.Diagram.Types (Diagram_, InputEvent_, Node_, Panel_, Shape_)
import GoJS.GraphObject.Panel.Methods (findObject_)
import GoJS.GraphObject.Panel.Part.Adornment.Properties (_adornedPart)
import GoJS.GraphObject.Panel.Part.Node.Methods (findTreeChildrenNodes_, findTreeParentLink_, findTreeParentNode_, findTreeParts_, isInTreeOf_)
import GoJS.GraphObject.Panel.Part.Node.Properties (_port)
import GoJS.GraphObject.Panel.Part.Properties (_location)
import GoJS.GraphObject.Panel.Properties (_data)
import GoJS.GraphObject.Properties (_part)
import GoJS.GraphObject.Properties as GraphObject
import GoJS.GraphObject.Shape.Properties (_fill)
import GoJS.Layout.LayoutNetwork.Properties (_vertexes)
import GoJS.Layout.LayoutVertex.Properties (_level, _node)
import GoJS.Layout.Properties (_network)
import GoJS.Model.Methods (addNodeData_, removeNodeData_, setDataProperty_)
import GoJS.Model.TreeModel.Methods (setParentKeyForNodeData_)
import GoJS.Model.Types (TreeModel_)
import GoJS.Prototype (prototype)
import GoJS.Settable (setUnsafe)
import GoJS.Tool.MouseMoveTools.LinkingTool.Methods (insertLink_)
import GoJS.Tool.Properties as Tool
import GoJS.Tool.ToolManager.Properties (_linkingTool)
import GoJS.Unsafe (getUnsafe)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (removeAttribute, setAttribute)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (setTitle, title, toDocument)
import Web.HTML.Window (document)
import Went.Diagram.EnumValue.ValidCycle (ValidCycle(..))
import Went.Diagram.Event (DiagramEvent(..))
import Went.Diagram.Make (MakeDiagram, addDiagramListener, addLinkTemplate, addNodeTemplate, attach)
import Went.Diagram.Make as Diagram
import Went.FFI.Override (Override(..))
import Went.Geometry.Margin (Margin(..))
import Went.Geometry.Size (Size(..))
import Went.Geometry.Spot as Spot
import Went.GraphObject.EnumValue.Routing (Routing(..))
import Went.GraphObject.EnumValue.Wrap (Wrap(..))
import Went.GraphObject.Make (button, contextMenu, link, node, panel, picture, shape, textBlock)
import Went.GraphObject.Panel (Auto', Basic', ContextMenu', Horizontal', Spot', Table', TreeExpander')
import Went.GraphObject.Shape.Figure (Figure(..))
import Went.Layout.EnumValue.TreeAlignment (Alignment(..))
import Went.Layout.EnumValue.TreeArrangement (Arrangement(..))
import Went.Layout.EnumValue.TreeStyle (TreeStyle(..))
import Went.Layout.Make (treeLayout)
import Went.Model.Binding (binding, bindingOfObject)
import Went.Model.Make (treeModel)
import Went.RowColumnDefinition.Make (rowColumnDefinition)
import Went.Settable (set)
import Went.Template.Makers (MadeNode)
import Went.Tool.Make (clickCreatingTool)

levelColors :: Array String
levelColors =
  [ "#AC193D"
  , "#2672EC"
  , "#8C0095"
  , "#5133AB"
  , "#008299"
  , "#D24726"
  , "#008A00"
  , "#094AB2"
  ]

textStyle = set
  { font: "9pt Segoe UI,sans-serif"
  , stroke: "white"
  }

findHeadShot :: Nullable String -> String
findHeadShot pic = case toMaybe pic of
  Just p -> "images/HS" <> p
  Nothing -> "images/HSnopic.png" 

-- TODO: implement 
-- case pic of
--   Just p -> "images/HS" <> p
--   Nothing -> "images/HSnopic.png"

mayWorkFor :: Node_ -> Node_ -> Effect Boolean
mayWorkFor node1 node2 =
  not <$> (node2 # isInTreeOf_ node1)

nodeTemplate :: MadeNode NodeData Node_
nodeTemplate = node @Spot' $ do
  set
    { selectionObjectName: "BODY"
    , mouseEnter: \_e node _ -> do
        findObject_ @Panel_ "BUTTON" node >>= maybe (pure unit) (flip setUnsafe { opacity: 1.0 })
        findObject_ @Panel_ "BUTTONX" node >>= maybe (pure unit) (flip setUnsafe { opacity: 1.0 })
    , mouseLeave: \_e node _ -> do
        findObject_ @Panel_ "BUTTON" node >>= maybe (pure unit) (flip setUnsafe { opacity: 0.0 })
        findObject_ @Panel_ "BUTTONX" node >>= maybe (pure unit) (flip setUnsafe { opacity: 0.0 })
    , mouseDragEnter: \_e node _ -> do
        let selnode = setFirst $ node # GraphObject._diagram >>> _selection @Node_
        findObject_ "SHAPE" node >>= \maybeSh -> case selnode *> maybeSh of
          Just sh -> setUnsafe sh { _prevFill: sh # _fill, fill: "darkred" }
          Nothing -> pure unit
    , mouseDragLeave: \_e node _ -> findObject_ @Shape_ "SHAPE" node >>= \sh -> case sh >>= toMaybe <<< getUnsafe [ "_prevFill" ] of
        Just f -> setUnsafe sh { fill: f }
        Nothing -> pure unit
    , mouseDrop: \_e node -> do
        let diagram = node # GraphObject._diagram
        case setFirst $ diagram # _selection of
          Nothing -> pure unit
          Just selnode -> do
            canWork <- mayWorkFor selnode node
            maybeLink <- findTreeParentLink_ selnode
            case canWork, maybeLink of
              true, Just l -> setUnsafe l { fromNode: node }
              true, Nothing -> case node # _port @Node_, selnode # _port @Node_ of
                Just targetPort, Just selport -> do
                  void $ diagram # _toolManager >>> _linkingTool >>> insertLink_ node targetPort selnode selport
                _, _ -> pure unit
              _, _ -> pure unit
    }

  -- For sorting, have the Node's text be the nodeData's name
  binding @"text" @"name" Nothing Nothing
  -- Bind the Part's layerName to controle the Node's layer depending on whether it's selected
  bindingOfObject @"layerName" "isSelected" (Just $ \sel -> if sel then "Foreground" else "") Nothing

  panel @Auto' $ do
    set { name: "BODY" }
    -- Node's outer shape is a rectangle
    shape Rectangle $ do
      set { name: "SHAPE", fill: "#333333", stroke: "white", strokeWidth: 3.5, portId: "" }
    panel @Horizontal' $ do
      picture "images/HSnopic.png" $ do
        set
          { name: "Picture"
          , desiredSize: SizeBoth 70.0
          , margin: MarginAll 1.5
          }
        binding @"source" @"pic" (Just findHeadShot) Nothing
      -- Panel where text will appear
      panel @Table' $ do
        set
          { minSize: SizeEach { w: 130.0, h: nan }
          , maxSize: SizeEach { w: 150.0, h: nan }
          , margin: MarginEach { top: 6.0, right: 10.0, bottom: 0.0, left: 6.0 }
          , defaultAlignment: Spot.left
          }
        rowColumnDefinition { column: 2 } $ do
          set { width: 4.0 }
        textBlock "" $ do -- The name
          textStyle
          set
            { name: "NAMETB"
            , row: 0
            , column: 0
            , columnSpan: 5
            , font: "12pt Segoe UI,sans-serif"
            , editable: true
            , isMultiline: false
            , minSize: SizeEach { w: 50.0, h: 16.0 }
            }
          binding @"text" @"name" (Just identity) (Just identity)
        textBlock "Title: " $ do
          textStyle
          set { row: 1, column: 0 }
        textBlock "" $ do
          textStyle
          set
            { row: 1
            , column: 1
            , columnSpan: 4
            , editable: true
            , isMultiline: false
            , minSize: SizeEach { w: 50.0, h: 14.0 }
            , margin: MarginEach { top: 0.0, right: 0.0, bottom: 0.0, left: 3.0 }
            }
          binding @"text" @"title" (Just identity) (Just identity)
        textBlock "" $ do
          textStyle
          set { row: 2, column: 0 }
          binding @"text" @"key" (Just (\v -> "ID: " <> show v)) Nothing
        textBlock "" $ do -- The comments
          textStyle
          set
            { row: 3
            , column: 0
            , columnSpan: 5
            , font: "italic 9pt sans-serif"
            , wrap: WrapFit
            , editable: true
            , minSize: SizeEach { w: 100.0, h: 14.0 }
            }
          binding @"text" @"comments" (Just identity) (Just identity)
    -- end Table Panel
    -- end Horizontal Panel
    -- end Auto Panel
    button @Basic' @Auto' $ do
      shape PlusLine $ do
        set { width: 10.0, height: 10.0 }
      set
        { name: "BUTTON"
        , alignment: Spot.right
        , opacity: 0.0
        , click: \e but -> addEmployee (e # InputEvent._diagram) (but # _part)
        }
      bindingOfObject @"opacity" "isSelected" (Just $ \s -> if s then 1.0 else 0.0) Nothing
  binding @"isTreeExpanded" @"isTreeExpanded" (Just identity) (Just identity)
  button @TreeExpander' @Auto' $ do
    set
      { name: "BUTTONX"
      , alignment: Spot.bottom
      , opacity: 0.0
      , "_treeExpandedFigure": TriangleUp
      , "_treeCollapsedFigure": TriangleDown
      }
    bindingOfObject @"opacity" "isSelected" (Just $ \s -> if s then 1.0 else 0.0) Nothing
  contextMenu $ do
    button @ContextMenu' @Auto' $ do
      textBlock "Add Employee" $ pure unit
      set { click: \e but -> addEmployee (e # InputEvent._diagram) (but # _part) }
    button @ContextMenu' @Auto' $ do
      textBlock "Vacate Position" $ pure unit
      set
        { click: \(e :: InputEvent_ Diagram_) but -> do
            case but # (_part >=> _adornedPart @Node_) of
              Nothing -> pure unit
              Just node -> do
                let
                  thisemp = node # _data
                  diagram = e # InputEvent._diagram
                  model = diagram # _model @(TreeModel_ _)
                void $ startTransaction_ "vacate" diagram
                model # setDataProperty_ thisemp "name" "(Vacant)"
                model # setDataProperty_ thisemp "pic" ""
                model # setDataProperty_ thisemp "comments" ""
                void $ commitTransaction_ "vacate" diagram
        }
    button @ContextMenu' @Auto' $ do
      textBlock "Remove Role" $ pure unit
      set
        { click: \(e :: InputEvent_ Diagram_) but -> do
            case but # (_part >=> _adornedPart @Node_) of
              Nothing -> pure unit
              Just node -> do
                let diagram = e # InputEvent._diagram
                void $ startTransaction_ "reparent remove" diagram
                chl <- findTreeChildrenNodes_ @Node_ node
                findTreeParentNode_ @Node_ node >>= case _ of
                  Just nodeParent -> do
                    for_ chl \ch -> do
                      let
                        d = ch # _data
                        parentKey = (nodeParent # _data).key
                      diagram # _model >>> setParentKeyForNodeData_ d parentKey
                  Nothing -> pure unit
                diagram # _model @(TreeModel_ _) >>> removeNodeData_ (node # _data)
                void $ diagram # commitTransaction_ "reparent remove"
        }
    button @ContextMenu' @Auto' $ do
      textBlock "Remove Department" $ pure unit
      set
        { click: \(e :: InputEvent_ Diagram_) but -> do
            case but # (_part >=> _adornedPart @Node_) of
              Nothing -> pure unit
              Just node -> do
                let diagram = e # InputEvent._diagram
                void $ diagram # startTransaction_ "remove dept"
                findTreeParts_ @Node_ node >>= flip removeParts_ diagram
                void $ diagram # commitTransaction_ "remove dept"
        }
  where
  addEmployee :: Diagram_ -> Maybe Node_ -> Effect Unit
  addEmployee d n = case n of
    Nothing -> pure unit
    Just node -> do
      let thisEmp = node # _data
      _ <- d # startTransaction_ "add employee"
      let newEmp = { name: "(new person)", title: "(title)", comments: "", parent: thisEmp.key }
      d # _model @(TreeModel_ _) >>> addNodeData_ newEmp
      d # findNodeForData_ @Node_ newEmp >>= case _ of
        Just newNode -> do
          setUnsafe node { location: node # _location }
          d # _commandHandler >>> scrollToPart_ newNode
        Nothing -> pure unit
      _ <- d # commitTransaction_ "add employee"
      pure unit

type NodeData =
  ( comments :: String
  , key :: Int
  , name :: String
  , pic :: Maybe String
  , title :: String
  , parent :: Int
  , isTreeExpanded :: Boolean
  )

type LinkData =
  () :: Row Type

nodeDataArray :: Array (Record NodeData)
nodeDataArray =
  [ { "key": 1, comments: "", "name": "Stella Payne Diaz", "title": "CEO", "pic": Just "1.jpg", parent: -1, isTreeExpanded: true }
  , { "key": 2, comments: "", "name": "Luke Warm", "title": "VP Marketing/Sales", "pic": Just "2.jpg", "parent": 1, isTreeExpanded: true }
  , { "key": 3, comments: "", "name": "Meg Meehan Hoffa", "title": "Sales", "pic": Just "3.jpg", "parent": 2, isTreeExpanded: true }
  , { "key": 4, comments: "", "name": "Peggy Flaming", "title": "VP Engineering", "pic": Just "4.jpg", "parent": 1, isTreeExpanded: true }
  , { "key": 5, comments: "", "name": "Saul Wellingood", "title": "Manufacturing", "pic": Just "5.jpg", "parent": 4, isTreeExpanded: true }
  , { "key": 6, comments: "", "name": "Al Ligori", "title": "Marketing", "pic": Just "6.jpg", "parent": 2, isTreeExpanded: true }
  , { "key": 7, comments: "", "name": "Dot Stubadd", "title": "Sales Rep", "pic": Just "7.jpg", "parent": 3, isTreeExpanded: true }
  , { "key": 8, comments: "", "name": "Les Ismore", "title": "Project Mgr", "pic": Just "8.jpg", "parent": 5, isTreeExpanded: true }
  , { "key": 9, comments: "", "name": "April Lynn Parris", "title": "Events Mgr", "pic": Just "9.jpg", "parent": 6, isTreeExpanded: true }
  , { "key": 10, comments: "", "name": "Xavier Breath", "title": "Engineering", "pic": Just "10.jpg", "parent": 4, isTreeExpanded: true }
  , { "key": 11, comments: "", "name": "Anita Hammer", "title": "Process", "pic": Just "11.jpg", "parent": 5, isTreeExpanded: true }
  , { "key": 12, comments: "", "name": "Billy Aiken", "title": "Software", "pic": Just "12.jpg", "parent": 10, isTreeExpanded: true }
  , { "key": 13, comments: "", "name": "Stan Wellback", "title": "Testing", "pic": Just "13.jpg", "parent": 10, isTreeExpanded: true }
  , { "key": 14, comments: "", "name": "Marge Innovera", "title": "Hardware", "pic": Just "14.jpg", "parent": 10, isTreeExpanded: true }
  , { "key": 15, comments: "", "name": "Evan Elpus", "title": "Quality", "pic": Just "15.jpg", "parent": 5, isTreeExpanded: true }
  , { "key": 16, comments: "", "name": "Lotta B. Essen", "title": "Sales Rep", "pic": Just "16.jpg", "parent": 3, isTreeExpanded: true }
  ]

diag :: MakeDiagram NodeData LinkData Diagram_ Unit
diag = do
  attach
    { allowCopy: false
    , allowDelete: false
    --, initialAutoScale: Uniform 
    , maxSelectionCount: 1
    , validCycle: CycleDestinationTree
    , "undoManager.isEnabled": true
    }
  clickCreatingTool $ do
    set
      { archetypeNodeData:
          { name: "(new person)"
          , title: ""
          , comments: ""
          , key: -100
          , parent: 0
          , isTreeExpanded: true
          }
      , insertPart: Override $ \this loc -> do
          toMaybe <$> prototype @"insertPart" this loc >>= case _ of
            Nothing -> pure unit
            Just node -> do
              let diagram = this # Tool._diagram @Diagram_
              diagram # select_ node
              diagram # _commandHandler >>> scrollToPart_ node
              findObject_ "NAMETB" node >>= case _ of
                Just n -> diagram # _commandHandler >>> editTextBlock_ n
                Nothing -> pure unit
      }
  treeLayout $ do
    set
      { treeStyle: StyleLastParents
      , arrangement: ArrangementHorizontal
      , angle: 90.0
      , layerSpacing: 35.0
      , alternateAngle: 90.0
      , alternateLayerSpacing: 35.0
      , alternateAlignment: AlignmentBus
      , alternateNodeSpacing: 20.0
      , commitNodes: Override $ \this -> do
          prototype @"commitNodes" this
          let vertexes = this # _network >>> _vertexes
          for_ vertexes $ \v -> do
            let
              level = fromMaybe 0 (fromNumber (v # _level)) `rem` length levelColors
              color = fromMaybe "" $ levelColors !! level
            case v # _node :: (_ Node_) of
              Just n -> findObject_ @Shape_ "SHAPE" n >>= case _ of
                Just s -> setUnsafe s { stroke: color }
                Nothing -> pure unit
              Nothing -> pure unit
      }
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
      false, (Just _idx) -> setTitle (take (String.length docTitle - 1) docTitle) doc
      _, _ -> pure unit
  addNodeTemplate "" nodeTemplate
  addLinkTemplate "" $ link $ do
    set { routing: Orthogonal, layerName: "Background", corner: 5.0 }
    shape None $
      set { strokeWidth: 1.5, stroke: "#F5F5F5" }
  treeModel $ do
    set { nodeDataArray }


init ∷ Effect Unit
init = do
  Diagram.make "myDiagramDiv" diag
  log "🍝 a"