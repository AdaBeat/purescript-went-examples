module Example.IncrementalTree where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Int (fromNumber, round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Random (randomInt)
import GoJS.Diagram.Methods (commitTransaction_, findNodeForData_, findNodeForKey_, startTransaction_, zoomToFit_)
import GoJS.Diagram.Properties (_model)
import GoJS.Diagram.Types (Diagram_, Node_, Panel_)
import GoJS.GraphObject.Panel.Methods (findObject_)
import GoJS.GraphObject.Panel.Part.Node.Methods (collapseTree_, expandTree_, findTreeParentNode_)
import GoJS.GraphObject.Panel.Part.Node.Properties (_isTreeExpanded)
import GoJS.GraphObject.Panel.Part.Properties (_location)
import GoJS.GraphObject.Panel.Properties (_data)
import GoJS.GraphObject.Properties (_diagram, _part)
import GoJS.Model.Methods (addNodeData_, setDataProperty_)
import GoJS.Model.Properties (_nodeDataArray)
import GoJS.Model.Types (TreeModel_)
import GoJS.Settable (setUnsafe)
import Went.Diagram.Make (MakeDiagram, attach, nodeTemplateMap)
import Went.Diagram.Make as Diagram
import Went.Geometry.Margin (Margin(..))
import Went.Geometry.Spot as Spot
import Went.GraphObject.Make (button, node, panel, shape, textBlock)
import Went.GraphObject.Panel (Auto', Spot', TreeExpander')
import Went.GraphObject.Shape.Figure (Figure(..))
import Went.Layout.Make (forceDirectedLayout)
import Went.Model.Binding (binding)
import Went.Model.Make (treeModel)
import Went.Settable (set)
import Went.Template.Makers (MadeNode)

blues :: Array String
blues = [ "#E1F5FE", "#B3E5FC", "#81D4FA", "#4FC3F7", "#29B6F6", "#03A9F4", "#039BE5", "#0288D1", "#0277BD", "#01579B" ]

separationFromGrandparent :: forall n. Int -> (n -> Effect (Maybe n)) -> Maybe n -> Effect Int
separationFromGrandparent degrees getparent maybeParent = case maybeParent of
  Nothing -> pure degrees
  Just parent -> getparent parent >>= separationFromGrandparent (1 + degrees) getparent

createSubTree diagram parentdata = do
  numchildren <- randomInt 1 10
  let model = diagram # _model @(TreeModel_ _)
  -- TODO: findNodeForData for some reason doesn't work here. needed to use forKey instead
  parent <- diagram # findNodeForKey_ @Node_ parentdata.key >>= case _ of
    Just x -> pure x
    Nothing -> unsafeThrow $ "cannot create subtree"
  rootdistance <- separationFromGrandparent 1 findTreeParentNode_ (Just parent)
  for_ (Array.range 1 numchildren) $ \i -> do -- i is added to the key for consistency
    let
      key = Array.length $ model # _nodeDataArray
      childdata = { key: key - 1 + i, parent: parentdata.key, rootdistance, everExpanded: false }
    model # addNodeData_ childdata
    diagram # findNodeForData_ @Node_ childdata >>= case _ of
      Just child -> setUnsafe child { location: (parent # _location) } -- TODO: there's something weird about the location
      Nothing -> pure unit
  pure numchildren

expandNode n = do
  let diagram = n # _diagram
  void $ diagram # startTransaction_ "CollapseExpandTree"
  let dat = n # _data
  whenM (pure $ not $ dat.everExpanded) do
    -- only create children once per node
    diagram # _model @(TreeModel_ _) >>> setDataProperty_ dat "everExpanded" true
    numchildren <- createSubTree diagram dat
    whenM (pure $ numchildren == 0) $ do
      n # findObject_ @Panel_ "TREEBUTTON" >>= case _ of
        Just tree -> setUnsafe tree { visible: false }
        Nothing -> pure unit
  if n # _isTreeExpanded then
    collapseTree_ n
  else do
    expandTree_ n
  void $ diagram # commitTransaction_ "CollapseExpandTree"
  zoomToFit_ diagram

nodeTemplate :: MadeNode NodeData Node_
nodeTemplate = node @Spot' $ do
  set
    { selectionObjectName: "PANEL"
    , isTreeExpanded: false
    , isTreeLeaf: false
    }
  panel @Auto' $ do
    set { name: "PANEL" }
    shape Circle $ do
      set { fill: "whitesmoke", stroke: "black" }
      binding @"fill" @"rootdistance"
        ( Just $ \dist ->
            let
              index = fromMaybe 0 $ fromNumber (min dist $ toNumber (Array.length blues - 1))
            in
              fromMaybe "" $ blues Array.!! index
        )
        Nothing
    textBlock "" $ do
      set { font: "12pt sans-serif", margin: MarginAll 5.0 }
      binding @"text" @"key" (Just $ (show <<< round)) Nothing
  button @TreeExpander' @Auto' $ do
    set
      { name: "TREEBUTTON"
      , width: 20.0
      , height: 20.0
      , alignment: Spot.topRight
      , alignmentFocus: Spot.center
      , click: \e obj -> do
          case obj # _part @Node_ of
            Just n -> do
              setUnsafe e { handled: true }
              expandNode n
            Nothing -> pure unit
      }

type NodeData =
  ( parent :: Int
  , key :: Int
  , color :: String
  , everExpanded :: Boolean
  , rootdistance :: Int
  )

nodeData :: Array (Record NodeData)
nodeData = [ { parent: 0, key: 0, color: "#E1F5FE", everExpanded: false, rootdistance: 0 } ]

diag :: forall linkData. MakeDiagram NodeData linkData Diagram_ Unit
diag = do
  attach
    { initialContentAlignment: Spot.center
    , "commandHandler.copiesTree": true
    , "commandHandler.deletesTree": true
    , "draggingTool.dragsTree": true
    , "undoManager.isEnabled": true
    }
  nodeTemplateMap {"": nodeTemplate}
  forceDirectedLayout $ pure unit
  treeModel $ do
    set { nodeDataArray: nodeData }

init ∷ Effect Unit
init = do
  Diagram.make "myDiagramDiv" diag
  Console.log "🍝 a"