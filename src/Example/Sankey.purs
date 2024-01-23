module Example.Sankey where

import Prelude

import Data.Foldable (findMap, foldM, for_, length)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Number (round)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception.Unsafe (unsafeThrow)
import GoJS.Diagram.Properties (_nodes)
import GoJS.Diagram.Types (Diagram_)
import GoJS.EnumValue (enumValueBuilder_)
import GoJS.Geometry.Rect.Properties as Rect
import GoJS.GraphObject.Panel.Methods (findObject_)
import GoJS.GraphObject.Panel.Part.Link.Methods (computeThickness_, invalidateRoute_)
import GoJS.GraphObject.Panel.Part.Link.Properties (_curve)
import GoJS.GraphObject.Panel.Part.Methods (ensureBounds_)
import GoJS.GraphObject.Panel.Part.Node.Methods (findLinksInto_, findLinksOutOf_)
import GoJS.GraphObject.Properties (_actualBounds)
import GoJS.GraphObject.Types (class IsNode, Node_, Shape_, TextBlock_)
import GoJS.Key (KeyProperty(..))
import GoJS.Layout (LayeredDigraphLayout_, LayeredDigraphNetwork_, LayeredDigraphVertex_)
import GoJS.Layout.LayeredDigraphLayout.Properties (_columnSpacing, _maxLayer)
import GoJS.Layout.LayoutEdge.Properties (_link)
import GoJS.Layout.LayoutNetwork.Methods (findVertex_)
import GoJS.Layout.LayoutNetwork.Properties (_vertexes)
import GoJS.Layout.LayoutNetwork.Properties as Network
import GoJS.Layout.LayoutVertex.Properties (_destinationVertexes, _edges, _edgesCount, _height, _node, _sourceVertexes)
import GoJS.Layout.Properties (_diagram, _network)
import GoJS.Prototype (prototype)
import GoJS.Unsafe.Set (setUnsafe)
import Went.Diagram.EnumValue.AutoScale (AutoScale(..))
import Went.Diagram.Make (MakeDiagram, addLinkTemplate, addNodeTemplate, attach)
import Went.Diagram.Make as Diagram
import Went.FFI.Override (Override(..))
import Went.Geometry (Margin(..), Spot(..), left)
import Went.GraphObject (Adjusting(End), Curve(..), Horizontal', Link', MadeGraphObject, PortSpreading(..), adornment, link, node, selectionAdornmentTemplate, shape, textBlock)
import Went.GraphObject.Shape.Arrowhead (Arrowhead(..))
import Went.GraphObject.Shape.Figure as Figure
import Went.Layout (LayeringOption(..), alignAll, layeredDigraphLayout, packMedian, packStraighten)
import Went.Model (binding, binding1, graphLinksModel)
import Went.Settable (set)

getAutoHeightForNode :: forall n. IsNode n => n -> Effect Number
getAutoHeightForNode node = do
  linksInto <- findLinksInto_ node
  heightIn <- foldM (\acc l -> (acc + _) <$> computeThickness_ l) 0.0 linksInto
  linksOutOf <- findLinksOutOf_ node
  heightOut <- foldM (\acc l -> (acc + _) <$> computeThickness_ l) 0.0 linksOutOf
  pure <<< max 10.0 $ max heightIn heightOut

makeNetwork :: Override LayeredDigraphLayout_ (Diagram_ -> Effect LayeredDigraphNetwork_)
makeNetwork = Override $ \this coll -> do
  net <- prototype @"makeNetwork" this coll
  let nodes = this # _diagram >>> fromMaybe' (\_ -> unsafeThrow "no diagram can't make network") >>> _nodes
  for_ nodes \n -> do
    -- figure out how tall the node's bar should be
    height <- getAutoHeightForNode n
    findObject_ @Shape_ "SHAPE" n >>= case _ of
      Just shape -> setUnsafe shape { height: height }
      Nothing -> pure unit
    let font = "bold " <> show (max 12.0 (round $ height / 8.0) # fromNumber >>> fromMaybe 5) <> "pt Segoe UI, sans-serif"
    findObject_ @TextBlock_ "TEXT" n >>= case _ of
      Just text -> setUnsafe text { font: font }
      Nothing -> pure unit
    findObject_ @TextBlock_ "LTEXT" n >>= case _ of
      Just text -> setUnsafe text { font: font }
      Nothing -> pure unit
    -- and update the vertex's dimensions accordingly
    findVertex_ n net >>= case _ of
      Just v -> do
        ensureBounds_ n
        let r = n # _actualBounds
        setUnsafe v { width: r # Rect._width }
        setUnsafe v { height: r # Rect._height }
        setUnsafe v { focusY: (v # _height) / 2.0 }
      Nothing -> pure unit
  pure net

nodeMinColumnSpace :: Override LayeredDigraphLayout_ (LayeredDigraphVertex_ -> Boolean -> Effect Number)
nodeMinColumnSpace = Override $ \this layoutVertex topleft -> do
  rest <- case (layoutVertex # _node @Node_) of
    Just _ -> do
      if (layoutVertex # _edgesCount) >= 1.0 then do
        let edges = layoutVertex # _edges
        maxBasedOnThickness <- case findMap _link edges of
          Just l -> max 1.0 <$> computeThickness_ l
          Nothing -> pure 1.0
        pure $ max 2.0 $ maxBasedOnThickness / (this # _columnSpacing)
      else do
        pure 1.0
    Nothing -> do
      prototype @"nodeMinColumnSpace" this layoutVertex topleft
  pure rest

nodeMinLayerSpace :: Override LayeredDigraphLayout_ (LayeredDigraphVertex_ -> Boolean -> Effect Number)
nodeMinLayerSpace = Override $ \this layoutVertex topleft -> do
  case (layoutVertex # _node @Node_) of
    Nothing -> pure 100.0
    Just _ -> prototype @"nodeMinLayerSpace" this layoutVertex topleft

assignLayers :: Override LayeredDigraphLayout_ (Effect Unit)
assignLayers = Override $ \this -> do
  prototype @"assignLayers" this
  let maxLayer = this # _maxLayer
  for_ (this # _network >>> _vertexes) $ \v -> do
    if (length (v # _destinationVertexes) == 0) then
      setUnsafe v { layer: 0.0 }
    else pure unit
    if (length (v # _sourceVertexes) == 0) then
      setUnsafe v { layer: maxLayer }
    else pure unit

commitLayout :: Override LayeredDigraphLayout_ (Effect Unit)
commitLayout = Override $ \this -> do
  prototype @"commitLayout" this
  for_ (this # _network >>> Network._edges) \edge -> do
    case edge # _link of
      Just l ->
        if (l # _curve) == enumValueBuilder_ "Link" "Bezier" then
          invalidateRoute_ l
        else
          pure unit
      Nothing -> pure unit

sankeyLayout = layeredDigraphLayout $ do
  set
    { makeNetwork: makeNetwork
    , nodeMinColumnSpace: nodeMinColumnSpace
    , nodeMinLayerSpace: nodeMinLayerSpace
    , commitLayout: commitLayout
    , assignLayers: assignLayers
    , alignOption: alignAll
    , packOption: packStraighten <> packMedian
    , setsPortSpots: false
    , direction: 0.0
    , layeringOption: LayerOptimalLinkLength
    , layerSpacing: 100.0
    , columnSpacing: 1.0
    }

textStyle =
  set { font: "bold 12pt Segoe UI, sans-serif", stroke: "black", margin: MarginAll 5.0 }

nodeTemplate :: MadeGraphObject NodeData Node_ Node_
nodeTemplate = node @Horizontal' $ do
  set
    { locationObjectName: "SHAPE"
    , locationSpot: left
    , portSpreading: SpreadingPacked
    }
  textBlock "" $ do
    textStyle
    set { name: "LTEXT" }
    binding @"text" @"ltext" Nothing Nothing
  shape Figure.None $ do
    set
      { name: "SHAPE"
      , fill: "#2E8DEF"
      , strokeWidth: 0.0
      , portId: ""
      , fromSpot: RightSide
      , toSpot: LeftSide
      , height: 10.0
      , width: 20.0
      }
    binding @"fill" @"color" Nothing Nothing
  textBlock "" $ do
    textStyle
    set { name: "TEXT" }
    binding1 @"text"

linkTemplate = link $ do
  set
    {
      curve: Bezier
    , layerName: "Background"
    , fromEndSegmentLength: 150.0
    , toEndSegmentLength: 150.0
    , adjusting: End
    }
  selectionAdornmentTemplate $ do
    adornment @Link' $ do 
      shape Figure.None $ do
        set { isPanelMain: true, fill: "transparent", stroke: "rgba(0, 0, 255, 0.3)", strokeWidth: 0.0 }
  shape Figure.None $ do
    set { strokeWidth: 4.0, stroke: "rgba(173, 173, 173, 0.25)", segmentFraction: 0.4, toArrow: Feather }
    binding @"stroke" @"ignored" (Just getAutoLinkColor) Nothing
    binding @"strokeWidth" @"width" Nothing Nothing
  where
  getAutoLinkColor _ = "rgba(173, 173, 173, 0.25)"

type NodeData =
  ( key :: String
  , ltext :: String
  , text :: String
  , color :: String
  )

type LinkData =
  ( from :: String
  , to :: String
  , width :: Number
  , ignored :: String
  )

nodeDataArray :: Array (Record NodeData)
nodeDataArray =
  [ { key: "Coal reserves", ltext: "", text: "Coal reserves", color: "#9d75c2" }
  , { key: "Coal imports", ltext: "", text: "Coal imports", color: "#9d75c2" }
  , { key: "Oil reserves", ltext: "", text: "Oil\nreserves", color: "#9d75c2" }
  , { key: "Oil imports", ltext: "", text: "Oil imports", color: "#9d75c2" }
  , { key: "Gas reserves", ltext: "", text: "Gas reserves", color: "#a1e194" }
  , { key: "Gas imports", ltext: "", text: "Gas imports", color: "#a1e194" }
  , { key: "UK land based bioenergy", ltext: "", text: "UK land based bioenergy", color: "#f6bcd5" }
  , { key: "Marine algae", ltext: "", text: "Marine algae", color: "#681313" }
  , { key: "Agricultural 'waste'", ltext: "", text: "Agricultural 'waste'", color: "#3483ba" }
  , { key: "Other waste", ltext: "", text: "Other waste", color: "#c9b7d8" }
  , { key: "Biomass imports", ltext: "", text: "Biomass imports", color: "#fea19f" }
  , { key: "Biofuel imports", ltext: "", text: "Biofuel imports", color: "#d93c3c" }
  , { key: "Coal", ltext: "", text: "Coal", color: "#9d75c2" }
  , { key: "Oil", ltext: "", text: "Oil", color: "#9d75c2" }
  , { key: "Natural gas", ltext: "", text: "Natural\ngas", color: "#a6dce6" }
  , { key: "Solar", ltext: "", text: "Solar", color: "#c9a59d" }
  , { key: "Solar PV", ltext: "", text: "Solar PV", color: "#c9a59d" }
  , { key: "Bio-conversion", ltext: "", text: "Bio-conversion", color: "#b5cbe9" }
  , { key: "Solid", ltext: "", text: "Solid", color: "#40a840" }
  , { key: "Liquid", ltext: "", text: "Liquid", color: "#fe8b25" }
  , { key: "Gas", ltext: "", text: "Gas", color: "#a1e194" }
  , { key: "Nuclear", ltext: "", text: "Nuclear", color: "#fea19f" }
  , { key: "Thermal generation", ltext: "", text: "Thermal\ngeneration", color: "#3483ba" }
  , { key: "CHP", ltext: "", text: "CHP", color: "yellow" }
  , { key: "Electricity imports", ltext: "", text: "Electricity imports", color: "yellow" }
  , { key: "Wind", ltext: "", text: "Wind", color: "#cbcbcb" }
  , { key: "Tidal", ltext: "", text: "Tidal", color: "#6f3a5f" }
  , { key: "Wave", ltext: "", text: "Wave", color: "#8b8b8b" }
  , { key: "Geothermal", ltext: "", text: "Geothermal", color: "#556171" }
  , { key: "Hydro", ltext: "", text: "Hydro", color: "#7c3e06" }
  , { key: "Electricity grid", ltext: "", text: "Electricity grid", color: "#e483c7" }
  , { key: "H2 conversion", ltext: "", text: "H2 conversion", color: "#868686" }
  , { key: "Solar Thermal", ltext: "", text: "Solar Thermal", color: "#c9a59d" }
  , { key: "H2", ltext: "", text: "H2", color: "#868686" }
  , { key: "Pumped heat", ltext: "", text: "Pumped heat", color: "#96665c" }
  , { key: "District heating", ltext: "", text: "District heating", color: "#c9b7d8" }
  , { key: "Losses", text: "", ltext: "Losses", color: "#fec184" }
  , { key: "Over generation / exports", text: "", ltext: "Over generation / exports", color: "#f6bcd5" }
  , { key: "Heating and cooling - homes", text: "", ltext: "Heating and cooling - homes", color: "#c7a39b" }
  , { key: "Road transport", text: "", ltext: "Road transport", color: "#cbcbcb" }
  , { key: "Heating and cooling - commercial", text: "", ltext: "Heating and cooling - commercial", color: "#c9a59d" }
  , { key: "Industry", text: "", ltext: "Industry", color: "#96665c" }
  , { key: "Lighting &amp; appliances - homes", text: "", ltext: "Lighting &amp; appliances - homes", color: "#2dc3d2" }
  , { key: "Lighting &amp; appliances - commercial", text: "", ltext: "Lighting &amp; appliances - commercial", color: "#2dc3d2" }
  , { key: "Agriculture", text: "", ltext: "Agriculture", color: "#5c5c10" }
  , { key: "Rail transport", text: "", ltext: "Rail transport", color: "#6b6b45" }
  , { key: "Domestic aviation", text: "", ltext: "Domestic aviation", color: "#40a840" }
  , { key: "National navigation", text: "", ltext: "National navigation", color: "#a1e194" }
  , { key: "International aviation", text: "", ltext: "International aviation", color: "#fec184" }
  , { key: "International shipping", text: "", ltext: "International shipping", color: "#fec184" }
  , { key: "Geosequestration", text: "", ltext: "Geosequestration", color: "#fec184" }
  ]

linkDataArray :: Array (Record LinkData)
linkDataArray =
  [ { from: "Coal reserves", to: "Coal", width: 31.0, ignored: "" }
  , { from: "Coal imports", to: "Coal", width: 86.0, ignored: "" }
  , { from: "Oil reserves", to: "Oil", width: 244.0, ignored: "" }
  , { from: "Oil imports", to: "Oil", width: 1.0, ignored: "" }
  , { from: "Gas reserves", to: "Natural gas", width: 182.0, ignored: "" }
  , { from: "Gas imports", to: "Natural gas", width: 61.0, ignored: "" }
  , { from: "UK land based bioenergy", to: "Bio-conversion", width: 1.0, ignored: "" }
  , { from: "Marine algae", to: "Bio-conversion", width: 1.0, ignored: "" }
  , { from: "Agricultural 'waste'", to: "Bio-conversion", width: 1.0, ignored: "" }
  , { from: "Other waste", to: "Bio-conversion", width: 8.0, ignored: "" }
  , { from: "Other waste", to: "Solid", width: 1.0, ignored: "" }
  , { from: "Biomass imports", to: "Solid", width: 1.0, ignored: "" }
  , { from: "Biofuel imports", to: "Liquid", width: 1.0, ignored: "" }
  , { from: "Coal", to: "Solid", width: 117.0, ignored: "" }
  , { from: "Oil", to: "Liquid", width: 244.0, ignored: "" }
  , { from: "Natural gas", to: "Gas", width: 244.0, ignored: "" }
  , { from: "Solar", to: "Solar PV", width: 1.0, ignored: "" }
  , { from: "Solar PV", to: "Electricity grid", width: 1.0, ignored: "" }
  , { from: "Solar", to: "Solar Thermal", width: 1.0, ignored: "" }
  , { from: "Bio-conversion", to: "Solid", width: 3.0, ignored: "" }
  , { from: "Bio-conversion", to: "Liquid", width: 1.0, ignored: "" }
  , { from: "Bio-conversion", to: "Gas", width: 5.0, ignored: "" }
  , { from: "Bio-conversion", to: "Losses", width: 1.0, ignored: "" }
  , { from: "Solid", to: "Over generation / exports", width: 1.0, ignored: "" }
  , { from: "Liquid", to: "Over generation / exports", width: 18.0, ignored: "" }
  , { from: "Gas", to: "Over generation / exports", width: 1.0, ignored: "" }
  , { from: "Solid", to: "Thermal generation", width: 106.0, ignored: "" }
  , { from: "Liquid", to: "Thermal generation", width: 2.0, ignored: "" }
  , { from: "Gas", to: "Thermal generation", width: 87.0, ignored: "" }
  , { from: "Nuclear", to: "Thermal generation", width: 41.0, ignored: "" }
  , { from: "Thermal generation", to: "District heating", width: 2.0, ignored: "" }
  , { from: "Thermal generation", to: "Electricity grid", width: 92.0, ignored: "" }
  , { from: "Thermal generation", to: "Losses", width: 142.0, ignored: "" }
  , { from: "Solid", to: "CHP", width: 1.0, ignored: "" }
  , { from: "Liquid", to: "CHP", width: 1.0, ignored: "" }
  , { from: "Gas", to: "CHP", width: 1.0, ignored: "" }
  , { from: "CHP", to: "Electricity grid", width: 1.0, ignored: "" }
  , { from: "CHP", to: "Losses", width: 1.0, ignored: "" }
  , { from: "Electricity imports", to: "Electricity grid", width: 1.0, ignored: "" }
  , { from: "Wind", to: "Electricity grid", width: 1.0, ignored: "" }
  , { from: "Tidal", to: "Electricity grid", width: 1.0, ignored: "" }
  , { from: "Wave", to: "Electricity grid", width: 1.0, ignored: "" }
  , { from: "Geothermal", to: "Electricity grid", width: 1.0, ignored: "" }
  , { from: "Hydro", to: "Electricity grid", width: 1.0, ignored: "" }
  , { from: "Electricity grid", to: "H2 conversion", width: 1.0, ignored: "" }
  , { from: "Electricity grid", to: "Over generation / exports", width: 1.0, ignored: "" }
  , { from: "Electricity grid", to: "Losses", width: 6.0, ignored: "" }
  , { from: "Gas", to: "H2 conversion", width: 1.0, ignored: "" }
  , { from: "H2 conversion", to: "H2", width: 1.0, ignored: "" }
  , { from: "H2 conversion", to: "Losses", width: 1.0, ignored: "" }
  , { from: "Solar Thermal", to: "Heating and cooling - homes", width: 1.0, ignored: "" }
  , { from: "H2", to: "Road transport", width: 1.0, ignored: "" }
  , { from: "Pumped heat", to: "Heating and cooling - homes", width: 1.0, ignored: "" }
  , { from: "Pumped heat", to: "Heating and cooling - commercial", width: 1.0, ignored: "" }
  , { from: "CHP", to: "Heating and cooling - homes", width: 1.0, ignored: "" }
  , { from: "CHP", to: "Heating and cooling - commercial", width: 1.0, ignored: "" }
  , { from: "District heating", to: "Heating and cooling - homes", width: 1.0, ignored: "" }
  , { from: "District heating", to: "Heating and cooling - commercial", width: 1.0, ignored: "" }
  , { from: "District heating", to: "Industry", width: 2.0, ignored: "" }
  , { from: "Electricity grid", to: "Heating and cooling - homes", width: 7.0, ignored: "" }
  , { from: "Solid", to: "Heating and cooling - homes", width: 3.0, ignored: "" }
  , { from: "Liquid", to: "Heating and cooling - homes", width: 3.0, ignored: "" }
  , { from: "Gas", to: "Heating and cooling - homes", width: 81.0, ignored: "" }
  , { from: "Electricity grid", to: "Heating and cooling - commercial", width: 7.0, ignored: "" }
  , { from: "Solid", to: "Heating and cooling - commercial", width: 1.0, ignored: "" }
  , { from: "Liquid", to: "Heating and cooling - commercial", width: 2.0, ignored: "" }
  , { from: "Gas", to: "Heating and cooling - commercial", width: 19.0, ignored: "" }
  , { from: "Electricity grid", to: "Lighting &amp; appliances - homes", width: 21.0, ignored: "" }
  , { from: "Gas", to: "Lighting &amp; appliances - homes", width: 2.0, ignored: "" }
  , { from: "Electricity grid", to: "Lighting &amp; appliances - commercial", width: 18.0, ignored: "" }
  , { from: "Gas", to: "Lighting &amp; appliances - commercial", width: 2.0, ignored: "" }
  , { from: "Electricity grid", to: "Industry", width: 30.0, ignored: "" }
  , { from: "Solid", to: "Industry", width: 13.0, ignored: "" }
  , { from: "Liquid", to: "Industry", width: 34.0, ignored: "" }
  , { from: "Gas", to: "Industry", width: 54.0, ignored: "" }
  , { from: "Electricity grid", to: "Agriculture", width: 1.0, ignored: "" }
  , { from: "Solid", to: "Agriculture", width: 1.0, ignored: "" }
  , { from: "Liquid", to: "Agriculture", width: 1.0, ignored: "" }
  , { from: "Gas", to: "Agriculture", width: 1.0, ignored: "" }
  , { from: "Electricity grid", to: "Road transport", width: 1.0, ignored: "" }
  , { from: "Liquid", to: "Road transport", width: 122.0, ignored: "" }
  , { from: "Electricity grid", to: "Rail transport", width: 2.0, ignored: "" }
  , { from: "Liquid", to: "Rail transport", width: 1.0, ignored: "" }
  , { from: "Liquid", to: "Domestic aviation", width: 2.0, ignored: "" }
  , { from: "Liquid", to: "National navigation", width: 4.0, ignored: "" }
  , { from: "Liquid", to: "International aviation", width: 38.0, ignored: "" }
  , { from: "Liquid", to: "International shipping", width: 13.0, ignored: "" }
  , { from: "Electricity grid", to: "Geosequestration", width: 1.0, ignored: "" }
  , { from: "Gas", to: "Losses", width: 2.0, ignored: "" }
  ]

diag :: MakeDiagram NodeData LinkData Diagram_ Unit
diag = do
  attach
    { initialAutoScale: UniformToFill
    , "animationManager.isEnabled": false
    }
  addNodeTemplate "" nodeTemplate
  addLinkTemplate "" linkTemplate
  sankeyLayout
  graphLinksModel $ do
    set { nodeDataArray, linkDataArray, linkFromPortIdProperty: Property "", linkToPortIdProperty: Property "" }

init ∷ Effect Unit
init = do
  Diagram.make "myDiagramDiv" diag
  log "🍝 b"