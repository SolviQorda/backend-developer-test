module Halogen.VDom.Driver
  ( runUI
  , module Halogen.Aff.Driver
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Halogen.Aff.Driver (HalogenIO)
import Halogen.Aff.Driver as AD
import Halogen.Aff.Driver.State (RenderStateX, unRenderStateX)
import Halogen.Component (Component, ComponentSlot(..), ComponentSlotBox)
import Halogen.HTML.Core (HTML(..), Prop)
import Halogen.Query.Input (Input)
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as VP
import Halogen.VDom.Thunk (Thunk)
import Halogen.VDom.Thunk as Thunk
import Unsafe.Reference (unsafeRefEq)
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Node (Node, appendChild, removeChild, parentNode, nextSibling, insertBefore) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (HTMLElement) as DOM
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document) as DOM

type VHTML act ps =
  V.VDom (Array (Prop (Input act))) (ComponentSlot HTML ps Aff act)

type ChildRenderer act ps
  = ComponentSlotBox HTML ps Aff act -> Effect (RenderStateX RenderState)

newtype RenderState s act ps o =
  RenderState
    { node :: DOM.Node
    , machine :: V.Step (VHTML act ps) DOM.Node
    , renderChildRef :: Ref (ChildRenderer act ps)
    }

type HTMLThunk ps act =
  Thunk (HTML (ComponentSlot HTML ps Aff act)) act

type WidgetState ps act =
  Maybe (V.Step (HTMLThunk ps act) DOM.Node)

mkSpec
  :: forall act ps
   . (Input act -> Effect Unit)
  -> Ref (ChildRenderer act ps)
  -> DOM.Document
  -> V.VDomSpec
      (Array (VP.Prop (Input act)))
      (ComponentSlot HTML ps Aff act)
mkSpec handler renderChildRef document =
  V.VDomSpec { buildWidget, buildAttributes, document }
  where

  buildAttributes
    :: DOM.Element
    -> V.Machine (Array (VP.Prop (Input act))) Unit
  buildAttributes = VP.buildProp handler

  buildWidget
    :: V.VDomSpec
          (Array (VP.Prop (Input act)))
          (ComponentSlot HTML ps Aff act)
    -> V.Machine
          (ComponentSlot HTML ps Aff act)
          DOM.Node
  buildWidget spec = render
    where

    render :: V.Machine (ComponentSlot HTML ps Aff act) DOM.Node
    render = EFn.mkEffectFn1 \slot ->
      case slot of
        ComponentSlot cs ->
          EFn.runEffectFn1 renderComponentSlot cs
        ThunkSlot t -> do
          step <- EFn.runEffectFn1 buildThunk t
          pure $ V.mkStep $ V.Step (V.extract step) (Just step) patch done

    patch
      :: EFn.EffectFn2 (WidgetState ps act)
            (ComponentSlot HTML ps Aff act)
            (V.Step (ComponentSlot HTML ps Aff act) DOM.Node)
    patch = EFn.mkEffectFn2 \st slot ->
      case st of
        Just step -> case slot of
          ComponentSlot cs -> do
            EFn.runEffectFn1 V.halt step
            EFn.runEffectFn1 renderComponentSlot cs
          ThunkSlot t -> do
            step' <- EFn.runEffectFn2 V.step step t
            pure $ V.mkStep $ V.Step (V.extract step') (Just step') patch done
        _ -> EFn.runEffectFn1 render slot

    buildThunk :: V.Machine (HTMLThunk ps act) DOM.Node
    buildThunk = Thunk.buildThunk unwrap spec

    renderComponentSlot
      :: EFn.EffectFn1
            (ComponentSlotBox HTML ps Aff act)
            (V.Step (ComponentSlot HTML ps Aff act) DOM.Node)
    renderComponentSlot = EFn.mkEffectFn1 \cs -> do
      renderChild <- Ref.read renderChildRef
      rsx <- renderChild cs
      let node = getNode rsx
      pure $ V.mkStep $ V.Step node Nothing patch done

  done :: EFn.EffectFn1 (WidgetState ps act) Unit
  done = EFn.mkEffectFn1 \st ->
    case st of
      Just step -> EFn.runEffectFn1 V.halt step
      _ -> pure unit

  getNode :: RenderStateX RenderState -> DOM.Node
  getNode = unRenderStateX (\(RenderState { node }) -> node)

runUI
  :: forall f i o
   . Component HTML f i o Aff
  -> i
  -> DOM.HTMLElement
  -> Aff (HalogenIO f o Aff)
runUI component i element = do
  document <- liftEffect $ HTMLDocument.toDocument <$> (DOM.document =<< DOM.window)
  AD.runUI (renderSpec document element) component i

renderSpec
  :: DOM.Document
  -> DOM.HTMLElement
  -> AD.RenderSpec HTML RenderState
renderSpec document container =
    { render
    , renderChild: identity
    , removeChild
    , dispose: removeChild
    }
  where

  render
    :: forall s act ps o
     . (Input act -> Effect Unit)
    -> (ComponentSlotBox HTML ps Aff act -> Effect (RenderStateX RenderState))
    -> HTML (ComponentSlot HTML ps Aff act) act
    -> Maybe (RenderState s act ps o)
    -> Effect (RenderState s act ps o)
  render handler child (HTML vdom) =
    case _ of
      Nothing -> do
        renderChildRef <- Ref.new child
        let spec = mkSpec handler renderChildRef document
        machine <- EFn.runEffectFn1 (V.buildVDom spec) vdom
        let node = V.extract machine
        void $ DOM.appendChild node (HTMLElement.toNode container)
        pure $ RenderState { machine, node, renderChildRef }
      Just (RenderState { machine, node, renderChildRef }) -> do
        Ref.write child renderChildRef
        parent <- DOM.parentNode node
        nextSib <- DOM.nextSibling node
        machine' <- EFn.runEffectFn2 V.step machine vdom
        let newNode = V.extract machine'
        when (not unsafeRefEq node newNode) do
          substInParent newNode nextSib parent
        pure $ RenderState { machine: machine', node: newNode, renderChildRef }

removeChild :: forall s act ps o. RenderState s act ps o -> Effect Unit
removeChild (RenderState { node }) = do
  npn <- DOM.parentNode node
  traverse_ (\pn -> DOM.removeChild node pn) npn

substInParent :: DOM.Node -> Maybe DOM.Node -> Maybe DOM.Node -> Effect Unit
substInParent newNode (Just sib) (Just pn) = void $ DOM.insertBefore newNode sib pn
substInParent newNode Nothing (Just pn) = void $ DOM.appendChild newNode pn
substInParent _ _ _ = pure unit
