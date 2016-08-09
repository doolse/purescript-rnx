module RNX.Props where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (mkFn3, mkFn4)
import RNX.Components (ListViewDataSource)
import React (ReactElement, Event, EventHandlerContext, handle)
import React.DOM.Props (Props, unsafeMkProps)
import Data.Maybe (Maybe)

type ViewProps = {
  accessibilityLabel :: String,
  accessible :: Boolean,
  hitSlop :: {top :: Int, left :: Int, bottom :: Int, right :: Int},
  onAccessibilityTap :: (Unit -> Unit), -- TODO: Check the function
  onLayout :: (Unit-> Unit),
  onMagicTap :: (Unit -> Unit),
  onMoveShouldSetResponder :: Maybe (Unit -> Unit),
  onMoveShouldSetResponderCapture :: Maybe (Unit -> Unit)
}

type ActivityIndicatorProps = {
  animating :: Boolean,
  color :: String, -- COLOR
  size :: String, -- SIZE enum
  hidesWhenStopped :: Boolean
}

activityIndicatorProps :: ActivityIndicatorProps
activityIndicatorProps = {
  animating : true,
  color : "gray",
  size : "small",
  hidesWhenStopped : true
}



type RenderRowFn = forall rowData highlightFn. rowData -> String -> String -> highlightFn -> ReactElement
type RenderSeparatorFn = String -> String -> Boolean -> ReactElement
type RenderHeaderFn = forall eff. Eff eff ReactElement
type OnPressFn = forall eff. Eff eff Unit

foreign import unitFn :: forall a. a -> (Unit -> a)

renderRow :: forall rowData highlightFn. (rowData -> String -> String -> highlightFn -> ReactElement) -> Props
renderRow = unsafeMkProps "renderRow" <<< mkFn4

renderSeparator :: RenderSeparatorFn -> Props
renderSeparator fun = unsafeMkProps "renderSeparator" (mkFn3 fun)

renderHeader :: ReactElement -> Props
renderHeader elem = unsafeMkProps "renderHeader" (unitFn elem)

renderFooter :: ReactElement -> Props
renderFooter elem = unsafeMkProps "renderFooter" (unitFn elem)

dataSource :: ListViewDataSource -> Props
dataSource = unsafeMkProps "dataSource"

onPress :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onPress f = unsafeMkProps "onPress" (handle f)

onPressIn :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onPressIn f = unsafeMkProps "onPressIn" (handle f)

onPressOut :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onPressOut f = unsafeMkProps "onPressOut" (handle f)

onSubmitEditing :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onSubmitEditing f = unsafeMkProps "onSubmitEditing" (handle f)

onChangeText :: forall eff props state result. (String -> EventHandlerContext eff props state result) -> Props
onChangeText f = unsafeMkProps "onChangeText" (handle f)

foreign import data NativeFeedbackBackground :: *
foreign import selectableBackground :: NativeFeedbackBackground
foreign import selectableBackgroundBorderless :: NativeFeedbackBackground

background :: NativeFeedbackBackground -> Props
background = unsafeMkProps "background"