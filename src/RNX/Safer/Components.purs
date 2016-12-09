module RNX.Safer.Components where

import Prelude
import RNX.Components as U
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Function.Uncurried (mkFn3)
import Data.Newtype (class Newtype)
import RNX.Components (createElementOneChild, textElem)
import RNX.Props (ImageSource)
import RNX.Safer.Styles (Styles)
import RNX.Safer.Undefinable (mapUndefined)
import React (ReactElement, createElement)
import Unsafe.Coerce (unsafeCoerce)

foreign import data ListViewDataSource :: * -> *

foreign import listViewDataSource :: forall a. (Eq a) => Array a -> ListViewDataSource a
foreign import cloneWithRows :: forall a. ListViewDataSource a -> Array a -> ListViewDataSource a

type Props a = a -> a
type Props_ a = a Unit -> a Unit

class HandledEvent a where
  handle :: a -> Unit

handleUndefined :: forall ev a. (HandledEvent ev) => (a -> ev) -> a -> Unit
handleUndefined = mapUndefined (compose handle)

instance unitHandler :: HandledEvent Unit where
  handle = id

instance affHandler :: HandledEvent (Aff eff Unit) where
  handle = unsafePerformEff <<< void <<< launchAff

type ReactBase r = { key :: String | r }

type StyleBase r ev = ReactBase (style :: Styles | r)

styleOnly :: forall r. Styles -> Props (StyleBase r Unit)
styleOnly s = _ {style=s}

newtype RefreshControl = RefreshControl ReactElement

derive instance refreshNT :: Newtype RefreshControl _

type RefreshControlProps r ev = (
  onRefresh :: Unit -> ev
, refreshing :: Boolean | r
)

refreshControl' :: forall ev. HandledEvent ev => { | RefreshControlProps () ev } -> RefreshControl
refreshControl' p = RefreshControl $ createElement U.refreshControlClass (fixProps p) []
  where fixProps pr = pr {onRefresh = handleUndefined pr.onRefresh }

type ListViewProps ev sid rid = {
    initialListSize ::Int
  , renderSeparator :: sid -> rid -> Boolean -> ReactElement
  , enableEmptySections :: Boolean
  , onEndReached :: Unit -> ev
  , refreshControl :: RefreshControl
}

listView :: forall a. ListViewDataSource a -> (a -> ReactElement) ->  ReactElement
listView = listView' (id :: forall sid rid. ListViewProps Unit sid rid -> ListViewProps Unit sid rid)

listView' :: forall ev a sid rid. HandledEvent ev => Props (ListViewProps ev sid rid) -> ListViewDataSource a -> (a -> ReactElement) ->  ReactElement
listView' p dataSource renderRow = createElement U.listViewClass (fixProps $ p $ unsafeCoerce {dataSource, renderRow, enableEmptySections: true}) []
  where
  fixProps props = props {
      renderSeparator = mapUndefined mkFn3 props.renderSeparator
    , onEndReached = handleUndefined props.onEndReached
  }

scrollView_ :: Array ReactElement -> ReactElement
scrollView_ = createElement U.scrollViewClass {}

type ViewProps = StyleBase () Unit

view' :: Props ViewProps -> Array ReactElement -> ReactElement
view' p = createElement U.viewClass (p $ unsafeCoerce {})

view :: Styles -> Array ReactElement -> ReactElement
view = view' <<< styleOnly

view_ :: Array ReactElement -> ReactElement
view_ = view' id

type TextProps ev = StyleBase (
  onPress :: Unit -> ev
) ev

text_ :: String -> ReactElement
text_ = text' (id :: Props_ TextProps)

text' :: forall ev. HandledEvent ev => Props (TextProps ev) -> String -> ReactElement
text' p t = texts' p [textElem t]

text :: Styles -> String -> ReactElement
text = text' <<< (styleOnly :: Styles -> Props (TextProps Unit))

texts' :: forall ev. HandledEvent ev => Props (TextProps ev) -> Array ReactElement -> ReactElement
texts' p c = createElement U.textClass (fixProp $ p $ unsafeCoerce {}) c
  where fixProp pr = pr { onPress = handleUndefined pr.onPress }

texts :: Styles -> Array ReactElement -> ReactElement
texts = texts' <<< (styleOnly :: Styles -> Props (TextProps Unit))

texts_ :: Array ReactElement -> ReactElement
texts_ = texts' (id :: Props (TextProps Unit))

type ImageProps = StyleBase () Unit

image_ :: ImageSource -> ReactElement
image_ = image' id

image' :: (ImageProps -> ImageProps) -> ImageSource -> ReactElement
image' p source = backgroundImage' p source []

image :: Styles -> ImageSource -> ReactElement
image = image' <<< styleOnly

backgroundImage :: Styles -> ImageSource -> Array ReactElement -> ReactElement
backgroundImage = backgroundImage' <<< styleOnly

backgroundImage' :: (ImageProps -> ImageProps) -> ImageSource -> Array ReactElement -> ReactElement
backgroundImage' p source = createElement U.imageClass (p $ unsafeCoerce {source})

type TouchableNativeFeedbackProps ev = {
  onPress:: Unit -> ev
}

touchableNativeFeedback :: forall ev. HandledEvent ev => TouchableNativeFeedbackProps ev -> ReactElement -> ReactElement
touchableNativeFeedback p child = createElementOneChild U.touchableNativeFeedbackClass (fixProps p) child
  where fixProps pr = pr { onPress = handleUndefined pr.onPress }

type SwitchProps ev = StyleBase (
    onValueChange :: Boolean -> ev
  , value :: Boolean
  ) ev

switch' :: forall ev. HandledEvent ev => Props (SwitchProps ev) -> ReactElement
switch' p = createElement U.switchClass (fixProps $ p $ unsafeCoerce {}) []
  where fixProps pr = pr {onValueChange=handleUndefined pr.onValueChange}
