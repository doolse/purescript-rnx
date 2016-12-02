module RNX.Safer.Components where

import Prelude
import RNX.Components as U
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Function.Uncurried (mkFn3)
import RNX.Components (textElem)
import RNX.Props (ImageSource)
import RNX.Safer.Styles (Styles)
import RNX.Safer.Undefinable (mapUndefined)
import React (ReactElement, createElement)
import Unsafe.Coerce (unsafeCoerce)

foreign import data ListViewDataSource :: * -> *

foreign import listViewDataSource :: forall a. (Eq a) => Array a -> ListViewDataSource a
foreign import cloneWithRows :: forall a. ListViewDataSource a -> Array a -> ListViewDataSource a

class HandledEvent a where
  handle :: a -> Unit

handleUndefined :: forall ev a. (HandledEvent ev) => (a -> ev) -> a -> Unit
handleUndefined = mapUndefined (compose handle)

instance unitHandler :: HandledEvent Unit where
  handle = id

instance affHandler :: HandledEvent (Aff eff Unit) where
  handle = unsafePerformEff <<< void <<< launchAff

type ListViewProps ev sid rid = {
    initialListSize ::Int
  , renderSeparator :: sid -> rid -> Boolean -> ReactElement
  , enableEmptySections :: Boolean
  , onEndReached :: Unit -> ev
}

listView :: forall a. ListViewDataSource a -> (a -> ReactElement) ->  ReactElement
listView = listView' (id :: forall sid rid. ListViewProps Unit sid rid -> ListViewProps Unit sid rid)

listView' :: forall ev a sid rid. HandledEvent ev => (ListViewProps ev sid rid -> ListViewProps ev sid rid) -> ListViewDataSource a -> (a -> ReactElement) ->  ReactElement
listView' p dataSource renderRow = createElement U.listViewClass (fixProps $ unsafeCoerce $ p $ unsafeCoerce {dataSource, renderRow, enableEmptySections: true}) []
  where
  fixProps props = props {
      renderSeparator = mapUndefined mkFn3 props.renderSeparator
    , onEndReached = mapUndefined (\u -> handle <<< u) (props.onEndReached :: (Unit -> ev))
  }

scrollView_ :: Array ReactElement -> ReactElement
scrollView_ = createElement U.scrollViewClass {}

type ViewProps = {
  style :: Styles
}

view' :: (ViewProps -> ViewProps) -> Array ReactElement -> ReactElement
view' p = createElement U.viewClass (p $ unsafeCoerce {})

view :: Styles -> Array ReactElement -> ReactElement
view = view' <<< styleOnly

view_ :: Array ReactElement -> ReactElement
view_ = view' id

type TextProps = {
  style :: Styles
}

styleOnly :: forall r. Styles -> {style::Styles|r} -> {style::Styles|r}
styleOnly s = _ {style=s}

text_ :: String -> ReactElement
text_ = text' id

text' :: (TextProps -> TextProps) -> String -> ReactElement
text' p t = texts' p [textElem t]

text :: Styles -> String -> ReactElement
text = text' <<< styleOnly

texts' :: (TextProps -> TextProps) -> Array ReactElement -> ReactElement
texts' p c = createElement U.textClass (p $ unsafeCoerce {}) c

texts :: Styles -> Array ReactElement -> ReactElement
texts = texts' <<< styleOnly

type ImageProps = {
  style :: Styles
}

image_ :: ImageSource -> ReactElement
image_ = image' id

image' :: (ImageProps -> ImageProps) -> ImageSource -> ReactElement
image' p source = createElement U.imageClass (p $ unsafeCoerce {source}) []

image :: Styles -> ImageSource -> ReactElement
image = image' <<< styleOnly
