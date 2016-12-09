module RNX.Safer.Components.Android where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import RNX.Color (Color)
import RNX.Components (toolbarAndroidClass)
import RNX.Props (ImageSource)
import RNX.Safer.Components (class HandledEvent, handleUndefined)
import RNX.Safer.Styles (Styles)
import RNX.Safer.Undefinable (Undefinable, mapUndefined, toUndefinable)
import React (ReactClass, ReactElement, ReactState, ReactThis, ReadWrite, createElement)
import Unsafe.Coerce (unsafeCoerce)

foreign import backAndroidAddListener :: forall eff. Fn2 String (Eff eff Boolean) Unit
foreign import data DrawPositionVal :: *
foreign import dcLeft :: DrawPositionVal
foreign import dcRight :: DrawPositionVal
foreign import drawerLayoutAndroidClass :: forall props. ReactClass props
foreign import openDrawer :: forall p s eff. ReactThis p s -> Eff (state::ReactState ReadWrite|eff) Unit
foreign import closeDrawer :: forall p s eff. ReactThis p s -> Eff (state::ReactState ReadWrite|eff) Unit
foreign import exitApp :: Int -> Unit

type ToolbarAction = {title::String, icon::Undefinable ImageSource, show::ActionShow, showWithText::Boolean}
newtype ActionShow = ActionShow String

-- Always | IfRoom | Never
always :: ActionShow
always = ActionShow "always"

ifRoom :: ActionShow
ifRoom = ActionShow "ifRoom"

never :: ActionShow
never = ActionShow "never"

action :: String -> ToolbarAction
action title = action' {title, icon:Nothing, show:never, showWithText:true}

action' :: {title::String, icon::Maybe ImageSource, show::ActionShow, showWithText::Boolean} -> ToolbarAction
action' p = p {icon = toUndefinable p.icon }

type ToolbarAndroidProp ev = {
    style :: Styles
  , titleColor :: Color
  , actions :: Array ToolbarAction
  , navIcon :: ImageSource
  , onIconClicked :: Unit -> ev
  , onActionSelected :: Int -> ev
  , title :: String
}

toolbarAndroid :: forall ev. (HandledEvent ev) => (ToolbarAndroidProp ev -> ToolbarAndroidProp ev) -> Array ReactElement -> ReactElement
toolbarAndroid p = let
  fixProps props = props {
    titleColor = mapUndefined show props.titleColor
  , onIconClicked = handleUndefined props.onIconClicked
  , onActionSelected = handleUndefined props.onActionSelected
  }
  in  createElement toolbarAndroidClass (fixProps $ p $ unsafeCoerce {})

backEventCallback :: forall eff. Eff eff Unit -> Eff eff Unit
backEventCallback cb = pure $ runFn2 backAndroidAddListener "hardwareBackPress" $ do
  cb
  pure true

data DrawerPosition = Left | Right

drawerLayoutAndroid :: {ref::String} -> Int -> DrawerPosition -> (Unit -> ReactElement) -> Array ReactElement -> ReactElement
drawerLayoutAndroid {ref} drawerWidth pos renderNavigationView =
  createElement drawerLayoutAndroidClass {drawerWidth, drawerPosition: dp2pos pos, renderNavigationView, ref}
  where
    dp2pos Left = dcLeft
    dp2pos Right = dcRight
