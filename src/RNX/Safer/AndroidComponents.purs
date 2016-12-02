module RNX.Safer.Components.Android where

import Prelude
import Data.Maybe (Maybe(..))
import RNX.Color (Color)
import RNX.Components (toolbarAndroidClass)
import RNX.Props (ImageSource)
import RNX.Safer.Components (class HandledEvent, handleUndefined)
import RNX.Safer.Styles (Styles)
import RNX.Safer.Undefinable (mapUndefined, toUndefinable)
import React (ReactElement, createElement)
import Unsafe.Coerce (unsafeCoerce)

foreign import data ToolbarAction :: *
data ActionShow = Always | IfRoom | Never

action :: String -> ToolbarAction
action t = action' {title:t, icon:Nothing, show:Never, showWithText:true}

action' :: {title::String, icon::Maybe ImageSource, show::ActionShow, showWithText::Boolean} -> ToolbarAction
action' {title,icon,show,showWithText} = unsafeCoerce {title, icon:toUndefinable icon ,show: toShowStr show, showWithText}
  where
  toShowStr :: ActionShow -> String
  toShowStr Always = "always"
  toShowStr IfRoom = "ifRoom"
  toShowStr Never = "never"

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
