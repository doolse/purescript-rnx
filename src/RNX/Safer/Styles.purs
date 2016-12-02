module RNX.Safer.Styles where

import RNX.Styles (StyleProp)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Styles :: *

foreign import staticStyles :: Array StyleProp -> Styles

foreign import styles :: Array StyleProp -> Styles

styles' :: Array Styles -> Styles
styles' = unsafeCoerce
