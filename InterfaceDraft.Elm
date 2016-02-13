module InterfaceDraft where

import Random
import Signal
import Graphics.Collage
import Graphics.Element exposing (Element, empty)
import String
import Color exposing (..)
import Window
import Time
import Text


type alias Point = { x:Float, y:Float }







main : Element
main =
  Graphics.Element.leftAligned (Text.italic (Text.fromString "dude..."))
    

