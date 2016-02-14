module InterfaceDraft where

import Random
import String
import Color exposing (..)
import Window
import Time
import Text as T
import Signal exposing (Mailbox, mailbox)
import Graphics.Element as E
import Graphics.Input exposing (button, customButton)
import Graphics.Collage as C exposing (defaultLine)

-- Compile with: elm make InterfaceDraft.elm --output=int.html


(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

type alias State  = String
type CalcEvent = PLU | One | Two | Three| Compute | Clear

initInputState = ""
upstate e i       = case e of
                      PLU  -> i ++ "+"
                      Clear -> ""
                      One   -> i ++ "1"
                      Two   -> i ++ "2"
                      Three   -> i ++ "3"
                      Compute -> i ++ "="

strStyle : String -> E.Element
strStyle = T.fromString >> T.height 30 >> E.centered
lineStyle = { defaultLine | color = Color.darkCharcoal , width = 10 }

captionW = 500
captionH = 70

btnW = 60
btnH = 60

type alias Point = { x:Float, y:Float }

type alias Input bool =
    { returnKey : bool
    , delta : Time.Time
    }


myButton msg s =
  let drawButton c =
    C.collage btnW btnH
       [ C.filled c (C.rect btnW btnH)
       , C.outlined lineStyle (C.rect btnW btnH)
       , strStyle s |> C.toForm
    ]
  in
  customButton msg
    (drawButton Color.lightBlue)
    (drawButton Color.lightOrange)
    (drawButton Color.lightBlue)

buttonMailbox : Mailbox CalcEvent
buttonMailbox = mailbox Clear

plusButton   = myButton (Signal.message buttonMailbox.address PLU) "+"
oneButton   = myButton (Signal.message buttonMailbox.address One) "1"
twoButton   = myButton (Signal.message buttonMailbox.address Two) "2"
threeButton   = myButton (Signal.message buttonMailbox.address Three) "3"
clearButton = myButton (Signal.message buttonMailbox.address Clear) "C"
computeButton = myButton (Signal.message buttonMailbox.address Compute) "="

vspace = E.spacer 0 10

toStringMinusQuotes : String -> String
toStringMinusQuotes s =
  String.dropRight 1 (String.dropLeft 1 (toString s))


view i (w,h) =
  let caption = i |> toStringMinusQuotes |> strStyle |> E.container captionW captionH E.midTop in
  let column1 = E.flow E.down <| List.intersperse vspace [ caption, oneButton, twoButton, threeButton] in
  let column2 = E.flow E.down <| List.intersperse vspace [  clearButton, plusButton, computeButton] in
  let columns = C.collage  w h (List.map C.toForm [column1, column2]) in
  let buttonLayout = E.color Color.gray <| E.container w h E.middle columns in
  C.collage w h ([C.toForm buttonLayout])

stateOverTime : Signal State
stateOverTime = Signal.foldp upstate initInputState buttonMailbox.signal



main = 
  Signal.map2 view stateOverTime Window.dimensions





