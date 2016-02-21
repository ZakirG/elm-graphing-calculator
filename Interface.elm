module Interface where

import Parsing as P
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

-- Compile with: elm make Interface.elm --output=int.html && open -a Google\ Chrome int.html


(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

type alias State  = (String, String)
type CalcEvent = DEL | PLU | MIN | DIV | TIM | PM | DEC | LPAR | RPAR | Zero | One | Two | Three| Four | Five | Six | Seven | Eight | Nine | Compute | Clear

initInputState = ("","")
upstate e (stringToCompute , stringResult) = case e of
                      PLU  -> (stringToCompute ++ " + ", stringResult)
                      MIN  -> (stringToCompute ++ " - ", stringResult)
                      DIV  -> (stringToCompute ++ " / ", stringResult)
                      TIM  -> (stringToCompute ++ " * ", stringResult)
                      Clear -> ("", "")
                      Zero -> (stringToCompute ++ "0", stringResult)
                      One   -> (stringToCompute ++ "1", stringResult)
                      Two   -> (stringToCompute ++ "2", stringResult)
                      Three   -> (stringToCompute ++ "3", stringResult)
                      Four   -> (stringToCompute ++ "4", stringResult)
                      Five   -> (stringToCompute ++ "5", stringResult)
                      Six   -> (stringToCompute ++ "6", stringResult)
                      Seven   -> (stringToCompute ++ "7", stringResult)
                      Eight   -> (stringToCompute ++ "8", stringResult)
                      Nine   -> (stringToCompute ++ "9", stringResult)
                      Compute -> ("", toString (P.parseStringToCompute stringToCompute))
                      LPAR -> (stringToCompute ++ " ( ", stringResult)
                      RPAR -> (stringToCompute ++ " ) ", stringResult)
                      PM -> (P.plusOrMin stringToCompute, stringResult)
                      DEC -> (stringToCompute ++ ".", stringResult)
                      DEL -> (P.backspace stringToCompute, stringResult)



strStyle : String -> E.Element
strStyle = T.fromString >> T.height 25 >> T.color Color.white >> E.centered
lineStyle = { defaultLine | color = darkPink , width = 5 }
spacerLineStyle = { defaultLine | color = lightPink , width = 5 }
captionStrStyle = T.fromString >> T.height 20 >> T.italic >> T.color Color.white >> E.leftAligned

graphGridW = 900
graphGridH = 850


captionW = 900
captionH = 70

btnW = 70
btnH = 70

lightPink = Color.rgb 255 182 193
darkPink = Color.rgb 255 50 147

type alias Point = { x:Float, y:Float }

type alias Input bool =
    { returnKey : bool
    , delta : Time.Time
    }


myButton msg s =
  let drawButton c =
    C.collage btnW btnH
       [ C.filled c  (C.ngon 6 32)
       , C.outlined lineStyle (C.ngon 6 32)
       , strStyle s |> C.toForm
    ]
  in
  customButton msg
    (drawButton lightPink)
    (drawButton darkPink)
    (drawButton Color.grey)

buttonMailbox : Mailbox CalcEvent
buttonMailbox = mailbox Clear

plusButton   = myButton (Signal.message buttonMailbox.address PLU) "+"
minButton   = myButton (Signal.message buttonMailbox.address MIN) "-"
divButton   = myButton (Signal.message buttonMailbox.address DIV) "/"
timesButton   = myButton (Signal.message buttonMailbox.address TIM) "*"
zeroButton   = myButton (Signal.message buttonMailbox.address Zero) "0"
oneButton   = myButton (Signal.message buttonMailbox.address One) "1"
twoButton   = myButton (Signal.message buttonMailbox.address Two) "2"
threeButton   = myButton (Signal.message buttonMailbox.address Three) "3"
fourButton   = myButton (Signal.message buttonMailbox.address Four) "4"
fiveButton   = myButton (Signal.message buttonMailbox.address Five) "5"
sixButton   = myButton (Signal.message buttonMailbox.address Six) "6"
sevenButton   = myButton (Signal.message buttonMailbox.address Seven) "7"
eightButton   = myButton (Signal.message buttonMailbox.address Eight) "8"
nineButton   = myButton (Signal.message buttonMailbox.address Nine) "9"
clearButton = myButton (Signal.message buttonMailbox.address Clear) "AC"
computeButton = myButton (Signal.message buttonMailbox.address Compute) "="
pmButton = myButton (Signal.message buttonMailbox.address PM) "+/-"
decimalButton = myButton (Signal.message buttonMailbox.address DEC) "."
lparButton = myButton (Signal.message buttonMailbox.address LPAR) "("
rparButton = myButton (Signal.message buttonMailbox.address RPAR) ")"
delButton = myButton (Signal.message buttonMailbox.address DEL) "del"


vspace = E.spacer 5 5

toStringMinusQuotes : String -> String
toStringMinusQuotes s =
  String.dropRight 1 (String.dropLeft 1 (toString s))


view inputState (w,h) =
  let (stringToCompute, stringResult) = inputState in
  let squareSpacer = C.collage btnW btnH [C.outlined spacerLineStyle (C.rect 60 70)] in 
  let tallSquareSpacer = C.collage btnW btnH [C.outlined spacerLineStyle (C.rect 60 300)] in 
  let toCompute = C.collage captionW captionH [ 
    stringToCompute |> toStringMinusQuotes |> strStyle |> E.container captionW captionH E.middle |> C.toForm,
    C.outlined lineStyle (C.rect captionW captionH),
     captionStrStyle "INPUT" |> C.toForm |> C.move (-400,0)] in
  let resultBar = C.collage captionW captionH [
    stringResult |> toStringMinusQuotes |> strStyle |> E.container captionW captionH E.middle |> C.toForm,
    C.outlined lineStyle (C.rect captionW captionH),
    captionStrStyle "RESULT" |> C.toForm |> C.move (-400,0)] in
  let graphPane = C.collage graphGridW graphGridH [
    C.outlined lineStyle (C.rect  graphGridW graphGridH),
    captionStrStyle "GRAPH OUTPUT" |> C.toForm |> C.move (-370,400)]
      in
  let spacerColumn = E.flow E.down <| List.intersperse vspace [tallSquareSpacer, squareSpacer, squareSpacer, squareSpacer, squareSpacer, squareSpacer] in
  let column1 = E.flow E.down <| List.intersperse vspace [tallSquareSpacer, squareSpacer, clearButton, oneButton, fourButton, sevenButton,  squareSpacer] in
  let column2 = E.flow E.down <| List.intersperse vspace [tallSquareSpacer, squareSpacer, pmButton, twoButton, fiveButton, eightButton, squareSpacer] in
  let column3 = E.flow E.down <| List.intersperse vspace [tallSquareSpacer, squareSpacer, computeButton, threeButton, sixButton, nineButton, squareSpacer] in
  let column4 = E.flow E.down <| List.intersperse vspace [tallSquareSpacer, squareSpacer, lparButton, rparButton, decimalButton, zeroButton, squareSpacer] in
  let column5 = E.flow E.down <| List.intersperse vspace [tallSquareSpacer,  delButton,plusButton, minButton, divButton, timesButton, squareSpacer] in
  let columns = C.collage 500 1000 [ 
    C.outlined lineStyle (C.rect 500 1000),
    C.toForm <| E.flow E.right <| List.intersperse vspace [spacerColumn, column1, column2, column3, column4, column5, spacerColumn]
    ] in
  let outputGrid = E.flow E.down <| List.intersperse vspace [toCompute , resultBar, graphPane] in
  let calcGrid = E.flow E.right <| List.intersperse vspace [columns, outputGrid] in
  let fullLayout = E.color lightPink <| E.container w h E.middle calcGrid in
  C.collage w h [(C.toForm fullLayout)]

stateOverTime : Signal State
stateOverTime = Signal.foldp upstate initInputState buttonMailbox.signal



main = 
  Signal.map2 view stateOverTime Window.dimensions





