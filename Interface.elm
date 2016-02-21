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

-- Compile with: elm make Interface.elm --output=int.html && open -a Google\ Chrome int.html


(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

type alias State  = String
type CalcEvent = PLU | MIN | DIV | TIM | PM | DEC | LPAR | RPAR | Zero | One | Two | Three| Four | Five | Six | Seven | Eight | Nine | Compute | Clear

initInputState = ""
upstate e i       = case e of
                      PLU  -> i ++ " + "
                      MIN  -> i ++ " - "
                      DIV  -> i ++ "/"
                      TIM  -> i ++ "*"
                      Clear -> ""
                      Zero -> i ++ "0"
                      One   -> i ++ "1"
                      Two   -> i ++ "2"
                      Three   -> i ++ "3"
                      Four   -> i ++ "4"
                      Five   -> i ++ "5"
                      Six   -> i ++ "6"
                      Seven   -> i ++ "7"
                      Eight   -> i ++ "8"
                      Nine   -> i ++ "9"
                      Compute -> i ++ "="
                      LPAR -> i ++ "("
                      RPAR -> i ++ ")"
                      PM -> plusOrMin i
                      DEC -> i ++ "."

strStyle : String -> E.Element
strStyle = T.fromString >> T.height 30 >> E.centered
lineStyle = { defaultLine | color = darkPink , width = 5 }
spacerLineStyle = { defaultLine | color = lightPink , width = 5 }

captionW = 500
captionH = 70

btnW = 70
btnH = 70

lightPink = Color.rgb 255 182 193
darkPink = Color.rgb 255 50 147


-- go implement this properly
plusOrMin : String -> String
plusOrMin s = 
  "-" ++ s


type alias Point = { x:Float, y:Float }

type alias Input bool =
    { returnKey : bool
    , delta : Time.Time
    }


myButton msg s =
  let drawButton c =
    C.collage btnW btnH
       [ C.filled c  (C.ngon 6 30)
       , C.outlined lineStyle (C.ngon 6 30)
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


vspace = E.spacer 5 5

toStringMinusQuotes : String -> String
toStringMinusQuotes s =
  String.dropRight 1 (String.dropLeft 1 (toString s))


view i (w,h) =
  let squareSpacer = C.collage btnW btnH [C.outlined spacerLineStyle (C.rect btnW btnH)] in 
  let caption = C.collage captionW captionH [
    i |> toStringMinusQuotes |> strStyle |> E.container captionW captionH E.midTop |> C.toForm,
    C.outlined lineStyle (C.rect captionW captionH)] in
  let spacerColumn = E.flow E.down <| List.intersperse vspace [squareSpacer, squareSpacer, squareSpacer, squareSpacer, squareSpacer] in
  let column1 = E.flow E.down <| List.intersperse vspace [squareSpacer, clearButton, oneButton, fourButton, sevenButton, plusButton, squareSpacer] in
  let column2 = E.flow E.down <| List.intersperse vspace [squareSpacer, pmButton, twoButton, fiveButton, eightButton, minButton, squareSpacer] in
  let column3 = E.flow E.down <| List.intersperse vspace [squareSpacer, computeButton, threeButton, sixButton, nineButton, divButton, squareSpacer] in
  let column4 = E.flow E.down <| List.intersperse vspace [squareSpacer, lparButton, rparButton, zeroButton, decimalButton, timesButton, squareSpacer] in
  let columns = E.flow E.right <| List.intersperse vspace [spacerColumn, column1, column2, column3, column4] in
  let calcGrid = E.flow E.down <| List.intersperse vspace [caption , columns] in
  let fullLayout = E.color lightPink <| E.container w h E.middle calcGrid in
  C.collage w h [(C.toForm fullLayout)]        -- ([C.toForm buttonLayoutLeft, C.toForm buttonLayoutMiddle, C.toForm buttonLayoutRight ])

stateOverTime : Signal State
stateOverTime = Signal.foldp upstate initInputState buttonMailbox.signal



main = 
  Signal.map2 view stateOverTime Window.dimensions





