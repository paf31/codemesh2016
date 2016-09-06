module Main (main) where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Partial.Unsafe (unsafePartial)
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T

data SlidesState
  = S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6 Int
  | S7 Int
  | S8
  | S9
  | S10
  | S11
  | S12
  | S13
  | S14
  | S15
  | S16
  | S17
  | S18
  | S19
  | S20

initialState :: SlidesState
initialState = S0

data SlidesAction
  = First
  | Back
  | Next
  | Last
  | Increment

first :: SlidesState -> SlidesState
first _ = S0

next :: SlidesState -> SlidesState
next S0     = S1
next S1     = S2
next S2     = S3
next S3     = S4
next S4     = S5
next S5     = S6 0
next (S6 _) = S7 0
next (S7 _) = S8
next S8     = S9
next S9     = S10
next S10    = S11
next S11    = S12
next S12    = S13
next S13    = S14
next S14    = S15
next S15    = S16
next S16    = S17
next S17    = S18
next S18    = S19
next S19    = S20
next S20    = S0

back :: SlidesState -> SlidesState
back S0     = S20
back S1     = S0
back S2     = S1
back S3     = S2
back S4     = S3
back S5     = S4
back (S6 _) = S5
back (S7 _) = S6 0
back S8     = S7 0
back S9     = S8
back S10    = S9
back S11    = S10
back S12    = S11
back S13    = S12
back S14    = S13
back S15    = S14
back S16    = S15
back S17    = S16
back S18    = S17
back S19    = S18
back S20    = S19

last :: SlidesState -> SlidesState
last _ = S20

navbar :: forall props eff. T.Spec eff SlidesState props SlidesAction
navbar = T.simpleSpec performAction render where
  render :: T.Render SlidesState props SlidesAction
  render dispatch _ _ _ =
    [ RD.nav' [ RD.a [ RP.href "#"
                     , RP.onClick \_ -> dispatch First
                     ]
                     [ RD.text "⇦" ]
              , RD.a [ RP.href "#"
                     , RP.onClick \_ -> dispatch Back
                     ]
                     [ RD.text "←" ]
              , RD.a [ RP.href "#"
                     , RP.onClick \_ -> dispatch Next
                     ]
                     [ RD.text "→" ]
              , RD.a [ RP.href "#"
                     , RP.onClick \_ -> dispatch Last
                     ]
                     [ RD.text "⇨" ]
              ]
    ]

  performAction :: T.PerformAction eff SlidesState props SlidesAction
  performAction action _ _ = void $ T.cotransform (move action) where
    move First s = first s
    move Back  s = back s
    move Next  s = next s
    move Last  s = last s
    move Increment (S6 count) = S6 (count + 1)
    move Increment (S7 count) = S7 (count + 1)
    move _ s = s

slidesComponent :: forall props eff. T.Spec eff SlidesState props SlidesAction
slidesComponent = T.simpleSpec T.defaultPerformAction render where
  render :: T.Render SlidesState props SlidesAction
  render dispatch _ slide _ =
    [ RD.div'
        case slide of
          S0  -> [ RD.h1' [ RD.text "Front End Development with PureScript and Thermite" ]
                 , RD.h2' [ RD.text "Phil Freeman" ]
                 ]
          S1  -> [ RD.h1' [ RD.text "Intro" ]
                 , RD.p'  [ RD.text "Thermite is a" ]
                 , RD.ul' [ RD.li' [ RD.text "React-based" ]
                          , RD.li' [ RD.text "\"opinionated\"" ]
                          ]
                 , RD.p'  [ RD.text "UI library for PureScript" ]
                 ]
          S2  -> [ RD.h1' [ RD.text "Problems for UI Libraries" ]
                 , RD.p'  [ RD.text "UI libraries have to solve the following problems:" ]
                 , RD.ul' [ RD.li' [ RD.text "Multiple components" ]
                          , RD.li' [ RD.text "3rd party components" ]
                          , RD.li' [ RD.text "Async code (AJAX etc.)" ]
                          ]
                 , RD.p'  [ RD.text "Thermite uses PureScript's advanced type system features to solve these problems." ]
                 ]
          S3  -> [ RD.h1' [ RD.text "Components" ]
                 , RD.p'  [ RD.text "Thermite components are defined by:" ]
                 , RD.ul' [ RD.li' [ RD.text "A state type" ]
                          , RD.li' [ RD.text "An action type" ]
                          , RD.li' [ RD.text "A function which renders the current state" ]
                          , RD.li' [ RD.text "A function which updates the current state based on an action" ]
                          ]
                 , RD.p'  [ RD.text "See also: The Elm Architecture (TEA)" ]
                 ]
          S4  -> [ RD.h1' [ RD.text "Component Types" ]
                 , RD.pre' [ RD.code' [ RD.text """newtype Spec eff state props action = Spec
  { performAction ∷ PerformAction eff state props action
  , render        ∷ Render state props action
  }

type PerformAction eff state props action
  = action
  → props
  → state
  → CoTransformer (Maybe state) (state → state) (Aff eff) Unit

type Render state props action
  = (action → EventHandler)
  → props
  → state
  → Array ReactElement
  → Array ReactElement""" ] ]
                 ]
          S5  -> [ RD.h1' [ RD.text "Simple Components" ]
                 , RD.pre' [ RD.code' [ RD.text """simpleSpec
  ∷ ∀ eff state props action
  . PerformAction eff state props action
  → Render state props action
  → Spec eff state props action""" ] ]
                 ]
          S6 count ->
                 [ RD.h1' [ RD.text "Counter Component" ]
                 , RD.pre' [ RD.code' [ RD.text """type CounterState = Int
data CounterAction = Increment

counter = T.simpleSpec performAction render where
  render dispatch _ count _ =
    [ button [ onClick \_ -> dispatch Increment ]
             [ text (show count) ]
    ]

  performAction Increment _ _ = void $ T.cotransform (_ + 1)""" ] ]
                 , RD.p' [ RD.button [ RP.onClick \_ -> dispatch Increment ]
                                     [ RD.text (show count) ]
                         ]
                 ]
          S7 count ->
                 [ RD.h1' [ RD.text "Composing Components" ]
                 , RD.p'  [ RD.text "Components form a Monoid which gives us one type of composition:" ]
                 , RD.pre' [ RD.code' [ RD.text "twoCounters = counter <> counter" ] ]
                 , RD.p' [ RD.button [ RP.className "buttonLeft"
                                     , RP.onClick \_ -> dispatch Increment ]
                                     [ RD.text (show count) ]
                         , RD.button [ RP.className "buttonRight"
                                     , RP.onClick \_ -> dispatch Increment ]
                                     [ RD.text (show count) ]
                         , RD.div [ RP.className "clearBoth" ] []
                         ]
                 , RD.p' [ RD.text "... but perhaps not the one you were thinking of" ]
                 , RD.p' [ RD.text "We need a way to break up the state" ]
                 ]
          S8  -> [ RD.h1' [ RD.text "Focusing" ] ]
          S9  -> [ RD.h1' [ RD.text "Tab Components" ] ]
          S10 -> [ RD.h1' [ RD.text "Data Lists" ] ]
          S11 -> [ RD.h1' [ RD.text "Task List Example" ] ]
          S12 -> [ RD.h1' [ RD.text "Async" ] ]
          S13 -> [ RD.h1' [ RD.text "Try Thermite" ] ]
          S14 -> [ RD.h1' [ RD.text "" ] ]
          S15 -> [ RD.h1' [ RD.text "" ] ]
          S16 -> [ RD.h1' [ RD.text "" ] ]
          S17 -> [ RD.h1' [ RD.text "" ] ]
          S18 -> [ RD.h1' [ RD.text "" ] ]
          S19 -> [ RD.h1' [ RD.text "" ] ]
          S20 -> [ RD.h1' [ RD.text "Questions?" ] ]
    ]

main :: Eff (dom :: DOM.DOM) Unit
main = void do
  let component = T.createClass (navbar <> slidesComponent) initialState
  document <- DOM.window >>= DOM.document
  container <- unsafePartial (fromJust <<< toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document))
  RDOM.render (R.createFactory component {}) container
