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
  = Intro
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
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
  | End

initialState :: SlidesState
initialState = Intro

data SlidesAction
  = First
  | Back
  | Next
  | Last

first :: SlidesState -> SlidesState
first _ = Intro

next :: SlidesState -> SlidesState
next Intro = S1
next S1  = S2
next S2  = S3
next S3  = S4
next S4  = S5
next S5  = S6
next S6  = S7
next S7  = S8
next S8  = S9
next S9  = S10
next S10 = S11
next S11 = S12
next S12 = S13
next S13 = S14
next S14 = S15
next S15 = S16
next S16 = S17
next S17 = S18
next S18 = S19
next S19 = End
next End = Intro

back :: SlidesState -> SlidesState
back Intro = End
back S1  = Intro
back S2  = S1
back S3  = S2
back S4  = S3
back S5  = S4
back S6  = S5
back S7  = S6
back S8  = S7
back S9  = S8
back S10 = S9
back S11 = S10
back S12 = S11
back S13 = S12
back S14 = S13
back S15 = S14
back S16 = S15
back S17 = S16
back S18 = S17
back S19 = S18
back End = S19

last :: SlidesState -> SlidesState
last _ = End

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
    move First = first
    move Back  = back
    move Next  = next
    move Last  = last

slidesComponent :: forall props eff. T.Spec eff SlidesState props SlidesAction
slidesComponent = T.simpleSpec T.defaultPerformAction render where
  render :: T.Render SlidesState props SlidesAction
  render dispatch _ slide _ =
    [ RD.div'
        case slide of
          Intro ->
            [ RD.h1' [ RD.text "Front End Development with PureScript and Thermite" ]
            , RD.p' [ RD.text "Phil Freeman" ]
            ]
          S1 -> [ RD.h1' [ RD.text "Intro" ] ]
          S2 -> [ RD.h1' [ RD.text "" ] ]
          S3 -> [ RD.h1' [ RD.text "" ] ]
          S4 -> [ RD.h1' [ RD.text "" ] ]
          S5 -> [ RD.h1' [ RD.text "" ] ]
          S6 -> [ RD.h1' [ RD.text "" ] ]
          S7 -> [ RD.h1' [ RD.text "" ] ]
          S8 -> [ RD.h1' [ RD.text "" ] ]
          S9 -> [ RD.h1' [ RD.text "" ] ]
          S10 -> [ RD.h1' [ RD.text "" ] ]
          S11 -> [ RD.h1' [ RD.text "" ] ]
          S12 -> [ RD.h1' [ RD.text "" ] ]
          S13 -> [ RD.h1' [ RD.text "" ] ]
          S14 -> [ RD.h1' [ RD.text "" ] ]
          S15 -> [ RD.h1' [ RD.text "" ] ]
          S16 -> [ RD.h1' [ RD.text "" ] ]
          S17 -> [ RD.h1' [ RD.text "" ] ]
          S18 -> [ RD.h1' [ RD.text "" ] ]
          S19 -> [ RD.h1' [ RD.text "" ] ]
          End -> [ RD.h1' [ RD.text "Questions?" ] ]
    ]

main :: Eff (dom :: DOM.DOM) Unit
main = void do
  let component = T.createClass (navbar <> slidesComponent) initialState
  document <- DOM.window >>= DOM.document
  container <- unsafePartial (fromJust <<< toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document))
  RDOM.render (R.createFactory component {}) container
