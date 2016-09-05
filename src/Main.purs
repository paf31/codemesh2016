module Main (main) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.List (List(Nil))
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Partial.Unsafe (unsafePartial)
import React (createFactory) as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T

data Slide = Slide

type SlidesState =
  { prev        :: List Slide
  , current     :: Slide
  , next        :: List Slide
  }

slides :: SlidesState
slides =
  { prev: Nil
  , current: Slide
  , next: Nil
  }

data SlidesAction
  = First
  | Back
  | Next
  | Last

first :: SlidesState -> SlidesState
first = id

back :: SlidesState -> SlidesState
back = id

next :: SlidesState -> SlidesState
next = id

last :: SlidesState -> SlidesState
last = id

slidesComponent :: forall props eff. T.Spec eff SlidesState props SlidesAction
slidesComponent = T.simpleSpec performAction render
  where
    render :: T.Render SlidesState props SlidesAction
    render dispatch _ s _ =
      [ R.h1' [ R.text "Thermite" ]
      , R.nav' [ R.a [ RP.href "#"
                     , RP.onClick \_ -> dispatch First
                     ]
                     [ R.text "⇦" ]
               , R.a [ RP.href "#"
                     , RP.onClick \_ -> dispatch Back
                     ]
                     [ R.text "←" ]
               , R.a [ RP.href "#"
                     , RP.onClick \_ -> dispatch Next
                     ]
                     [ R.text "→" ]
               , R.a [ RP.href "#"
                     , RP.onClick \_ -> dispatch Last
                     ]
                     [ R.text "⇨" ]
               ]
      ]

    performAction :: T.PerformAction eff SlidesState props SlidesAction
    performAction action _ _ = void $ T.cotransform (move action) where
      move First = first
      move Back  = back
      move Next  = next
      move Last  = last

main :: Eff (dom :: DOM.DOM) Unit
main = void do
  let component = T.createClass slidesComponent slides
  document <- DOM.window >>= DOM.document
  container <- unsafePartial (fromJust <<< toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document))
  RDOM.render (R.createFactory component {}) container
