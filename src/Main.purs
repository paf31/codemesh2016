module Main (main) where

import Prelude
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Control.Monad.Eff (Eff)
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Lens (only, ALens', Prism', Lens, cloneLens, over, set, prism, lens)
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import Partial.Unsafe (unsafePartial)

type SharedState =
  { fstCounter    :: Int
  , sndCounter    :: Int
  }

type SlidesState =
  { slideNumber   :: Int
  , sharedState   :: SharedState
  }

slideNumber :: forall a b r. Lens { slideNumber :: a | r } { slideNumber :: b | r } a b
slideNumber = lens _.slideNumber (_ { slideNumber = _ })

sharedState :: forall a b r. Lens { sharedState :: a | r } { sharedState :: b | r } a b
sharedState = lens _.sharedState (_ { sharedState = _ })

fstCounter :: forall a b r. Lens { fstCounter :: a | r } { fstCounter :: b | r } a b
fstCounter = lens _.fstCounter (_ { fstCounter = _ })

sndCounter :: forall a b r. Lens { sndCounter :: a | r } { sndCounter :: b | r } a b
sndCounter = lens _.sndCounter (_ { sndCounter = _ })

slideNumberIs :: Int -> Prism' SlidesState SharedState
slideNumberIs n =
  prism { slideNumber: n, sharedState: _ }
    case _ of
      o@{ slideNumber: m, sharedState: shared }
        | m == n -> Right shared
        | otherwise -> Left o

initialState :: SlidesState
initialState =
  { slideNumber : 0
  , sharedState:
    { fstCounter  : 0
    , sndCounter  : 0
    }
  }

data SlidesAction
  = First
  | Back
  | Next
  | Last
  | Increment1
  | Increment2

derive instance eqSlidesAction :: Eq SlidesAction

increment1 :: Prism' SlidesAction Unit
increment1 = only Increment1

increment2 :: Prism' SlidesAction Unit
increment2 = only Increment2

first :: SlidesState -> SlidesState
first = set slideNumber 0

next :: SlidesState -> SlidesState
next = over slideNumber ((_ `mod` 21) <<< add 1)

back :: SlidesState -> SlidesState
back = over slideNumber ((_ `mod` 21) <<< add 20)

last :: SlidesState -> SlidesState
last = set slideNumber 20

increment :: ALens' SharedState Int -> SlidesState -> SlidesState
increment l = over (sharedState <<< cloneLens l) (add 1)

counter :: forall eff props. T.Spec eff Int props Unit
counter = T.simpleSpec performAction render
  where
    render :: T.Render Int props Unit
    render dispatch _ state _ =
      [ RD.p' [ RD.button [ RP.onClick \_ -> dispatch unit ]
                          [ RD.text (show state) ]
              ]
      ]

    performAction :: T.PerformAction eff Int props Unit
    performAction _ _ _ = void $ T.modifyState (add 1)

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
  performAction First _ _ = void $ T.modifyState first
  performAction Back  _ _ = void $ T.modifyState back
  performAction Next  _ _ = void $ T.modifyState next
  performAction Last  _ _ = void $ T.modifyState last
  performAction _ _ _ = pure unit

slide0 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide0 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Front End Development with PureScript and Thermite" ]
  , RD.h2' [ RD.text "Phil Freeman" ]
  ]

slide1 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide1 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Intro" ]
  , RD.p'  [ RD.text "Thermite is" ]
  , RD.ul' [ RD.li' [ RD.text "A React-based UI library for PureScript" ]
           , RD.li' [ RD.text "\"Opinionated\"" ]
           , RD.li' [ RD.text "Inspired by Elm, react-blaze and OpticUI" ]
           , RD.li' [ RD.text "Stable (i.e. version 1.*)" ]
           ]
  ]

slide2 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide2 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Problems for UI Libraries" ]
  , RD.p'  [ RD.text "UI libraries have to solve the following problems:" ]
  , RD.ul' [ RD.li' [ RD.text "Multiple components" ]
           , RD.li' [ RD.text "3rd party components" ]
           , RD.li' [ RD.text "Async code (AJAX etc.)" ]
           ]
  , RD.p'  [ RD.text "Thermite uses PureScript's advanced type system features to solve these problems:" ]
  , RD.ul' [ RD.li' [ RD.text "Lenses" ]
           , RD.li' [ RD.text "Coroutines" ]
           ]
  ]

slide3 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide3 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Components" ]
  , RD.p'  [ RD.text "Thermite components are defined by:" ]
  , RD.ul' [ RD.li' [ RD.text "A state type σ" ]
           , RD.li' [ RD.text "An action type δ" ]
           , RD.li' [ RD.text "A function which renders the current state" ]
           , RD.li' [ RD.text "A function which updates the current state based on an action" ]
           ]
  , RD.p'  [ RD.text "See also: The Elm Architecture (TEA)" ]
  ]

slide4 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide4 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
    [ RD.h1' [ RD.text "Component Types" ]
    , RD.pre' [ RD.code' [ RD.text example ] ]
    ]
  where
    example =
      """newtype Spec eff σ δ = Spec
  { performAction ∷ PerformAction eff σ δ
  , render        ∷ Render σ δ
  }

type PerformAction eff σ δ
  = δ
  → σ
  → CoTransformer (Maybe σ) (σ → σ) (Aff eff) Unit

type Render σ δ
  = (δ → EventHandler)
  → σ
  → Array ReactElement
  → Array ReactElement"""

slide5 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide5 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
    [ RD.h1' [ RD.text "Simple Components" ]
    , RD.pre' [ RD.code' [ RD.text example ] ]
    ]
  where
    example = """simpleSpec
  ∷ ∀ eff σ δ
  . PerformAction eff σ δ
  → Render σ δ
  → Spec eff σ δ"""

slide6 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide6 = intro <> T.focus fstCounter increment1 counter
  where
    intro = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
      [ RD.h1' [ RD.text "Counter Component" ]
      , RD.pre' [ RD.code' [ RD.text example ] ]
      ]
    example = """type CounterState = Int
data CounterAction = Increment

counter = T.simpleSpec performAction render where
  render dispatch _ count _ =
    [ button [ onClick \_ → dispatch Increment ]
    [ text (show count) ]
    ]

  performAction Increment _ _ = void $ T.cotransform (_ + 1)"""

slide7 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide7 = intro <> counters <> outro
  where
    intro = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
      [ RD.h1' [ RD.text "Composing Components" ]
      , RD.p'  [ RD.text "Components form a Monoid which gives us one type of composition:" ]
      , RD.pre' [ RD.code' [ RD.text "twoCounters = counter <> counter" ] ]
      ]

    counters = T.focus fstCounter increment1 counter
            <> T.focus fstCounter increment1 counter

    outro = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
      [ RD.p' [ RD.text "... but perhaps not the one you were thinking of" ]
      , RD.p' [ RD.text "We need a way to break up the state" ]
      ]

slide8 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide8 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Focusing" ]
  , RD.p'   [ RD.text "The state type for two independent counters is" ]
  , RD.pre' [ RD.code' [ RD.text "Tuple CounterState CounterState" ] ]
  , RD.p'   [ RD.text "And the action type is" ]
  , RD.pre' [ RD.code' [ RD.text "Either CounterAction CounterAction" ] ]
  , RD.p'   [ RD.text "We could write explicit functions to direct the actions to the right part of the state" ]
  ]

slide9 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide9 = intro <> counters
  where
    intro = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
      [ RD.h1'  [ RD.text "Focusing" ]
      , RD.p'   [ RD.text "Instead, we use a Lens to focus on a smaller part of the state, and a Prism to match a subset of the actions." ]
      , RD.pre' [ RD.code' [ RD.text example ] ]
      , RD.p'   [ RD.text "Our component becomes:" ]
      , RD.pre' [ RD.code' [ RD.text "focus _1 _Left counter <> focus _2 _Right counter" ] ]
      ]

    counters = T.focus fstCounter increment1 counter
            <> T.focus sndCounter increment2 counter

    example = """focus
  ∷ ∀ eff σ₁ σ₂ δ₁ δ₂
  . LensP σ₂ σ₁
  → PrismP δ₂ δ₁
  → Spec eff σ₁ δ₁
  → Spec eff σ₂ δ₂"""

slide10 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide10 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
    [ RD.h1'  [ RD.text "Tab Components" ]
    , RD.p'   [ RD.text "Lenses let us identify smaller parts of a product of types, which is a good model for independent components." ]
    , RD.p'   [ RD.text "Prisms let us identify smaller parts of a sum of types, which is a good model for tabbed applications:" ]
    , RD.pre' [ RD.code' [ RD.text example ] ]
    , RD.p'   [ RD.text "Note that" ]
    , RD.ul' [ RD.li' [ RD.text "The action type δ does not change here" ]
             , RD.li' [ RD.text "No information is shared between tabs" ]
             ]
    ]
  where
    example = """split
  ∷ ∀ eff σ₁ σ₂ δ
  . PrismP σ₁ σ₂
  → Spec eff σ₂ δ
  → Spec eff σ₁ δ"""

slide11 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide11 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
    [ RD.h1'  [ RD.text "List Components" ]
    , RD.p'   [ RD.text "The last type of composition lets us define lists of subcomponents:" ]
    , RD.pre' [ RD.code' [ RD.text example ] ]
    , RD.p'   [ RD.text "In OpticUI, this is generalized to a Traversal." ]
    , RD.p'   [ RD.text "Note that" ]
    , RD.ul'  [ RD.li' [ RD.text "We have a whole list of states, one for each list element" ]
              , RD.li' [ RD.text "The action type now includes the index of the component to update" ]
              ]
    ]
  where
    example = """foreach
  ∷ ∀ eff σ δ
  . (Int -> Spec eff σ δ)
  → Spec eff (List σ) (Tuple Int δ)"""

slide12 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide12 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Summary" ]
  , RD.table' [ RD.thead' [ RD.th' [ RD.text "Component" ]
                           , RD.th' [ RD.text "Function" ]
                           , RD.th' [ RD.text "Optic" ]
                           ]
               , RD.tbody' [ RD.tr' [ RD.td' [ RD.text "Pair (same state)" ]
                                    , RD.td' [ RD.pre' [ RD.code' [ RD.text "(<>)" ] ] ]
                                    , RD.td' [ RD.text "" ]
                                    ]
                           , RD.tr' [ RD.td' [ RD.text "Pair (independent)" ]
                                    , RD.td' [ RD.pre' [ RD.code' [ RD.text "focus" ] ] ]
                                    , RD.td' [ RD.text "Lens" ]
                                    ]
                           , RD.tr' [ RD.td' [ RD.text "Tabs" ]
                                    , RD.td' [ RD.pre' [ RD.code' [ RD.text "split" ] ] ]
                                    , RD.td' [ RD.text "Prism" ]
                                    ]
                           , RD.tr' [ RD.td' [ RD.text "Lists" ]
                                    , RD.td' [ RD.pre' [ RD.code' [ RD.text "foreach" ] ] ]
                                    , RD.td' [ RD.text "Traversal" ]
                                    ]
                           ]
               ]
  ]

slide13 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide13 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Task List Example" ]
  , RD.p'  [ RD.text "Putting it all together" ]
  , RD.p'  [ RD.a [ RP.href "http://functorial.com/purescript-thermite-todomvc/"
                   , RP.target "_blank"
                   ]
                   [ RD.text "Demo" ]
            ]
  , RD.p'  [ RD.a [ RP.href "https://github.com/paf31/purescript-thermite/blob/master/test"
                   , RP.target "_blank"
                   ]
                   [ RD.text "Code" ]
            ]
  ]

slide14 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide14 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Async" ]
  , RD.p'  [ RD.text "Or \"What's this cotransform thing about?\"" ]
  ]

slide15 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide15 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Coroutines" ]
  , RD.p'  [ RD.text "The purescript-coroutines library defines the Coroutine abstraction, which generalizes" ]
  , RD.ul'  [ RD.li' [ RD.text "Data producers" ]
             , RD.li' [ RD.text "Data consumers" ]
             , RD.li' [ RD.text "Data transformers" ]
             ]
  , RD.p'  [ RD.text "over some base monad (usually Aff)" ]
  , RD.p'  [ RD.text "This is a good model for various asynchronous processes:" ]
  , RD.ul'  [ RD.li' [ RD.text "AJAX" ]
             , RD.li' [ RD.text "Websockets" ]
             , RD.li' [ RD.text "Node streams" ]
             ]
  ]

slide16 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide16 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
    [ RD.h1' [ RD.text "Producers and Consumers" ]
    , RD.p'  [ RD.text "Coroutines are free monad transformers" ]
    , RD.pre' [ RD.code' [ RD.text example ] ]
    ]
  where
    example = """type Co f m = FreeT f m

data Emit o a = Emit o a
type Producer o = Co (Emit o)

newtype Await i a = Await (i -> a)
type Consumer i = Co (Await i)"""

slide17 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide17 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
    [ RD.h1' [ RD.text "Transformers" ]
    , RD.p'  [ RD.text "Transformers take one input and return one output" ]
    , RD.pre' [ RD.code' [ RD.text example ] ]
    ]
  where
    example = """newtype Transform i o a = Transform (i -> Tuple o a)
type Transformer i o = Co (Transform i o)

data CoTransform i o a = CoTransform o (i -> a)
type CoTransformer i o = Co (CoTransform i o)

cotransform
  ∷ ∀ m i o
  . Monad m
  ⇒ o
  → CoTransformer i o m i"""

slide18 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide18 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "React as a Transformer" ]
  , RD.p'   [ RD.text "React yields the current state and waits for state update (asynchronously!)" ]
  , RD.pre' [ RD.code' [ RD.text "react :: CoTransformer (σ -> σ) (Maybe σ) (Aff eff) Unit" ] ]
  , RD.p'   [ RD.text "We can fuse this with our update coroutine:" ]
  , RD.pre' [ RD.code' [ RD.text "CoTransformer (Maybe σ) (σ → σ) (Aff eff) Unit" ] ]
  ]

slide19 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide19 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Try Thermite" ]
  , RD.p'  [ RD.text "Try Thermite in the browser and see results immediately:" ]
  , RD.p'  [ RD.a [ RP.href "http://paf31.github.io/try-thermite/"
                  , RP.target "_blank"
                  ]
                  [ RD.text "Try it now" ]
           ]
  ]

slide20 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide20 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Questions?" ] ]

slidesComponent :: forall props eff. T.Spec eff SlidesState props SlidesAction
slidesComponent =
    fold [ slide 0 slide0
         , slide 1 slide1
         , slide 2 slide2
         , slide 3 slide3
         , slide 4 slide4
         , slide 5 slide5
         , slide 6 slide6
         , slide 7 slide7
         , slide 8 slide8
         , slide 9 slide9
         , slide 10 slide10
         , slide 11 slide11
         , slide 12 slide12
         , slide 13 slide13
         , slide 14 slide14
         , slide 15 slide15
         , slide 16 slide16
         , slide 17 slide17
         , slide 18 slide18
         , slide 19 slide19
         , slide 20 slide20
         ]
  where
    slide n = T.split (slideNumberIs n)

main :: Eff (dom :: DOM.DOM) Unit
main = void do
  let component = T.createClass (navbar <> slidesComponent) initialState
  document <- DOM.window >>= DOM.document
  container <- unsafePartial (fromJust <<< toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document))
  RDOM.render (R.createFactory component {}) container
