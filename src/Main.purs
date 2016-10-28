module Main (main) where

import Prelude
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Timer (TIMER, setInterval)
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Int (floor)
import Data.Lens (Prism', Lens, _1, _2, _Left, _Right, only, over, set, prism, lens)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.String (length, joinWith)
import Data.Time.Duration (Seconds, convertDuration)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

type SharedState =
  { counter1 :: Int
  , counter2 :: Int
  , counter3 :: Int
  , counter4 :: Int
  }

type SlidesState =
  { slideNumber   :: Int
  , sharedState   :: SharedState
  }

slideNumber :: forall a b r. Lens { slideNumber :: a | r } { slideNumber :: b | r } a b
slideNumber = lens _.slideNumber (_ { slideNumber = _ })

sharedState :: forall a b r. Lens { sharedState :: a | r } { sharedState :: b | r } a b
sharedState = lens _.sharedState (_ { sharedState = _ })

counter1 :: forall a b r. Lens { counter1 :: a | r } { counter1 :: b | r } a b
counter1 = lens _.counter1 (_ { counter1 = _ })

counter2 :: forall a b r. Lens { counter2 :: a | r } { counter2 :: b | r } a b
counter2 = lens _.counter2 (_ { counter2 = _ })

counter3 :: forall a b r. Lens { counter3 :: a | r } { counter3 :: b | r } a b
counter3 = lens _.counter3 (_ { counter3 = _ })

counter4 :: forall a b r. Lens { counter4 :: a | r } { counter4 :: b | r } a b
counter4 = lens _.counter4 (_ { counter4 = _ })

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
    { counter1: 0
    , counter2: 0
    , counter3: 0
    , counter4: 0
    }
  }

data SlidesAction
  = First
  | Back
  | Next
  | Last
  | Increment1
  | Increment2
  | Increment3
  | Increment4

derive instance eqSlidesAction :: Eq SlidesAction

increment1 :: Prism' SlidesAction Unit
increment1 = only Increment1

increment2 :: Prism' SlidesAction Unit
increment2 = only Increment2

increment3 :: Prism' SlidesAction Unit
increment3 = only Increment3

increment4 :: Prism' SlidesAction Unit
increment4 = only Increment4

first :: SlidesState -> SlidesState
first = set slideNumber 0

next :: SlidesState -> SlidesState
next = over slideNumber ((_ `mod` 21) <<< add 1)

back :: SlidesState -> SlidesState
back = over slideNumber ((_ `mod` 21) <<< add 20)

last :: SlidesState -> SlidesState
last = set slideNumber 20

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
  [ RD.h1 [ RP.className "center" ]
          [ RD.text "Front End Development with PureScript and Thermite" ]
  , RD.p [ RP.className "center" ]
         [ RD.text "Phil Freeman" ]
  , RD.p [ RP.className "center" ]
         [ RD.code' [ RD.text "paf31/codemesh2016" ] ]
  ]

slide1 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide1 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Intro" ]
  , RD.p'  [ RD.text "Thermite is a" ]
  , RD.ul' [ RD.li' [ RD.text "small (~200 LOC)" ]
           , RD.li' [ RD.text "stable (v1.0!) 😀" ]
           , RD.li' [ RD.text "React-based" ]
           ]
  , RD.p'  [ RD.text "UI library for PureScript" ]
  , RD.p'  [ RD.text "inspired by Elm, react-blaze and OpticUI" ]
  ]

slide2 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide2 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "This Talk" ]
  , RD.ul' [ RD.li' [ RD.text "History of the API" ]
           , RD.li' [ RD.text "Techniques discovered" ]
           , RD.li' [ RD.text "Examples" ]
           ]
  ]

-- slide2 :: forall eff props. T.Spec eff SharedState props SlidesAction
-- slide2 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
--   [ RD.h1' [ RD.text "Problems for UI Libraries" ]
--   , RD.p'  [ RD.text "UI libraries have to solve the following problems:" ]
--   , RD.ul' [ RD.li' [ RD.text "Multiple components" ]
--            , RD.li' [ RD.text "3rd party components" ]
--            , RD.li' [ RD.text "Async code (AJAX etc.)" ]
--            ]
--   , RD.p'  [ RD.text "Thermite uses PureScript's advanced type system features to solve these problems:" ]
--   , RD.ul' [ RD.li' [ RD.text "Lenses" ]
--            , RD.li' [ RD.text "Coroutines" ]
--            ]
--   ]

slide3 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide3 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "The Elm Architecture (TEA) 😎" ]
  , RD.p'  [ RD.text "Components are defined by:" ]
  , RD.ul' [ RD.li' [ RD.text "A state/model type" ]
           , RD.li' [ RD.text "An action/message type" ]
           , RD.li' [ RD.text "A render/view function" ]
           , RD.li' [ RD.text "An update function" ]
           ]
  , RD.p'  [ RD.text "Thermite was originally a port of TEA to PureScript/React" ]
  , RD.p'  [ RD.text "Now see "
           , RD.code' [ RD.text "purescript-pux" ]
           ]
  ]

slide4 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide4 = intro <> T.focus counter1 increment1 counter
  where
    intro = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
      [ RD.h1' [ RD.text "Counter Component" ]
      , RD.pre' [ RD.code' [ RD.text example ] ]
      ]
    example = joinWith "\n"
      [ "type CounterState = Int"
      , "data CounterAction = Increment"
      , ""
      , "counter = T.simpleSpec performAction render where"
      , "  render dispatch _ count _ ="
      , "    [ button [ onClick \\_ → dispatch Increment ]"
      , "             [ text (show count) ]"
      , "    ]"
      , ""
      , "  performAction Increment _ _ = void $ modifyState (add 1)"
      ]

slide5 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide5 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Component Composition 😕" ]
  , RD.p'  [ RD.text "How should we compose these components?" ]
  , RD.ul' [ RD.li' [ RD.text "State: record types" ]
           , RD.li' [ RD.text "Action: sum types" ]
           , RD.li' [ RD.text "Render: projection" ]
           , RD.li' [ RD.text "Update: pattern matching" ]
           ]
  , RD.p'  [ RD.text "(i.e. lots of repetition/recipes)" ]
  ]

slide6 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide6 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Composition in Thermite" ]
  , RD.p'   [ RD.text "Preview:" ]
  , RD.ul' [ RD.li' [ RD.text "State: record types" ]
           , RD.li' [ RD.text "Action: sum types" ]
           , RD.li' [ RD.text "Render/Update: lenses and prisms" ]
           ]
  , RD.p'   [ RD.text "These slides are composed from several small components" ]
  , RD.p'   [ RD.text "(check the source!)" ]
  ]

slide7 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide7 = intro <> counters <> outro
  where
    intro = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
      [ RD.h1' [ RD.text "Monoidal composition" ]
      , RD.p'  [ RD.text "The Monoid instance gives us one type of composition:" ]
      , RD.pre' [ RD.code' [ RD.text "twoCounters = counter <> counter" ] ]
      ]

    counters = T.focus counter2 increment2 counter
            <> T.focus counter2 increment2 counter

    outro = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
      [ RD.p' [ RD.text "... but perhaps not the one you were thinking of" ]
      , RD.p' [ RD.text "We need a way to break up the state" ]
      ]

slide8 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide8 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Two Counters" ]
  , RD.p'   [ RD.text "State:" ]
  , RD.pre' [ RD.code' [ RD.text "Tuple CounterState CounterState" ] ]
  , RD.p'   [ RD.text "Actions:" ]
  , RD.pre' [ RD.code' [ RD.text "Either CounterAction CounterAction" ] ]
  , RD.p'   [ RD.text "We can talk about the parts using lenses and prisms" ]
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

    counters = T.focus counter3 increment3 counter
            <> T.focus counter4 increment4 counter

    example = joinWith "\n"
      [ "_1     :: Lens' (Tuple a b) a"
      , "_2     :: Lens' (Tuple a b) b"
      , "_Left  :: Prism' (Either a b) a"
      , "_Right :: Prism' (Either a b) b"
      ]

-- slide10 :: forall eff props. T.Spec eff SharedState props SlidesAction
-- slide10 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
--     [ RD.h1'  [ RD.text "Tab Components" ]
--     , RD.p'   [ RD.text "Lenses let us identify smaller parts of a product of types, which is a good model for independent components." ]
--     , RD.p'   [ RD.text "Prisms let us identify smaller parts of a sum of types, which is a good model for tabbed applications:" ]
--     , RD.pre' [ RD.code' [ RD.text example ] ]
--     , RD.p'   [ RD.text "Note that" ]
--     , RD.ul' [ RD.li' [ RD.text "The action type δ does not change here" ]
--              , RD.li' [ RD.text "No information is shared between tabs" ]
--              ]
--     ]
--   where
--     example = """split
--   ∷ ∀ eff σ₁ σ₂ δ
--   . PrismP σ₁ σ₂
--   → Spec eff σ₂ δ
--   → Spec eff σ₁ δ"""
--
-- slide11 :: forall eff props. T.Spec eff SharedState props SlidesAction
-- slide11 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
--     [ RD.h1'  [ RD.text "List Components" ]
--     , RD.p'   [ RD.text "The last type of composition lets us define lists of subcomponents:" ]
--     , RD.pre' [ RD.code' [ RD.text example ] ]
--     , RD.p'   [ RD.text "In OpticUI, this is generalized to a Traversal." ]
--     , RD.p'   [ RD.text "Note that" ]
--     , RD.ul'  [ RD.li' [ RD.text "We have a whole list of states, one for each list element" ]
--               , RD.li' [ RD.text "The action type now includes the index of the component to update" ]
--               ]
--     ]
--   where
--     example = """foreach
--   ∷ ∀ eff σ δ
--   . (Int -> Spec eff σ δ)
--   → Spec eff (List σ) (Tuple Int δ)"""
--
-- slide12 :: forall eff props. T.Spec eff SharedState props SlidesAction
-- slide12 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
--   [ RD.h1' [ RD.text "Summary" ]
--   , RD.table' [ RD.thead' [ RD.th' [ RD.text "Component" ]
--                            , RD.th' [ RD.text "Function" ]
--                            , RD.th' [ RD.text "Optic" ]
--                            ]
--                , RD.tbody' [ RD.tr' [ RD.td' [ RD.text "Pair (same state)" ]
--                                     , RD.td' [ RD.pre' [ RD.code' [ RD.text "(<>)" ] ] ]
--                                     , RD.td' [ RD.text "" ]
--                                     ]
--                            , RD.tr' [ RD.td' [ RD.text "Pair (independent)" ]
--                                     , RD.td' [ RD.pre' [ RD.code' [ RD.text "focus" ] ] ]
--                                     , RD.td' [ RD.text "Lens" ]
--                                     ]
--                            , RD.tr' [ RD.td' [ RD.text "Tabs" ]
--                                     , RD.td' [ RD.pre' [ RD.code' [ RD.text "split" ] ] ]
--                                     , RD.td' [ RD.text "Prism" ]
--                                     ]
--                            , RD.tr' [ RD.td' [ RD.text "Lists" ]
--                                     , RD.td' [ RD.pre' [ RD.code' [ RD.text "foreach" ] ] ]
--                                     , RD.td' [ RD.text "Traversal" ]
--                                     ]
--                            ]
--                ]
--   ]
--
-- slide13 :: forall eff props. T.Spec eff SharedState props SlidesAction
-- slide13 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
--   [ RD.h1' [ RD.text "Task List Example" ]
--   , RD.p'  [ RD.text "Putting it all together" ]
--   , RD.p'  [ RD.a [ RP.href "http://functorial.com/purescript-thermite-todomvc/"
--                    , RP.target "_blank"
--                    ]
--                    [ RD.text "Demo" ]
--             ]
--   , RD.p'  [ RD.a [ RP.href "https://github.com/paf31/purescript-thermite/blob/master/test"
--                    , RP.target "_blank"
--                    ]
--                    [ RD.text "Code" ]
--             ]
--   ]
--
-- slide14 :: forall eff props. T.Spec eff SharedState props SlidesAction
-- slide14 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
--   [ RD.h1' [ RD.text "Async" ]
--   , RD.p'  [ RD.text "Or \"What's this cotransform thing about?\"" ]
--   ]
--
-- slide15 :: forall eff props. T.Spec eff SharedState props SlidesAction
-- slide15 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
--   [ RD.h1' [ RD.text "Coroutines" ]
--   , RD.p'  [ RD.text "The purescript-coroutines library defines the Coroutine abstraction, which generalizes" ]
--   , RD.ul'  [ RD.li' [ RD.text "Data producers" ]
--              , RD.li' [ RD.text "Data consumers" ]
--              , RD.li' [ RD.text "Data transformers" ]
--              ]
--   , RD.p'  [ RD.text "over some base monad (usually Aff)" ]
--   , RD.p'  [ RD.text "This is a good model for various asynchronous processes:" ]
--   , RD.ul'  [ RD.li' [ RD.text "AJAX" ]
--              , RD.li' [ RD.text "Websockets" ]
--              , RD.li' [ RD.text "Node streams" ]
--              ]
--   ]
--
-- slide16 :: forall eff props. T.Spec eff SharedState props SlidesAction
-- slide16 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
--     [ RD.h1' [ RD.text "Producers and Consumers" ]
--     , RD.p'  [ RD.text "Coroutines are free monad transformers" ]
--     , RD.pre' [ RD.code' [ RD.text example ] ]
--     ]
--   where
--     example = """type Co f m = FreeT f m
--
-- data Emit o a = Emit o a
-- type Producer o = Co (Emit o)
--
-- newtype Await i a = Await (i -> a)
-- type Consumer i = Co (Await i)"""
--
-- slide17 :: forall eff props. T.Spec eff SharedState props SlidesAction
-- slide17 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
--     [ RD.h1' [ RD.text "Transformers" ]
--     , RD.p'  [ RD.text "Transformers take one input and return one output" ]
--     , RD.pre' [ RD.code' [ RD.text example ] ]
--     ]
--   where
--     example = """newtype Transform i o a = Transform (i -> Tuple o a)
-- type Transformer i o = Co (Transform i o)
--
-- data CoTransform i o a = CoTransform o (i -> a)
-- type CoTransformer i o = Co (CoTransform i o)
--
-- cotransform
--   ∷ ∀ m i o
--   . Monad m
--   ⇒ o
--   → CoTransformer i o m i"""
--
-- slide18 :: forall eff props. T.Spec eff SharedState props SlidesAction
-- slide18 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
--   [ RD.h1'  [ RD.text "React as a Transformer" ]
--   , RD.p'   [ RD.text "React yields the current state and waits for state update (asynchronously!)" ]
--   , RD.pre' [ RD.code' [ RD.text "react :: CoTransformer (σ -> σ) (Maybe σ) (Aff eff) Unit" ] ]
--   , RD.p'   [ RD.text "We can fuse this with our update coroutine:" ]
--   , RD.pre' [ RD.code' [ RD.text "CoTransformer (Maybe σ) (σ → σ) (Aff eff) Unit" ] ]
--   ]
--
-- slide19 :: forall eff props. T.Spec eff SharedState props SlidesAction
-- slide19 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
--   [ RD.h1' [ RD.text "Try Thermite" ]
--   , RD.p'  [ RD.text "Try Thermite in the browser and see results immediately:" ]
--   , RD.p'  [ RD.a [ RP.href "http://paf31.github.io/try-thermite/"
--                   , RP.target "_blank"
--                   ]
--                   [ RD.text "Try it now" ]
--            ]
--   ]
--
-- slide20 :: forall eff props. T.Spec eff SharedState props SlidesAction
-- slide20 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
--   [ RD.h1' [ RD.text "Questions?" ] ]

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
        --  , slide 10 slide10
        --  , slide 11 slide11
        --  , slide 12 slide12
        --  , slide 13 slide13
        --  , slide 14 slide14
        --  , slide 15 slide15
        --  , slide 16 slide16
        --  , slide 17 slide17
        --  , slide 18 slide18
        --  , slide 19 slide19
        --  , slide 20 slide20
         ]
  where
    slide n = T.split (slideNumberIs n)

timer :: forall eff props. T.Spec eff Seconds props Seconds
timer = T.simpleSpec performAction render
  where
    render :: T.Render Seconds props Seconds
    render _ _ state _ =
        [ RD.div [ RP.className "timer" ]
                 [ RD.text (show2 mm <> ":" <> show2 ss') ]
        ]
      where
        ss = floor (unwrap state)
        mm = ss / 60
        ss' = ss `mod` 60
        show2 x | length (show x) < 2 = "0" <> show x
                | otherwise = show x

    performAction :: T.PerformAction eff Seconds props Seconds
    performAction dur _ _ = void $ T.writeState dur

main :: Eff (dom :: DOM.DOM, now :: NOW, timer :: TIMER) Unit
main = do
  startTime <- now
  let mainComponent = T.focus _1 _Left (navbar <> slidesComponent)
                   <> T.focus _2 _Right timer
  case T.createReactSpec mainComponent (Tuple initialState (wrap 0.0)) of
    { spec, dispatcher } -> void do
      let setupTimer this = void (setInterval 1000 (tick this))
          tick this = do
            newTime <- now
            let dur = convertDuration (unInstant newTime - unInstant startTime)
            dispatcher this (Right dur)
          spec' = spec { componentWillMount = setupTimer }
      document <- DOM.window >>= DOM.document
      container <- unsafePartial (fromJust <<< toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document))
      RDOM.render (R.createFactory (R.createClass spec') {}) container
