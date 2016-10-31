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
import Data.List (List, fromFoldable)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.String (length, joinWith)
import Data.Time.Duration (Seconds, convertDuration)
import Data.Tuple (fst, Tuple(..))
import Partial.Unsafe (unsafePartial)

type SharedState =
  { counter1 :: Int
  , counter2 :: Int
  , counter3 :: Int
  , counter4 :: Int
  , counterList :: List Int
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

counterList :: forall a b r. Lens { counterList :: a | r } { counterList :: b | r } a b
counterList = lens _.counterList (_ { counterList = _ })

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
    , counterList: fromFoldable [1, 2, 3]
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
  | IncrementN Int

derive instance eqSlidesAction :: Eq SlidesAction

increment1 :: Prism' SlidesAction Unit
increment1 = only Increment1

increment2 :: Prism' SlidesAction Unit
increment2 = only Increment2

increment3 :: Prism' SlidesAction Unit
increment3 = only Increment3

increment4 :: Prism' SlidesAction Unit
increment4 = only Increment4

incrementN :: Prism' SlidesAction (Tuple Int Unit)
incrementN = prism (IncrementN <<< fst)
               case _ of
                 IncrementN n -> Right (Tuple n unit)
                 other -> Left other

first :: SlidesState -> SlidesState
first = set slideNumber 0

numberOfSlides :: Int
numberOfSlides = 33

next :: SlidesState -> SlidesState
next = over slideNumber ((_ `mod` numberOfSlides) <<< add 1)

back :: SlidesState -> SlidesState
back = over slideNumber ((_ `mod` numberOfSlides) <<< add (numberOfSlides - 1))

last :: SlidesState -> SlidesState
last = set slideNumber (numberOfSlides - 1)

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
  , RD.p   [ RP.className "center" ]
           [ RD.text "Phil Freeman" ]
  , RD.p   [ RP.className "center" ]
           [ RD.code' [ RD.text "paf31/codemesh2016" ] ]
  ]

hello :: forall eff props. T.Spec eff SharedState props SlidesAction
hello = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Hello!" ]
  , RD.ul' [ RD.li' [ RD.text "Twitter/GitHub: "
                    , RD.code' [ RD.text "paf31" ]
                    ]
           , RD.li' [ RD.text "I write Haskell" ]
           , RD.li' [ RD.text "I work on PureScript" ]
           ]
  ]

slide1 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide1 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "This Talk" ]
  , RD.ul' [ RD.li' [ RD.text "Thermite API intro" ]
           , RD.li' [ RD.text "Techniques discovered" ]
           , RD.li' [ RD.text "Will assume some Haskell knowledge (e.g. Monad)" ]
           ]
  ]

slide2 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide2 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Intro" ]
  , RD.p'  [ RD.text "Thermite is a" ]
  , RD.ul' [ RD.li' [ RD.text "small (~200 LOC)" ]
           , RD.li' [ RD.text "stable (v1.0!) 😀" ]
           , RD.li' [ RD.text "React-based" ]
           ]
  , RD.p'  [ RD.text "UI library for PureScript" ]
  , RD.p'  [ RD.text "inspired by Elm, react-blaze and OpticUI" ]
  ]

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
      , "  render send _ count _ ="
      , "    [ button [ onClick \\_ → send Increment ]"
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
  , RD.ul' [ RD.li' [ RD.text "State: record types and lists" ]
           , RD.li' [ RD.text "Action: sum types" ]
           , RD.li' [ RD.text "Render/Update: lenses, prisms and traversals" ]
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
    , RD.p'   [ RD.text "The state type for two counters is" ]
    , RD.pre' [ RD.code' [ RD.text "Tuple CounterState CounterState" ] ]
    , RD.p'   [ RD.text "i.e. one "
              , RD.code' [ RD.text "CounterState" ]
              , RD.text " for each counter"
              ]
    , RD.p'   [ RD.text "We can access the parts using lenses:" ]
    , RD.pre' [ RD.code' [ RD.text example ] ]
    ]
  where
    example = joinWith "\n"
      [ "_1 ∷ Lens' (Tuple a b) a"
      , "_2 ∷ Lens' (Tuple a b) b"
      ]

slide9 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide9 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
    [ RD.h1'  [ RD.text "Two Counters" ]
    , RD.p'   [ RD.text "The corresponding action type is" ]
    , RD.pre' [ RD.code' [ RD.text "Either CounterAction CounterAction" ] ]
    , RD.p'   [ RD.text "i.e. we can either" ]
    , RD.ul'  [ RD.li' [ RD.text "Update the "
                       , RD.code' [ RD.text "Left" ]
                       , RD.text " counter, or"
                       ]
              , RD.li' [ RD.text "update the "
                       , RD.code' [ RD.text "Right" ]
                       , RD.text " counter"
                       ]
              ]
    , RD.p'   [ RD.text "We can handle the parts using prisms:" ]
    , RD.pre' [ RD.code' [ RD.text example ] ]
    ]
  where
    example = joinWith "\n"
      [ "_Left  ∷ Prism' (Either a b) a"
      , "_Right ∷ Prism' (Either a b) b"
      ]

slide10 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide10 = intro <> counters <> outro
  where
    intro = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
      [ RD.h1'  [ RD.text "Focusing" ]
      , RD.p'   [ RD.text "Our component becomes:" ]
      , RD.pre' [ RD.code' [ RD.text "focus _1 _Left counter <> focus _2 _Right counter" ] ]
      ]

    counters = T.focus counter3 increment3 counter
            <> T.focus counter4 increment4 counter

    outro = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
      [ RD.p'   [ RD.text "Note:" ]
      , RD.ul'  [ RD.li' [ RD.text "Lenses and prisms can be derived in most cases" ]
                , RD.li' [ RD.text "PureScript can infer the correct lenses and prisms in some cases" ]
                ]
      ]

slide11 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide11 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Many Counters" ]
  , RD.p'   [ RD.text "If we have many counters, the state type becomes" ]
  , RD.pre' [ RD.code' [ RD.text "List CounterState" ] ]
  , RD.p'   [ RD.text "and the action type becomes" ]
  , RD.pre' [ RD.code' [ RD.text "Tuple Int CounterAction" ] ]
  , RD.p'   [ RD.text "Note: The action type now includes the index of the component to update" ]
  , RD.p'   [ RD.text "In OpticUI, this is generalized to an (indexed) Traversal of states." ]
  ]

slide12 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide12 = intro <> counters <> outro
  where
    intro = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
      [ RD.h1'  [ RD.text "Many Counters" ]
      , RD.p'   [ RD.text "Our component becomes:" ]
      , RD.pre' [ RD.code' [ RD.text "foreach \\_ → counter" ] ]
      ]

    counters = T.focus counterList incrementN (T.foreach \_ -> counter)

    outro = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
      [ RD.p' [ RD.text "See the todo list example for a more involved example" ]
      ]

slide13 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide13 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1' [ RD.text "Summary" ]
  , RD.table' [ RD.thead' [ RD.th' [ RD.text "Component" ]
                           , RD.th' [ RD.text "Function" ]
                           , RD.th' [ RD.text "Optic" ]
                           ]
               , RD.tbody' [ RD.tr' [ RD.td' [ RD.text "Pair (same state)" ]
                                    , RD.td' [ RD.code' [ RD.text "(<>)" ] ]
                                    , RD.td' [ RD.text "" ]
                                    ]
                           , RD.tr' [ RD.td' [ RD.text "Pair (independent)" ]
                                    , RD.td' [ RD.code' [ RD.text "focus" ] ]
                                    , RD.td' [ RD.text "Lens" ]
                                    ]
                           , RD.tr' [ RD.td' [ RD.text "List" ]
                                    , RD.td' [ RD.code' [ RD.text "foreach" ] ]
                                    , RD.td' [ RD.text "Traversal" ]
                                    ]
                           ]
               ]
  ]

slide14 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide14 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "A Brief History of the API" ]
  , RD.ul'  [ RD.li' [ RD.text "v0.1: Elm arch + React" ]
            , RD.li' [ RD.text "v0.3: Monadic actions" ]
            , RD.li' [ RD.text "..." ]
            , RD.li' [ RD.text "v0.10: Use purescript-react" ]
            ]
  ]

slide15 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide15 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "OpticUI" ]
  , RD.pre  [ RP.className "center" ]
            [ RD.code' [ RD.text "type UI s = s → (s → Eff eff Unit) → Eff eff HTML" ] ]
  , RD.ul'  [ RD.li' [ RD.text "Also React-based, with a lensy API" ]
            , RD.li' [ RD.text "See "
                     , RD.code' [ RD.text "zrho/purescript-opticui" ]
                     ]
            ]
  ]

slide16 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide16 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "OpticUI" ]
  , RD.pre  [ RP.className "center" ]
            [ RD.code' [ RD.text "type UI s = s → (s → Eff eff Unit) → Eff eff HTML" ] ]
  , RD.p'   [ RD.text "Not a "
            , RD.code' [ RD.text "Functor" ]
            , RD.text " or a "
            , RD.code' [ RD.text "Monad" ]
            ]
  ]

slide17 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide17 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
    [ RD.h1'  [ RD.text "OpticUI" ]
    , RD.pre  [ RP.className "center" ]
              [ RD.code' [ RD.text "type UI' s t = s → (t → Eff eff Unit) → Eff eff HTML" ] ]
    , RD.p'  [ RD.code' [ RD.text "UI'" ]
             , RD.text " is a "
             , RD.code' [ RD.text "Profunctor" ]
             ]
    , RD.pre' [ RD.code' [ RD.text example ] ]
    ]
  where
    example = joinWith "\n"
      [ "class Profunctor p where"
      , "  dimap ∷ (c → a) → (b → d) → p a b → p c d"
      ]

slide18 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide18 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
    [ RD.h1'  [ RD.text "OpticUI" ]
    , RD.pre  [ RP.className "center" ]
              [ RD.code' [ RD.text "type UI' s t = s → (t → Eff eff Unit) → Eff eff HTML" ] ]
    , RD.p'   [ RD.text "In fact it is a "
              , RD.code' [ RD.text "Strong" ]
              , RD.text " profunctor"
              ]
    , RD.pre' [ RD.code' [ RD.text example ] ]
    ]
  where
    example = joinWith "\n"
      [ "class Strong p where"
      , "  strength ∷ p a b → p (Tuple a c) (Tuple b c)"
      ]

slide19 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide19 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Pure Profunctor Lenses" ]
  , RD.p    [ RP.className "center" ]
            [ RD.img [ RP.src "images/profunctor-lenses.png" ] [] ]
  ]

slide20 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide20 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Pure Profunctor Lenses" ]
  , RD.p'   [ RD.text "An alternative formulation of lenses" ]
  , RD.pre' [ RD.code' [ RD.text "type Lens a b = ∀ p. Strong p ⇒ p b b -> p a a" ] ]
  , RD.p'   [ RD.text "See "
            , RD.code' [ RD.text "purescript-profunctor-lenses" ]
            ]
  ]

slide21 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide21 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Pure Profunctor Lenses" ]
  , RD.p'   [ RD.text "In other words, we have:" ]
  , RD.pre' [ RD.code' [ RD.text "focus ∷ Lens a b → UI' b → UI' a" ] ]
  , RD.p'   [ RD.text "If you have a "
            , RD.code' [ RD.text "Strong" ]
            , RD.text " profunctor, you get these operations for free!"
            ]
  ]

slide22 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide22 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Other optics" ]
  , RD.p'   [ RD.text "What optics correspond to operations on components?" ]
  , RD.table' [ RD.thead' [ RD.th' [ RD.text "Component" ]
                           , RD.th' [ RD.text "Function" ]
                           , RD.th' [ RD.text "Optic" ]
                           ]
               , RD.tbody' [ RD.tr' [ RD.td' [ RD.text "Pair" ]
                                    , RD.td' [ RD.code' [ RD.text "focus" ] ]
                                    , RD.td' [ RD.text "Lens" ]
                                    ]
                           , RD.tr' [ RD.td' [ RD.text "List" ]
                                    , RD.td' [ RD.code' [ RD.text "foreach" ] ]
                                    , RD.td' [ RD.text "(Indexed) Traversal" ]
                                    ]
                           , RD.tr' [ RD.td' [ RD.text "Tabs" ]
                                    , RD.td' [ RD.code' [ RD.text "split" ] ]
                                    , RD.td' [ RD.text "Prism" ]
                                    ]
                           , RD.tr' [ RD.td' [ RD.text "Cards" ]
                                    , RD.td' [ RD.code' [ RD.text "?" ] ]
                                    , RD.td' [ RD.text "Grate" ]
                                    ]
                           ]
               ]
  ]

slide23 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide23 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Finding a place for Async/Effects" ]
  , RD.p'   [ RD.text "Real applications need to deal with (asynchronous) effects:" ]
  , RD.ul'  [ RD.li' [ RD.text "AJAX" ]
            , RD.li' [ RD.text "Websockets" ]
            , RD.li' [ RD.text "Events" ]
            , RD.li' [ RD.text "etc." ]
            ]
  , RD.p'   [ RD.text "These fit into the "
            , RD.code' [ RD.text "Profunctor" ]
            , RD.text " model nicely"
            ]
  ]

slide24 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide24 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
    [ RD.h1'  [ RD.text "Aff" ]
    , RD.p'   [ RD.text "An asynchronous effect monad" ]
    , RD.pre' [ RD.code' [ RD.text example ] ]
    ]
  where
    example = joinWith "\n"
      [ "data Aff eff a"
      , ""
      , "instance monadAff ∷ Monad (Aff eff)"
      , ""
      , "example ∷ Aff _ (Array Result)"
      , "example = do"
      , "  res1 <- get \"/request/1\""
      , "  res2 <- get \"/request/2\""
      , "  pure [res1, res2]"
      ]

slide25 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide25 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
    [ RD.h1'  [ RD.text "Coroutines" ]
    , RD.p'   [ RD.text "We want to support multiple incremental return values" ]
    , RD.pre' [ RD.code' [ RD.text example ] ]
    ]
  where
    example = joinWith "\n"
      [ "data Producer o m a"
      , ""
      , "emit ∷ ∀ o m. o → Producer o m Unit"
      , ""
      , "instance monadProducer ∷ Monad m ⇒ Monad (Producer o m)"
      , ""
      , "example ∷ Producer Result (Aff _) Unit"
      , "example = do"
      , "  res1 <- liftAff $ get \"/request/1\""
      , "  emit res1"
      , "  res2 <- liftAff $ get \"/request/2\""
      , "  emit res2"
      ]

slide26 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide26 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Taxonomy of Coroutines" ]
  , RD.table' [ RD.thead' [ RD.th' [ RD.text "Coroutine" ]
                          , RD.th' [ RD.text "Function" ]
                          , RD.th' [ RD.text "Example" ]
                          ]
              , RD.tbody' [ RD.tr' [ RD.td' [ RD.text "Process" ]
                                   , RD.td' [ RD.code' [ RD.text "yield ∷ Process m Unit" ] ]
                                   , RD.td' [ RD.text "" ]
                                   ]
                          , RD.tr' [ RD.td' [ RD.text "Producer o" ]
                                   , RD.td' [ RD.code' [ RD.text "emit ∷ o → Producer o m Unit" ] ]
                                   , RD.td' [ RD.text "File Upload" ]
                                   ]
                          , RD.tr' [ RD.td' [ RD.text "Consumer i" ]
                                   , RD.td' [ RD.code' [ RD.text "await ∷ Consumer i m i" ] ]
                                   , RD.td' [ RD.text "File Download" ]
                                   ]
                          , RD.tr' [ RD.td' [ RD.text "Transformer i o" ]
                                   , RD.td' [ RD.code' [ RD.text "transform ∷ (i → o) → Transformer i o m Unit" ] ]
                                   , RD.td' [ RD.text "Websocket" ]
                                   ]
                          ]
              ]
  ]

slide27 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide27 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Coroutine Fusion" ]
  , RD.p'   [ RD.text "We can fuse Coroutines in many ways:" ]
  , RD.table' [ RD.thead' [ RD.th' [ RD.text "A" ]
                          , RD.th' [ RD.text "B" ]
                          , RD.th' [ RD.text "Result" ]
                          ]
              , RD.tbody' [ RD.tr' [ RD.td' [ RD.text "Producer a" ]
                                   , RD.td' [ RD.text "Producer b" ]
                                   , RD.td' [ RD.text "Producer (Tuple a b)" ]
                                   ]
                          , RD.tr' [ RD.td' [ RD.text "Consumer a" ]
                                   , RD.td' [ RD.text "Consumer b" ]
                                   , RD.td' [ RD.text "Consumer (Tuple a b)" ]
                                   ]
                          , RD.tr' [ RD.td' [ RD.text "Producer a" ]
                                   , RD.td' [ RD.text "Consumer a" ]
                                   , RD.td' [ RD.text "Process" ]
                                   ]
                          , RD.tr' [ RD.td' [ RD.text "Producer a" ]
                                   , RD.td' [ RD.text "Transformer a b" ]
                                   , RD.td' [ RD.text "Producer b" ]
                                   ]
                          , RD.tr' [ RD.td' [ RD.text "Consumer b" ]
                                   , RD.td' [ RD.text "Transformer a b" ]
                                   , RD.td' [ RD.text "Consumer a" ]
                                   ]
                          , RD.tr' [ RD.td' [ RD.text "Transformer a b" ]
                                   , RD.td' [ RD.text "Transformer b c" ]
                                   , RD.td' [ RD.text "Transformer a c" ]
                                   ]
                          ]
              ]
  ]

slide28 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide28 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "React is a Coroutine" ]
  , RD.p'   [ RD.text "We can model React's internal state as:" ]
  , RD.pre' [ RD.code' [ RD.text "react ∷ Consumer (state → state) (Aff _) Unit" ] ]
  , RD.p'   [ RD.text "If we can model our application as" ]
  , RD.pre' [ RD.code' [ RD.text "app ∷ Producer (state → state) (Aff _) Unit" ] ]
  , RD.p'   [ RD.text "then we can use fusion to run it" ]
  ]

slide29 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide29 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Main Idea" ]
  , RD.p'   [ RD.text "The "
            , RD.code' [ RD.text "UI" ]
            , RD.text " type can now become"
            ]
  , RD.pre' [ RD.code' [ RD.text "type UI'' s t = s → Consumer (s → t) (Aff eff) Unit → Eff eff HTML" ] ]
  , RD.p'   [ RD.text "Since this is still "
            , RD.code' [ RD.text "Strong" ]
            , RD.text ", we can continue using the "
            , RD.code' [ RD.text "Lens" ]
            , RD.text " operations!"
            ]
  ]

slide30 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide30 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Conclusion" ]
  , RD.p'   [ RD.text "(Strong) profunctors are a very useful abstraction and design tool!" ]
  ]

slide31 :: forall eff props. T.Spec eff SharedState props SlidesAction
slide31 = T.simpleSpec T.defaultPerformAction \dispatch _ state _ ->
  [ RD.h1'  [ RD.text "Links, questions?" ]
  , RD.ul'  [ RD.li' [ RD.text "paf31/purescript-thermite" ]
            , RD.li' [ RD.text "paf31.github.io/try-thermite" ]
            , RD.li' [ RD.text "purescript-contrib/purescript-profunctor-lenses" ]
            , RD.li' [ RD.text "zrho/purescript-opticui" ]
            , RD.li' [ RD.text "paf31/codemesh2016" ]
            ]
  ]

slidesComponent :: forall props eff. T.Spec eff SlidesState props SlidesAction
slidesComponent = fold
    [ slide 0  slide0
    , slide 1  hello
    , slide 2  slide1
    , slide 3  slide2
    , slide 4  slide3
    , slide 5  slide4
    , slide 6  slide5
    , slide 7  slide6
    , slide 8  slide7
    , slide 9  slide8
    , slide 10  slide9
    , slide 11 slide10
    , slide 12 slide11
    , slide 13 slide12
    , slide 14 slide13
    , slide 15 slide14
    , slide 16 slide15
    , slide 17 slide16
    , slide 18 slide17
    , slide 19 slide18
    , slide 20 slide19
    , slide 21 slide20
    , slide 22 slide21
    , slide 23 slide22
    , slide 24 slide23
    , slide 25 slide24
    , slide 26 slide25
    , slide 27 slide26
    , slide 28 slide27
    , slide 29 slide28
    , slide 30 slide29
    , slide 31 slide30
    , slide 32 slide31
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
