module App.Parent where

import App.ChildA                    as ChildA
import App.ChildB                    as ChildB
import App.Monad                     ( App )
import Data.Either.Nested            ( Either2 )
import Data.Functor.Coproduct.Nested ( Coproduct2 )
import Data.Maybe                    ( Maybe (..) )
import Halogen                       as H
import Halogen.Component.ChildPath   as CP
import Halogen.HTML                  as HH
import Halogen.HTML.Events           as HE
import Prelude

-- Whether ChildA or ChildB is displayed
type State = Boolean

data Query a = SwitchChild a

type Input = Unit
type Output = Void

type Slot = Either2 Unit Unit
type ChildQuery = Coproduct2 ChildA.Query ChildB.Query

eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Output App
eval = case _ of
  SwitchChild next -> do
    H.modify not
    pure next

render :: State -> H.ParentHTML Query ChildQuery Slot App
render state =
  let
    currentChild =
      if state
        then HH.slot' CP.cp1 unit ChildA.component unit absurd
        else HH.slot' CP.cp2 unit ChildB.component unit absurd
  in
    HH.div_
      [ currentChild
      , HH.button
          [ HE.onClick $ HE.input_ (SwitchChild) ]
          [ HH.text "Switch View" ]
      ]

component :: H.Component HH.HTML Query Input Output App
component = H.parentComponent
  { eval
  , initialState : const false
  , receiver     : const Nothing
  , render
  }
