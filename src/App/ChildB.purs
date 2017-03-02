module App.ChildB
  ( Query
  , component
  )
where

import App.Monad    ( App )
import Data.Maybe   ( Maybe (..) )
import Halogen      as H
import Halogen.HTML as HH
import Prelude

type State = Unit
data Query a = NoOp a
type Input = Unit
type Output = Void

eval :: Query ~> H.ComponentDSL State Query Output App
eval = case _ of
  NoOp next -> pure next

render :: State -> H.ComponentHTML Query
render _ = HH.div_ [ HH.text "ChildB: ", HH.text "Nothing to see here." ]

component :: H.Component HH.HTML Query Input Output App
component = H.component
  { eval
  , initialState : const unit
  , receiver     : const Nothing
  , render
  }
