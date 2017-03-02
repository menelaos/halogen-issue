module App.ChildA
  ( Query
  , component
  )
where

import App.Monad                   ( App )
import App.Wiring                  as Wiring
import Control.Monad.Aff.Console   ( log )
import Data.Maybe                  ( Maybe (..) )
import Halogen                     as H
import Halogen.Component.Utils     ( busEventSource )
import Halogen.HTML                as HH
import Halogen.Query.EventSource   as ES
import Prelude

type State = Boolean

data Query a
  = Init a
  | Toggle a

type Input = Unit
type Output = Void

eval :: Query ~> H.ComponentDSL State Query Output App
eval = case _ of
  Init next -> do
    { bus } <- Wiring.expose
    H.subscribe $ busEventSource (\_ -> Toggle ES.Listening) bus
    pure next
  Toggle next -> do
    H.liftAff $ log "Toggle state in ChildA"
    H.modify not
    pure next

render :: State -> H.ComponentHTML Query
render state =
  HH.div_ [ HH.text "ChildA: ", HH.text $ if state then "On" else "Off" ]

component :: H.Component HH.HTML Query Input Output App
component = H.lifecycleComponent
  { eval
  , finalizer    : Nothing
  , initializer  : Just (H.action Init)
  , initialState : const false
  , receiver     : const Nothing
  , render
  }
