module App.Monad
  ( App
  , AppM
  , runApp
  )
where

import App.Effects                 ( AppEffects )
import App.Wiring                  ( Wiring (Wiring) )
import Control.Monad.Aff           ( Aff )
import Control.Monad.Aff.Class     ( class MonadAff )
import Control.Monad.Eff.Class     ( class MonadEff, liftEff )
import Control.Monad.Free          ( Free, foldFree, liftF )
import Control.Monad.Reader.Class  ( class MonadAsk )
import Prelude

data AppF eff a
  = Aff (Aff eff a)
  | Ask (Wiring -> a)

newtype AppM eff a = AppM (Free (AppF eff) a)

unAppM :: ∀ eff. AppM eff ~> Free (AppF eff)
unAppM (AppM f) = f

type App = AppM AppEffects

derive newtype instance functorAppM     :: Functor (AppM eff)
derive newtype instance applyAppM       :: Apply (AppM eff)
derive newtype instance applicativeAppM :: Applicative (AppM eff)
derive newtype instance bindAppM        :: Bind (AppM eff)
derive newtype instance monadAppM       :: Monad (AppM eff)

instance monadAffAppM :: MonadAff eff (AppM eff) where
  liftAff = AppM <<< liftF <<< Aff

instance monadEffAppM :: MonadEff eff (AppM eff) where
  liftEff = AppM <<< liftF <<< Aff <<< liftEff

instance monadAsk :: MonadAsk Wiring (AppM eff) where
  ask = AppM <<< liftF $ Ask id


runApp :: Wiring -> App ~> Aff AppEffects
runApp wiring@(Wiring { bus }) = foldFree go <<< unAppM
  where
    go :: AppF AppEffects ~> Aff AppEffects
    go = case _ of
      Aff aff        -> aff
      Ask continue   -> pure $ continue wiring
