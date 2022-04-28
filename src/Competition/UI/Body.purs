module Competition.UI.Body
  ( ChildSlots
  , State
  , component
  , initialState
  , render
  )
  where

import Prelude

import Effect.Console (log)
import Effect.Class (class MonadEffect)
import Competition.UI.Overview as Overview
import Competition.UI.Edit as Edit
import Competition.UI.Show as Show
import Competition.Pages (Page (..), PageAction (..))
import Halogen as H
import Halogen.HTML as HH

type State = { page :: Page }

type ChildSlots =
  ( overview :: Overview.Slot PageAction
  , edit :: Edit.Slot PageAction
  , show :: Show.Slot PageAction
  )

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction= handleAction
      }
    }

initialState :: forall i. i -> State
initialState _ = { page: OverviewPage }

render :: forall m. MonadEffect m => State -> H.ComponentHTML PageAction ChildSlots m
render state = do
  case state.page of
    OverviewPage -> HH.slot Overview.slot unit Overview.component unit (\x -> x)
    EditPage competition -> HH.slot Edit.slot unit (Edit.component competition) unit (\x -> x)
    ShowPage competition -> HH.slot Show.slot unit (Show.component competition) unit (\x -> x)

handleAction :: forall o m. MonadEffect m => PageAction -> H.HalogenM State PageAction ChildSlots o m Unit
handleAction = case _ of
  NavigatePage page -> do
    H.liftEffect (log "Navigate page")
    H.modify_ (_ { page = page })
  CreatedNewCompetition competition -> 
    H.modify_ (_ { page = EditPage competition})

