module Competition.UI.Body where

import Prelude

import Effect.Class (class MonadEffect)
import Competition.UI.Overview as Overview
import Competition.Pages (Page (..), PageAction (..))
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type State = { page :: Page }

type ChildSlots =
  ( body :: Overview.Slot PageAction
  )

body :: Proxy "body"
body = Proxy

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { page: Overview }

render :: forall m. MonadEffect m => State -> H.ComponentHTML PageAction ChildSlots m
render state =
  case state.page of
    Overview -> HH.slot body unit Overview.component unit (\x -> x)
    _ -> HH.text "Another page"

handleAction :: forall o m. PageAction -> H.HalogenM State PageAction ChildSlots o m Unit
handleAction = case _ of
  NavigatePage page ->  H.modify_ (_ { page = page })
  CreatedNewCompetition competition -> H.modify_ (_ { page = Edit competition})