module Competition.UI.Body where

import Prelude

import Competition.UI.Overview as Overview
import Competition.Pages (Page (..))
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type State = { page :: Page }

data Action = NavigateButton Page

type ChildSlots =
  ( body :: Overview.Slot Unit
  )

body :: Proxy "body"
body = Proxy

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { page: Overview }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  case state.page of
    Overview -> HH.slot body unit Overview.component unit NavigateButton
    _ -> HH.text "Another page"

handleAction :: forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  NavigateButton page ->  H.modify_ (_ { page = page })