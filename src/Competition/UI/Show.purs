module Competition.UI.Show where

import Prelude

import Effect (Effect)
import Data.Tuple (Tuple (..))
import Data.Array ((:), head, zip, range, length)
import Data.Maybe (fromMaybe')
import Partial.Unsafe (unsafeCrashWith)
import Effect.Class (class MonadEffect)
import Competition.Competition (Competition, Apparatus, modifyCompetition, apparatusName, reorder)
import Competition.Participant (Lane (..), Participant, updateParticipant)
import Competition.Pages (Page (..), PageAction (..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy (..))

type Slot action = forall q. H.Slot q action Unit

data Action 
  = ToOverview
  | ChangeApparatus Apparatus
  | ChangeInput (Competition -> Effect Competition)

type State = 
  { competition :: Competition
  , curApparatus :: Apparatus
  }

slot :: Proxy "show"
slot = Proxy

component :: forall q i m. MonadEffect m => Competition -> H.Component q i PageAction m
component competition = H.mkComponent 
  { initialState: initialState competition
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

initialState :: forall i. Competition -> i -> State
initialState competition _ = { competition, curApparatus: fromMaybe' (\_ -> unsafeCrashWith "There should always be an apparatus") $ head competition.competitionApparatuses } 

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div [cls "container"] (header : menuBar : [content])
  where
    rowDiv = HH.div [cls "row"]
    header = rowDiv
      [ HH.div [cls "col-6"] [HH.h1_ [HH.text $ "Weergave: " <> state.competition.competitionName]]
      , HH.div [cls "col-6"] [
        HH.button
          [ HE.onClick \_ -> ToOverview 
          , cls "btn btn-primary"
          ]
          [ HH.text "Overzicht" ]
        ]
      ]
    menuBar = HH.ul [cls "nav nav-tabs" ]
      (map menuBarItem state.competition.competitionApparatuses)

    menuBarItem apparatus = HH.li [ cls "nav-item"] 
      [ HH.a [cls $ "nav-link" <> if apparatus == state.curApparatus then " active" else ""
      , HE.onClick (\_ -> ChangeApparatus apparatus)
      ] [HH.text (apparatusName apparatus)]]
    
    cls :: forall r i. String -> HP.IProp (class :: String | r) i
    cls name = HP.class_ (H.ClassName name)
    content = 
      let 
        indices :: Array Int
        indices = range 1 (length state.competition.competitionParticipants)
        participants :: Array (Tuple Int Participant)
        participants = zip indices (reorder state.curApparatus state.competition.competitionApparatuses state.competition.competitionParticipants)
      in HH.table [cls "table"] 
        [ HH.thead [] 
          [ HH.tr []
            [ HH.th [] []
            , HH.th [] [HH.text "#"]
            , HH.th [] [HH.text "Naam"]
            , HH.th [] [HH.text "Club"]
            , HH.th [] [HH.text "Baan"]
            , HH.th [] [HH.text "Aanwezig"]
            ] 
          ]
        , HH.tbody [] (map trParticipant participants)

        ]

    trParticipant (Tuple index participant) = 
      HH.tr (if not participant.participantPresent then [cls "absent"] else [])
      [ HH.td [] [HH.text (show index)]
      , HH.td [] [HH.text participant.participantNumber]
      , HH.td [] [HH.text participant.participantName]
      , HH.td [] [HH.text participant.participantClub]
      , HH.td [] [HH.text $ case participant.participantLane of 
          Lane1 -> "baan 1"
          Lane2 -> "baan 2"
      ]
      , HH.td [] [
          HH.button
          [ HE.onClick \_ -> ChangeInput (\c -> pure $ c { competitionParticipants = updateParticipant (participant { participantPresent = not participant.participantPresent}) (c.competitionParticipants)}) 
          , cls "btn btn-warning btn-sm"
          ]
          [ HH.span [cls $ if participant.participantPresent then "fa fa-xmark" else "fa fa-check"] []]
        ]
      ]


handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () PageAction m Unit
handleAction = case _ of
  ToOverview -> do
    H.raise (NavigatePage OverviewPage)
  ChangeApparatus apparatus -> do
    H.modify_ (\s -> s { curApparatus = apparatus})
  ChangeInput competitionF -> do
    competition <- H.get
    newCompetition <- H.liftEffect (modifyCompetition competition.competition.competitionId competitionF)
    H.put { competition: newCompetition, curApparatus: competition.curApparatus } 