module Competition.UI.Edit where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe (..))
import Data.Array ((:), head, filter, snoc)
import Effect.Class (class MonadEffect)
import Competition.Competition (Competition, CompetitionType (..), modifyCompetition, maleApparatuses, femaleApparatuses, rotateFirst, apparatusName)
import Competition.Participant (Lane (..), ParticipantId (..), Participant (..), newParticipant, genId, updateParticipant, removeParticipant)
import Competition.Pages (Page (..), PageAction (..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy (..))

type Slot action = forall q. H.Slot q action Unit

data Action 
  = ToOverview
  | ChangeInput (Competition -> Effect Competition)

type State = 
  { competition :: Competition
  }

slot :: Proxy "edit"
slot = Proxy

component :: forall q i m. MonadEffect m => Competition -> H.Component q i PageAction m
component competition = H.mkComponent 
  { initialState: initialState competition
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

initialState :: forall i. Competition -> i -> State
initialState competition _ = { competition } 

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div [cls "container"] (header : content)
  where
    rowDiv = HH.div [cls "row"]
    header = rowDiv
      [ HH.div [cls "col-6"] [HH.h1_ [HH.text "Aanpassen"]]
      , HH.div [cls "col-6"] [
        HH.button
          [ HE.onClick \_ -> ToOverview ]
          [ HH.text "Overzicht" ]
        ]
      ]
    cls :: forall r i. String -> HP.IProp (class :: String | r) i
    cls name = HP.class_ (H.ClassName name)
    content = 
      [ rowDiv nameEdit
      , rowDiv typeEdit
      , rowDiv [ HH.div [cls "col-md-12"] [HH.text "Starttoestel"]]
      , rowDiv apparatusEdit
      , rowDiv [ HH.div [cls "col-md-12"] [HH.text "Deelnemers"]]
      , rowDiv participantsEdit
      ]

    nameEdit = [ HH.div [cls "col-md-12"]
          [ HH.label_ [HH.text "Naam:"]
          , HH.input 
            [ HE.onValueInput $ \input -> (ChangeInput (\c -> pure $ c { competitionName = input}))
            , HP.value (state.competition.competitionName)
            ]
          ]
        ]
    typeEdit = 
      [ HH.div 
        [cls "col-md-12"]
        [ HH.div [cls "form-check"] 
          [ HH.input 
              [ HE.onValueInput $ \_ -> (ChangeInput (\c -> pure $ c { competitionType = Male, competitionApparatuses = maleApparatuses}))
              , HP.name "competitionType"
              , HP.type_ HP.InputRadio
              , HP.checked (state.competition.competitionType == Male)
              , HP.id "competitionTypeMale"
              , cls "form-check-input"
              ]
          , HH.label [cls "form-check-label", HP.for "competitionTypeMale"] [HH.text "Heren"]
          ] 
        , HH.div [cls "form-check"]
          [HH.input 
            [ HE.onValueInput $ \_ -> (ChangeInput (\c -> pure $ c { competitionType = Female, competitionApparatuses = femaleApparatuses}))
            , HP.name "competitionType"
            , HP.type_ HP.InputRadio
            , HP.checked (state.competition.competitionType == Female)
            , HP.id "competitionTypeFemale"
            , cls "form-check-input"
            ]
          , HH.label [cls "form-check-label", HP.for "competitionTypeFemale"] [HH.text "Dames"]
          ]
        ]
      ]
    apparatusEdit = 
      let apps = state.competition.competitionApparatuses
          appInput app = 
            HH.div [cls "form-check"]
              [HH.input 
                [ HE.onValueInput $ \_ -> (ChangeInput (\c -> pure $ c { competitionApparatuses = rotateFirst app apps}))
                , HP.name "apparatus"
                , HP.type_ HP.InputRadio
                , HP.checked (head apps == Just app)
                , HP.id ("apparatus" <> show app)
                , cls "form-check-input"
                ]
              , HH.label [cls "form-check-label", HP.for ("apparatus" <> show app)] [HH.text (apparatusName app)]
              ]

      in [ HH.div
        [cls "col-md-12"]
        (map appInput $ case state.competition.competitionType of
            Male -> maleApparatuses
            Female -> femaleApparatuses
        )
      ]

    participantsEdit = 
      [ HH.div [cls "col-md-6"] [HH.text "Baan 1", laneEdit Lane1]
      , HH.div [cls "col-md-6"] [HH.text "Baan 2", laneEdit Lane2]
      ]
    
    laneEdit lane = 
      let participants = filter (\p -> p.participantLane == lane) state.competition.competitionParticipants
      in HH.ol_ (snoc (map liParticipant participants) (newParticipantButton lane))
          
    liParticipant participant = HH.li [] [
      HH.div [] 
        [ HH.input 
          [ HP.value (participant.participantNumber)
          , HP.placeholder "nummer"
          , HE.onValueInput $ \v -> (ChangeInput (\c -> pure $ c { competitionParticipants = updateParticipant (participant { participantNumber = v}) (c.competitionParticipants)}))
          ]
        , HH.input 
          [ HP.value (participant.participantName)
          , HP.placeholder "naam"
          , HE.onValueInput $ \v -> (ChangeInput (\c -> pure $ c { competitionParticipants = updateParticipant (participant { participantName = v}) (c.competitionParticipants)}))
          ]
        , HH.input 
          [ HP.value (participant.participantClub)
          , HP.placeholder "club"
          , HE.onValueInput $ \v -> (ChangeInput (\c -> pure $ c { competitionParticipants = updateParticipant (participant { participantClub = v}) (c.competitionParticipants)}))
          ]
        , HH.button 
          [HE.onClick \_ -> ChangeInput (\c -> pure $ c { competitionParticipants = removeParticipant participant.participantId c.competitionParticipants})] 
          [HH.text "Verwijderen"]
        ]
    ]

    newParticipantButton lane = HH.li [] [        
      HH.button
        [ HE.onClick \_ -> ChangeInput (\c -> genId >>= \x -> pure $ c { competitionParticipants = snoc c.competitionParticipants (newParticipant (ParticipantId x) lane)}) ]
        [ HH.text "Nieuwe deelnemer" ]
      ]


handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () PageAction m Unit
handleAction = case _ of
  ToOverview -> do
    H.raise (NavigatePage OverviewPage)
  ChangeInput competitionF -> do
    competition <- H.get
    newCompetition <- H.liftEffect (modifyCompetition competition.competition.competitionId competitionF)
    H.put { competition: newCompetition }