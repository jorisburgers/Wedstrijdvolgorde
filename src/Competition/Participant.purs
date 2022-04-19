module Competition.Participant
  where

import Prelude

import Control.Monad.Except (throwError)
import Data.Int (fromString)
import Data.List.Types (NonEmptyList (..))
import Data.Maybe (Maybe (..))
import Data.NonEmpty (singleton)
import Data.Array (filter)
import Effect (Effect)
import Foreign (ForeignError (..))
import Simple.JSON as JSON
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem, getItem)

newtype ParticipantId = ParticipantId Int

derive instance eqParticipantId :: Eq ParticipantId

instance showParticipantId :: Show ParticipantId where
  show (ParticipantId x) = show x

type Participant =
  { participantId :: ParticipantId
  , participantNumber :: String 
  , participantName :: String
  , participantClub :: String
  , participantPresent :: Boolean
  , participantLane :: Lane
  }

data Lane = Lane1 | Lane2

derive instance eqLane :: Eq Lane

instance showLane :: Show Lane where
  show Lane1 = "lane1"
  show Lane2 = "lane2"

instance writeForeignLane :: JSON.WriteForeign Lane where
  writeImpl a = JSON.writeImpl $ show a

instance readForeignLane :: JSON.ReadForeign Lane where
  readImpl x = JSON.readImpl x >>= \str -> 
    case str of
      "lane1" -> pure Lane1
      "lane2" -> pure Lane2
      s -> throwError (NonEmptyList $ singleton $ ForeignError ("Invalid key: " <> s))

instance writeForeignParticipantId :: JSON.WriteForeign ParticipantId where
  writeImpl (ParticipantId a) = JSON.writeImpl $ show a

instance readForeignParticipantId :: JSON.ReadForeign ParticipantId where
  readImpl x = JSON.readImpl x >>= \str -> 
    case fromString str of
      Just i -> pure (ParticipantId i)
      Nothing -> throwError (NonEmptyList $ singleton $ ForeignError ("Invalid int: " <> str))

newParticipant :: ParticipantId -> Lane -> Participant
newParticipant pid lane =
  { participantId: pid
  , participantNumber: ""
  , participantName: ""
  , participantClub: ""
  , participantPresent: true
  , participantLane: lane
  }

genId :: Effect Int
genId = do
  w <- window
  s <- localStorage w
  let idKey = "maxId"
  existing <- getItem idKey s
  new <- case existing >>= fromString of
    Nothing -> pure 0
    Just x -> pure (x + 1)
  setItem idKey (show new) s
  pure new

updateParticipant :: Participant -> Array Participant -> Array Participant
updateParticipant p = map (\p' -> if p.participantId ==  p'.participantId then p else p')

removeParticipant :: ParticipantId -> Array Participant -> Array Participant
removeParticipant pId = filter (\p -> p.participantId /= pId)