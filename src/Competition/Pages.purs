module Competition.Pages where

import Competition.Competition (Competition)

data Page 
  = Overview
  | List Competition
  | Edit Competition

data PageAction
  = NavigatePage Page
  | CreatedNewCompetition Competition