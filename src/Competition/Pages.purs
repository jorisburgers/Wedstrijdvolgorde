module Competition.Pages where

import Competition.Competition (Competition)

data Page 
  = OverviewPage
  | ShowPage Competition
  | EditPage Competition
  | ImportExportPage

data PageAction
  = NavigatePage Page
  | CreatedNewCompetition Competition