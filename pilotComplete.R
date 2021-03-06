# EVENT: Pilot completes report and makes pull request
# 1. Address any obvious questions or issues. Check pilotReport.Rmd and pilotReport.csv are present. If ok, merge the fork on Github and move to the copiloting stage (below).
# 2. Download Articles sheet and extract data
# 3. Update reproducibilityStatus column for this id to 'copiloting'
# 4. Update copilot column to copilot's initials.
# 5. Print article id to send to copilot
pilotComplete <- function(articleID, thisCopilot){
  
  # load packages and custom function
  library(googlesheets)
  library(tidyverse)
  pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}
  
  # 1. Address any obvious questions or issues. Check pilotReport.Rmd and pilotReport.csv are present. If ok, merge the fork on Github and move to the copiloting stage (below).
  resp <- readline(prompt="If repo checked and merged enter 'y'. To abort enter any other key.")
  
  if(!(resp == 'y')) stop('User aborted!')
  
  
  # 2. Download the Articles sheet and extract the data
  articlesSheet <- gs_title("Articles")
  articlesData <- articlesSheet %>% gs_read()
  
  # throw an error if article id is not present
  allID <- articlesData %>% pull(id)
  if(!(articleID %in% allID)) stop('Cannot find this article ID in spreadsheet!')
  
  # 3. Update reproducibilityStatus column for this id to 'copiloting'
  # 4. Update copilot column to copilot's initials.
  
  # first make changes
  articles_mod <- articlesData %>%
    mutate(reproducibilityStatus = ifelse(id == articleID, 'copiloting', reproducibilityStatus),
           copilot = ifelse(id == articleID, thisCopilot, copilot))
  
  # now write new csv file
  write.csv(articles_mod, 'articles_mod.csv', row.names = F)
  
  # now upload csv and overwrite old sheet
  gs_upload("articles_mod.csv", sheet_title = 'Articles', overwrite = TRUE)
  
  # 5. Print article id to send to copilot
  print(paste0('Now e-mail the this article ID ', articleID, ' to the copilot.'))
}