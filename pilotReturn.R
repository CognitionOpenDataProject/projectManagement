# EVENT: Pilot returns article
# 1. Download the Articles sheet.
# 2. Update reproducibilityStatus column for this id to 'inPool'.
# 3. Update pilot column to '?'
pilotReturn <- function(articleID){
  
  # load packages and custom function
  library(googlesheets)
  library(tidyverse)
  pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}
  
  # 1. Download the Articles sheet and extract the data
  articlesSheet <- gs_title("Articles")
  articlesData <- articlesSheet %>% gs_read()
  
  # throw an error if article id is not present
  allID <- articlesData %>% pull(id)
  if(!(articleID %in% allID)) stop('Cannot find this article ID in spreadsheet!')
  
  # 2. Update reproducibilityStatus column for this id to 'inPool'.
  # 3. and update 'pilot' column to '?'
  
  # first make changes
  articles_mod <- articlesData %>%
    mutate(reproducibilityStatus = ifelse(id == articleID, 'inPool', reproducibilityStatus),
           pilot = ifelse(id == articleID, '?', pilot))
  
  # now write new csv file
  write.csv(articles_mod, 'articles_mod.csv', row.names = F)
  
  # now upload csv and overwrite old sheet
  gs_upload("articles_mod.csv", sheet_title = 'Articles', overwrite = TRUE)
  
  print("Article returned to Reproducibility Pool.")
}