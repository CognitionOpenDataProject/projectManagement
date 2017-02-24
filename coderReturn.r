# EVENT: Coder returns article(s)
# 1. Download the Articles sheet.
# 2. Update codingStatus column for this id to 'inPool'.
# 3. Update coder column to '?'
coderReturn <- function(articleID){
  
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

  # 2. Update codingStatus column for this id to 'inPool'.
  # 3. and update 'coder' column to '?'
  
  # first make changes
  articles_mod <- articlesData %>%
    mutate(codingStatus = ifelse(id == articleID, 'inPool', codingStatus),
           coder = ifelse(id == articleID, '?', coder))
  
  # now write new csv file
  write.csv(articles_mod, 'articles_mod.csv', row.names = F)
  
  # now upload csv and overwrite old sheet
  gs_upload("articles_mod.csv", sheet_title = 'Articles', overwrite = TRUE)
  
  print("Article returned to Coding Pool.")
}