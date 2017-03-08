# EVENT: Project lead verifies or rejects reproducibility repo
# 1. Download Articles sheet
# 2. Update 'triageStatus' for this article id to 'verified' or 'rejected'. If verified, update 'reproducibilityStatus' to 'inPool'.
verifyTriage <- function(thisID, decision){
  
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
  
  # 2. Update 'triageStatus' for this article id to 'verified' or 'rejected'. If verified, update 'reproducibilityStatus' to 'inPool'.
  
  # first make changes
  if(decision == 'verified'){
    articles_mod <- articlesData %>%
      mutate(triageStatus = ifelse(id == thisID, 'verified', triageStatus),
             reproducibilityStatus = ifelse(id == thisID, 'inPool', reproducibilityStatus))
  }else if(decision == 'rejected'){
    articles_mod <- articlesData %>%
      mutate(triageStatus = ifelse(id == thisID, 'rejected', triageStatus))
  }else{
    stop("Decision was entered incorrectly! Must be either 'verified' or 'rejected'.")
  }

  # now write new csv file
  write.csv(articles_mod, 'articles_mod.csv', row.names = F)
  
  # now upload csv and overwrite old sheet
  gs_upload("articles_mod.csv", sheet_title = 'Articles', overwrite = TRUE)
  
  # 3. Inform user of action
  if(decision == 'verified'){
    print('Article moved to reproducibility pool.')
  }else if(decision == 'rejected'){
    print('This article has been rejected and will not pass to the reproducibility pool')
  }
}