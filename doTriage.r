# ACTIVITY: Reproducibility triage
# 1. Download the Articles sheet.
# 2. Filter by ids that have TRUE in 'passToTriage' column and '?' in 'triageStatus' column
# 3. Randomly select an id and print out.
# 4. User manually downloads article and data.
# 5. User manually identifies target outcomes and writes targetOutcomes.md
# 6. User manually creates new Github repo with article, target outcomes, and data
# 7. User responds to prompt when these steps are complete
# 8. If 'y' then update 'triageStatus' to 'pendingVerification' for this id. 
# 8. If 'n' then update 'triageStatus' to 'rejected' for this id.  
# 9. Reminder user to request verification from another project lead.
doTriage <- function(){

  # load packages and custom function
  library(googlesheets)
  library(tidyverse)
  library(utils)
  pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}
  
  # 1. Download the Articles sheet and extract the data
  articlesSheet <- gs_title("Articles")
  articlesData <- articlesSheet %>% gs_read()
  
  # 2. Filter by ids that have TRUE in 'passToTriage' column and '?' in 'triageStatus' column
  triagePool <- articlesData %>%
    filter(passToTriage == TRUE & triageStatus == '?') %>%
    pull(id)
  
  # throw an error if triage pool is empty
  if(!(length(triagePool) > 0)) stop('Triage pool is currently empty!')
  
  # 3. Randomly select an id and print out. Also print out doi.
  thisID <- sample(triagePool, 1)
  doi <- articlesData %>% 
    filter(id == thisID) %>%
    pull(doi)
  
  print(paste("Please set up a repo for this article ID: ", thisID))
  print(paste("The article doi is: ", doi))
  print("The repo should contain the article pdf, targetOutcomes.md, and data file(s)")
  browseURL(paste("https://doi.org/", doi))
  
  # 4. User manually downloads article and data.
  # 5. User manually identifies target outcomes and writes targetOutcomes.md
  # 6. User manually creates new Github repo with article, target outcomes, and data
  # 7. User responds to prompt when these steps are complete
  
  resp <- readline(prompt="If repo setup enter 'y'. To reject enter 'n'. To abort enter any other key.")
  
  if(!(resp == 'y' | resp == 'n')) stop('User aborted triage!')
  
  # 8. If 'y' then update 'triageStatus' to 'pendingVerification' for this id. 
  # 8. If 'n' then update 'triageStatus' to 'rejected' for this id. 
  
  # first make changes
  if(resp == 'y'){
    articles_mod <- articlesData %>%
      mutate(triageStatus = ifelse(id == thisID, 'pendingVerification', triageStatus))
  }else if(resp == 'n'){
    articles_mod <- articlesData %>%
      mutate(triageStatus = ifelse(id == thisID, 'rejected', triageStatus))
  }
  
  # now write new csv file
  write.csv(articles_mod, 'articles_mod.csv', row.names = F)
  
  # now upload csv and overwrite old sheet
  gs_upload("articles_mod.csv", sheet_title = 'Articles', overwrite = TRUE)
  
  # 9. Reminder user to request verification from another project lead.
  if(resp == 'y'){
    print(paste("Triage outcome for article id ", thisID, " is now pending verification. Please send id to project lead."))
  }else if(resp == 'n'){
    print("This article has been rejected and will not pass to the reproducibility pool.")
  }
}