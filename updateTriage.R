# ACTIVITY: Update triage pool
# 1. Download the Articles sheet and Coding Form (Responses) sheet. Extract data.
# 2. Get article ids present in Coding sheet.
# 3. For these ids, update 'codingStatus' column in Articles sheet to 'complete'.
# 4. Find ids that were classified 'available' and 'usuable' in Coding sheet.
# 5. For these available and reusable ids, identify which ones have not already been passed to triage
# 6. For forTriageNew ids , update 'passToTriage' column in Articles sheet to TRUE.
updateTriage <- function(){
  
  # load packages and custom function
  library(googlesheets)
  library(tidyverse)
  library(utils)
  pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}
  
  # 1. Download the Articles sheet and Coding Form (Responses) sheet. Extract data.
  articlesSheet <- gs_title("Articles")
  articlesData <- articlesSheet %>% gs_read()
  
  codingSheet <- gs_title("Coding Form (Responses)")
  codingData <- codingSheet %>% gs_read()
  
  # 2. Get article ids present in Coding sheet.
  coded <- codingData %>%
    pull(`Article ID:`)
  
  # throw an error if no coded articles
  if(!(length(coded) > 0)) stop('No articles currently coded!')
  
  # 3. For these ids, update 'codingStatus' column in Articles sheet to 'complete'.
  
  # first make changes
  articles_mod <- articlesData %>%
    mutate(codingStatus = ifelse(id %in% coded, 'complete', codingStatus))
  
  # 4. Find ids that were classified 'available' and 'usuable' in Coding sheet.
  forTriage <- codingData %>%
    filter(`Were you able to successfully download and open the data file?` == "Yes") %>%
    filter(`Does all of the data needed for evaluation and reproduction of the research appear to be available after brief review?` == "Yes, all of the data appear to be available") %>%
    filter(`Are the data understandable after brief review?` == "Yes") %>%
    pull(`Article ID:`)
  
  # 5. For these available and reusable ids, identify which ones have not already been passed to triage
  forTriageNew <- articles_mod %>%
    filter(id %in% forTriage & passToTriage != "TRUE") %>%
    pull(id)

  # 6. For forTriageNew ids , update 'passToTriage' column in Articles sheet to TRUE.
  
  # first make changes
  articles_mod <- articles_mod %>%
    mutate(passToTriage = ifelse(id %in% forTriageNew, "TRUE", passToTriage)) # if new id ready for triage mark TRUE

  # now write new csv file
  write.csv(articles_mod, 'articles_mod.csv', row.names = F)
  
  # now upload csv and overwrite old sheet
  gs_upload("articles_mod.csv", sheet_title = 'Articles', overwrite = TRUE)
  
  # Notify user of changes
  print(paste(length(coded), " coded articles detected"))
  print(paste("Triage updated with ", length(forTriageNew), " new articles"))
}