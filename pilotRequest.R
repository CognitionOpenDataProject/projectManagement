# EVENT: Pilot requests article
# 1. Download Articles sheet and extract the data
# 2. Filter so we only have article ids where the reproducibilityStatus = 'inPool'. This is the current Reproducibility Pool.
# 3. Randomly select the article from the pool.
# 4. Update reproducibilityStatus column for this id to 'withPilot'
# 5. Update pilot column to pilot's initials
# 6. Send the id to the pilot.
pilotRequest <- function(thisPilot){
  
  # show warning if pilot name exceeds 3 characters
  if(length(thisPilot) > 3) warning('pilot initials exceed 3 characters!')
  
  # load packages and custom function
  library(googlesheets)
  library(tidyverse)
  pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}
  
  # 1. Download the Articles sheet and extract the data
  articlesSheet <- gs_title("Articles")
  articlesData <- articlesSheet %>% gs_read()
  
  # 2. Filter so we only have article ids where the reproducibilityStatus = 'inPool'. This is the current Reproducibility Pool.
  reproducibilityPool <- articlesData %>%
    filter(reproducibilityStatus == 'inPool') %>%
    pull(id)
  
  # throw an error if pool is empty
  if(length(reproducibilityPool) < 1) stop('Reproducibility pool is empty!')
  
  # 3. Randomly select an article from the pool.
  thisID <- sample(reproducibilityPool, 1)
  
  # 4. Update reproducibilityStatus column for this id to 'withPilot'
  # 5. Update pilot column to pilot's initials
  
  # first make changes
  articles_mod <- articlesData %>%
    mutate(reproducibilityStatus = ifelse(id == thisID, 'withPilot', reproducibilityStatus),
           pilot = ifelse(id == thisID, thisPilot, pilot))
  
  # now write new csv file
  write.csv(articles_mod, 'articles_mod.csv', row.names = F)
  
  # now upload csv and overwrite old sheet
  gs_upload("articles_mod.csv", sheet_title = 'Articles', overwrite = TRUE)
  
  # 6. Print article id to send to pilot
  print(paste0('Now e-mail the this article ID ', thisID, ' to the pilot.'))
}