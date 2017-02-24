# EVENT: Coder requests batch of articles
# 1. Download the Articles sheet.
# 2. Filter so we only have article ids where the codingStatus = 'inPool'. This is the current Coding Pool.
# 3. Randomly select the batch. Send ids to Coder.
# 4. Update codingStatus column for these ids to 'outForCoding'.
# 5. Update 'coder' column with coder's initials.
coderRequest <- function(batchSize, thisCoder){

  # show warning if batch size greater than 20
  if(batchSize > 20) warning('batch size exceeds 20!')
  
  # show warning if coder name exceeds 3 characters
  if(length(thisCoder) > 3) warning('coder initials exceed 3 characters!')
  
  # load packages and custom function
  library(googlesheets)
  library(tidyverse)
  pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}

  # 1. Download the Articles sheet and extract the data
  articlesSheet <- gs_title("Articles")
  articlesData <- articlesSheet %>% gs_read()
  
  # 2. Filter so we only have article ids where the codingStatus = 'inPool'. This is the current Coding Pool.
  codingPool <- articlesData %>%
    filter(codingStatus == 'inPool') %>%
    pull(id)
  
  # throw an error if batch size exceeds number of available articles
  if(batchSize > length(codingPool)) stop('Batch size exceeds number of available articles!')
  
  # 3. Randomly select the batch.
  thisBatch <- sample(codingPool, batchSize)
  
  # 4. Update codingStatus column for these ids to 'withCoder'.
  # 5. and update 'coder' column with coder's initials.
  
  # first make changes
  articles_mod <- articlesData %>%
    mutate(codingStatus = ifelse(id %in% thisBatch, 'withCoder', codingStatus),
           coder = ifelse(id %in% thisBatch, thisCoder, coder))
  
  # now write new csv file
  write.csv(articles_mod, 'articles_mod.csv', row.names = F)
  
  # now upload csv and overwrite old sheet
  gs_upload("articles_mod.csv", sheet_title = 'Articles', overwrite = TRUE)
  
  # 6. Output .csv file containing batch of article ids to send to coder
  filename <- paste0("codingBatches/", thisCoder, "_", format(Sys.time(), "%d-%b_%H-%M-%S"), "_batch", ".csv")
  write.csv(data.frame(articleID = thisBatch), filename, row.names = F)
  print(paste0('Now e-mail the .csv file ', filename, ' to the coder.'))
}