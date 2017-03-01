# ACTIVITY: Status update for all pools
# 1. Download the Articles sheet. Extract data.
# 2. Get article ids present in Coding sheet.
statusUpdate <- function(){
  
  # load packages and custom function
  library(googlesheets)
  library(tidyverse)
  library(utils)
  pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}
  
  # 1. Download the Articles sheet. Extract data.
  articlesSheet <- gs_title("Articles")
  articlesData <- articlesSheet %>% gs_read()
  
  print("CODING POOL:")
  print(table(articlesData$codingStatus))
  
  print("TRIAGE POOL:")
  print(table(articlesData$passToTriage))
  
  print("REPRODUCIBILITY POOL:")
  print(table(articlesData$triageStatus))
  
  print("REPRODUCIBILITY CHECK STATUS:")
  print(table(articlesData$reproducibilityStatus))
}