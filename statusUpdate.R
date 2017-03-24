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
  
  # 2. Download the coding form. Extract data.
  codingSheet <- gs_title("Coding Form (Responses)")
  codingDataRaw <- codingSheet %>% gs_read()
  
  # format columns
  codingData <- codingDataRaw %>%
    rename(date = `What date was the article received by the journal?`,
           availabilityStatement = `Does the article state whether or not the data are available?`,
           availabilitySuccess = `Were you able to successfully download and open the data file(s)?`,
           understandable = `Are the data understandable after brief review?`) %>%
    mutate(date = as.Date(date, format='%m/%d/%Y'),
           policy = ifelse(date >= "2015-01-03", "POST-", "PRE-"),
           policy = factor(policy, levels = c("PRE-", "POST-")),
           availabilityStatement = recode_factor(availabilityStatement, "Yes - the statement says that the data (or some of the data) are available" = "Yes",
                                                 "No - there is no data availability statement" = "No"),
           availabilitySuccess = factor(availabilitySuccess, levels = c("Yes", "Some files only", "No")),
           understandable = factor(understandable, levels = c("Yes", "Partly", "No"))) 
  
  # summary stats
  graph_availableStatement <- codingData %>%
    group_by(policy, availabilityStatement) %>%
    summarise(n = n()) %>%
    complete(availabilityStatement, fill = list(n = 0)) %>%
    ggplot() + geom_bar(aes(x = policy, y = n, fill = availabilityStatement), stat = "identity", position=position_dodge()) +
    ylim(0,200) +
    theme_minimal() +
    scale_fill_manual(values=c("#009E73", "#E69F00", "#CC6666"))
    
  availableStatementN <- table(codingData$availabilityStatement)[['Yes']]
  
  graph_availableSuccess <- codingData %>%
    filter(availabilitySuccess != "NA") %>%
    group_by(policy, availabilitySuccess) %>%
    summarise(n = n()) %>%
    complete(availabilitySuccess, fill = list(n = 0)) %>%
    mutate(prop = n / sum(n)) %>%
    ggplot() + geom_bar(aes(x = policy, y = prop, fill = availabilitySuccess), stat = "identity", position=position_dodge()) +
    ylim(0,1) +
    theme_minimal() +
    ylab("proportion") +
    ggtitle(paste0("For articles (", availableStatementN  ,") with availability statements..")) +
    scale_fill_manual(values=c("#009E73", "#E69F00", "#CC6666"))
  
  availableSuccessN <- table(codingData$availabilitySuccess)[['Yes']]
  
  graph_understand <- codingData %>%
    filter(understandable != "NA") %>%
    group_by(policy, understandable) %>%
    summarise(n = n()) %>%
    mutate(prop = n / sum(n)) %>%
      ggplot() + geom_bar(aes(x = policy, y = prop, fill = understandable), stat = "identity", position=position_dodge()) +
      ylim(0,1) +
      theme_minimal() +
      ylab("proportion") +
      ggtitle(paste0("For articles (", availableSuccessN  ,") with availability success..")) +
      scale_fill_manual(values=c("#009E73", "#E69F00", "#CC6666"))
  
  
  print(graph_availableStatement)
  print(graph_availableSuccess)
  print(graph_understand)

  print("CODING POOL:")
  print(table(articlesData$codingStatus))
  
  print("TRIAGE POOL:")
  print(table(articlesData$passToTriage))
  
  print("REPRODUCIBILITY POOL:")
  print(table(articlesData$triageStatus))
  
  print("REPRODUCIBILITY CHECK STATUS:")
  print(table(articlesData$reproducibilityStatus))
}