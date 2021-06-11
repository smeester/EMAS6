#clear code and load libraries
cat("\014")
rm(list=ls())
gc()
library(lubridate)
library(data.table)
library(parallel)
#library(dplyr)
#library(readr)
#library(tidyverse)
#library(tidyr)
#library(bit64)
#library(ggplot2)
#library(rpart)
#library(rpart.plot)
#library(xgboost)
#library(fastDummies)
#library(AUC)
#library(caret)
#library(e1071)
#library(randomForest)
#library(DiagrammeR)
'%nin%' = Negate('%in%')
# memory.limit(size=50800)


source("variables.R")

# Lees data in en doe een garbage collector
data <- fread(file_location,
              select=field_names_lgd)
names <- colnames(data)
gc()

#unique_dlq <- unique(data$DLQ_STATUS)
# Selecteer alle leningen die een keer in default zijn gegaan
# Doe een garbage collection achteraf. Vrij grote dataset
data$DLQ_STATUS <- as.numeric(data$DLQ_STATUS)
data_dlq <- unique(data$LOAN_ID[data$DLQ_STATUS >= 3])
data <- data[data$LOAN_ID %in% data_dlq, ]
gc()

# Converteer alle datums naar van format m(m)yyyy naar een datum. 
# functie my() converteert dit automatisch  
data$ACT_PERIOD <- my(data$ACT_PERIOD)
data$ORIG_DATE <- my(data$ORIG_DATE)
data$FIRST_PAY <- my(data$FIRST_PAY)
data$MATR_DT <- my(data$MATR_DT)
data$ZB_DTE <- my(data$ZB_DTE)
data$LAST_PAID_INSTALLMENT_DATE <- my(data$LAST_PAID_INSTALLMENT_DATE)
data$FORECLOSURE_DATE <- my(data$FORECLOSURE_DATE)
data$DISPOSITION_DATE <- my(data$DISPOSITION_DATE)
gc()

# Iedereen met DQL_STATUS >= 3 sowieso in default
# Default flag 0 of 1 
default_flag <- as.numeric(data$DLQ_STATUS >= 3)
default_flag[is.na(default_flag)] <- 0

data <- data[,DEFAULT_FLAG := default_flag]

data_list <- split(data, f = data$LOAN_ID)
data <- NULL
gc()

# De deelnemers die onder de drie gaan maar die in default zijn die moeten in 
# Default blijven totdat ze weer op 0 staan.
data_list_try <- mclapply(data_list, function(x){
  aantal_rijen <- nrow(x)
  if(aantal_rijen <2){
      return(x)
  } else {
    for(i in 2:aantal_rijen){
      dlq <- x[i,]$DLQ_STATUS
      in_default <- x[i-1,]$DEFAULT_FLAG
      if(!is.na(dlq) && (dlq == 1 || dlq == 2) && in_default){
        x[i,]$DEFAULT_FLAG <- 1
      }
    }
    return(x)
  }
  return(x)
})
gc()

#Voeg een aantal kolommen toe waarin staat wie in en uit default gaat. Dit kan je gebruiken
# om te kijken hoe vaak iedereen in en uit default is gegaan
# Todo blijkbaar is de DQL status 0 wanneer de hypotheekbalans weer naar 0 gaat 
# De reden hiervan moeten we nog even meenemen. 
data_list_try <- mclapply(data_list_try, function(x){
  
  df <- data.frame(CHANGE_DEFAULT = c(0, diff(x$DEFAULT_FLAG)))
  df$IN_DEFAULT <- as.numeric(df$CHANGE_DEFAULT == 1)
  df$OUT_DEFAULT <- as.numeric(df$CHANGE_DEFAULT == -1)
  df$TIMES_IN_DEFAULT <- cumsum(df$IN_DEFAULT)
  cbind(x, df)
})
gc()

mtimes_in_default <- sapply(data_list_try, function(x){
  any(x$TIMES_IN_DEFAULT >= 2)
})

list_mtimes_default <- data_list_try[mtimes_in_default]

recover_from_default <- unlist(mclapply(data_list_try, function(x){
  any(x$CHANGE_DEFAULT == -1)
}))
sum(recover_from_default)
list_not_in_default <- data_list[recover_from_default]

in_foreclosure <- sapply(data_list_try, function(x){
  any(!is.na(x$FORECLOSURE_DATE))
})

list_in_foreclosure <- data_list_try[in_foreclosure]

