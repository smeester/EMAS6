#clear code and load libraries
cat("\014")
rm(list=ls())
gc()
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(tidyverse)
library(tidyr)
library(bit64)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(xgboost)
library(fastDummies)
library(AUC)
library(caret)
library(e1071)
library(randomForest)
library(DiagrammeR)
'%nin%' = Negate('%in%')
# memory.limit(size=50800)


source("variables.R")

# Lees data in en doe een garbage collector
data <- fread(file_location,
              select=field_names_lgd)
names <- colnames(data)
gc()
# Onderstaande is gebruikt om een keer uit te zoeken welke kolommen allemaal gevuld zijn

#empty_cols <- apply(data,2, function(x){
#  any(!is.na(x))
#  })
#filled_columns <- names(empty_cols)
#cat(paste(shQuote(filled_columns , type="cmd"), collapse=", "))

# Selecteer alle leningen die een keer in default zijn gegaan
# Doe een garbage collection achteraf. Vrij grote dataset
data_dlq <- unique(data$LOAN_ID[data$DLQ_STATUS >= 3])
data <- data[data$LOAN_ID %in% data_dlq, ]
gc()

data$ACT_PERIOD <- my(data$ACT_PERIOD)
data$ORIG_DATE <- my(data$ORIG_DATE)

# Converteer alle datums naar van format m(m)yyyy naar een datum. 
# functie my() converteert dit automatisch  

data_list <- split(data, f = data$LOAN_ID)
#data <- NULL
data_list <- lapply(data_list, function(x){
  x$ACT_PERIOD <- my(x$ACT_PERIOD)
  x$ORIG_DATE <- my(x$ORIG_DATE)
  x$FIRST_PAY <- my(x$FIRST_PAY)
  x$MATR_DT <- my(x$MATR_DT)
  return(x)
})



