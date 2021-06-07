#clear code and load libraries
cat("\014")
rm(list=ls())
gc()
library(lubridate)
library(data.table)
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
              select=field_names_filled)
names <- colnames(data)
gc()

# Selecteer alle leningen die een keer in default zijn gegaan
# Doe een garbage collection achteraf. Vrij grote dataset
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

default_flag <- as.numeric(data$DLQ_STATUS >= 3)
data <- data[,DEFAULT_FLAG := default_flag]

data_list <- split(data, f = data$LOAN_ID)
data <- NULL
gc()

data_list <- lapply(data_list, function(x){
  
  df <- data.frame(CHANGE_DEFAULT = c(0, diff(x$DEFAULT_FLAG)))
  cbind(x, df)
})
gc()



