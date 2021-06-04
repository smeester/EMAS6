#clear code and load libraries
cat("\014")
rm(list=ls())
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

short_data <- fread(file_location, nrows = 100)


data <- fread(file_location,
              select=field_names_lgd)
