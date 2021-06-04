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


#read data
file_location <- 'C:/Users/smeester/Documents/AGAI/EMAS10/Case 6/Data aanlevering/emas_dataset.csv'
data <- fread(file_location,
              select=c("POOL_ID","LOAN_ID","ACT_PERIOD","CHANNEL","SELLER","SERVICER","MASTER_SERVICER",
                        "ORIG_RATE","CURR_RATE","ORIG_UPB","ISSUANCE_UPB","CURRENT_UPB","ORIG_TERM","ORIG_DATE",
                        "LOAN_AGE","MATR_DT","OLTV","DTI","CSCORE_B","CSCORE_C","FIRST_FLAG","PURPOSE","OCC_STAT",
                        "STATE","MSA","ZIP",
                        "MI_PCT","PRODUCT","DLQ_STATUS","PMT_HISTORY","MOD_FLAG",
                        "FORECLOSURE_DATE","DISPOSITION_DATE",
                        "FORECLOSURE_COSTS","PROPERTY_PRESERVATION_AND_REPAIR_COSTS","ASSET_RECOVERY_COSTS",
                        "MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS","ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY",
                        "NET_SALES_PROCEEDS","CREDIT_ENHANCEMENT_PROCEEDS","REPURCHASES_MAKE_WHOLE_PROCEEDS",
                        "OTHER_FORECLOSURE_PROCEEDS","NON_INTEREST_BEARING_UPB","ISSUE_SCOREB","ISSUE_SCOREC",
                        "CURR_SCOREB","CURR_SCOREC","MI_TYPE","DELINQUENT_ACCRUED_INTEREST",
                        "PROPERTY_INSPECTION_WAIVER_INDICATOR","HIGH_BALANCE_LOAN_INDICATOR",
                        "ARM_5_YR_INDICATOR","ARM_PRODUCT_TYPE","ARM_INDEX","ARM_CAP_STRUCTURE",
                        "MARGIN","BALLOON_INDICATOR","PLAN_NUMBER","FORBEARANCE_INDICATOR",
                        "HIGH_LOAN_TO_VALUE_HLTV_REFINANCE_OPTION_INDICATOR","DEAL_NAME","RE_PROCS_FLAG"))

#verwijderde kolommen: "FIRST_PAY","REM_MONTHS","ADJ_REM_MONTHS","OCLTV","NUM_BO","FIRST_FLAG","PROP",
#"NO_UNITS","OCC_STAT","PPMT_FLG","IO","FIRST_PAY_IO","MNTHS_TO_AMTZ_IO","MOD_FLAG","MI_CANCEL_FLAG",
#"Zero_Bal_Code","ZB_DTE","LAST_UPB","RPRCH_DTE","CURR_SCHD_PRNCPL","TOT_SCHD_PRNCPL","UNSCHD_PRNCPL_CURR",
#"LAST_PAID_INSTALLMENT_DATE","PRINCIPAL_FORGIVENESS_AMOUNT","ORIGINAL_LIST_START_DATE","ORIGINAL_LIST_PRICE",
#"CURRENT_LIST_START_DATE","CURRENT_LIST_PRICE","SERV_IND","CURRENT_PERIOD_MODIFICATION_LOSS_AMOUNT",
#"CUMULATIVE_MODIFICATION_LOSS_AMOUNT","CURRENT_PERIOD_CREDIT_EVENT_NET_GAIN_OR_LOSS",
#"CUMULATIVE_CREDIT_EVENT_NET_GAIN_OR_LOSS","HOMEREADY_PROGRAM_INDICATOR",
#"FORECLOSURE_PRINCIPAL_WRITE_OFF_AMOUNT","RELOCATION_MORTGAGE_INDICATOR","ZERO_BALANCE_CODE_CHANGE_DATE",
#"LOAN_HOLDBACK_INDICATOR","LOAN_HOLDBACK_EFFECTIVE_DATE","MONTHS_UNTIL_FIRST_PAYMENT_RESET",
#"MONTHS_BETWEEN_SUBSEQUENT_PAYMENT_RESET","INTEREST_RATE_CHANGE_DATE","PAYMENT_CHANGE_DATE",
#"INITIAL_INTEREST_RATE_CAP","PERIODIC_INTEREST_RATE_CAP","LIFETIME_INTEREST_RATE_CAP","ADR_TYPE","ADR_COUNT",
#"ADR_UPB"

#convert date format from "m(m)yyyy" to "yyyy-mm"
data$ACT_PERIOD <- paste(with(data, data$ACT_PERIOD %% 10000),
                                ifelse(as.numeric(substr(data$ACT_PERIOD, 1, floor(log10(data$ACT_PERIOD)) - 3)) < 10,
                                paste(0,substr(data$ACT_PERIOD, 1, floor(log10(data$ACT_PERIOD)) - 3),sep=""),
                                substr(data$ACT_PERIOD, 1, floor(log10(data$ACT_PERIOD)) - 3)), sep="-")

data$ORIG_DATE <- paste(with(data, data$ORIG_DATE %% 10000),
                         ifelse(as.numeric(substr(data$ORIG_DATE, 1, floor(log10(data$ORIG_DATE)) - 3)) < 10,
                                paste(0,substr(data$ORIG_DATE, 1, floor(log10(data$ORIG_DATE)) - 3),sep=""),
                                substr(data$ORIG_DATE, 1, floor(log10(data$ORIG_DATE)) - 3)), sep="-")

#data$MATR_DATE <- paste(with(data, data$MATR_DATE %% 10000),
#                        ifelse(as.numeric(substr(data$MATR_DATE, 1, floor(log10(data$MATR_DATE)) - 3)) < 10,
#                               paste(0,substr(data$MATR_DATE, 1, floor(log10(data$MATR_DATE)) - 3),sep=""),
#                               substr(data$MATR_DATE, 1, floor(log10(data$MATR_DATE)) - 3)), sep="-")

#calculate total/average exposure per month
exposure_total <- aggregate(CURRENT_UPB ~ ACT_PERIOD, data=data, sum)
exposure_total$ACT_PERIOD <- paste(exposure_total$ACT_PERIOD, "01", sep="-")
exposure_total$ACT_PERIOD <- as.Date(exposure_total$ACT_PERIOD, format =  "%Y-%m-%d")
exposure_total$CURRENT_UPB <- exposure_total$CURRENT_UPB / 1000000 #for clarity of graph
exposure_average <- aggregate(CURRENT_UPB ~ ACT_PERIOD, data=data, mean)
exposure_average$ACT_PERIOD <- paste(exposure_average$ACT_PERIOD, "01", sep="-")
exposure_average$ACT_PERIOD <- as.Date(exposure_average$ACT_PERIOD, format =  "%Y-%m-%d")
exposure_average$CURRENT_UPB <- exposure_average$CURRENT_UPB / 1000 #for clarity of graph

#make graph of total exposure
ggplot(data=exposure_total, aes(x=ACT_PERIOD, y=CURRENT_UPB)) +
        geom_line() +
        ggtitle("Total Current Unpaid Principal Balance") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Date") +
        ylab("Amount (in millions of $)")

#make graph of average exposure
ggplot(data=exposure_average, aes(x=ACT_PERIOD, y=CURRENT_UPB)) +
        geom_line() +
        ggtitle("Average Current Unpaid Principal Balance") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Date") +
        ylab("Amount (in thousands of $)")

#create subset
dataset_sub <- data[1:5000,]

#total loans
total_loans_count <- aggregate(LOAN_ID ~ ACT_PERIOD, data=data, length) #total loans per date
total_loans_count$ACT_PERIOD <- paste(total_loans_count$ACT_PERIOD, "01", sep="-")
total_loans_count$ACT_PERIOD <- as.Date(total_loans_count$ACT_PERIOD, format =  "%Y-%m-%d")
ggplot(data=total_loans_count, aes(x=ACT_PERIOD, y=LOAN_ID)) +
        geom_line() +
        ggtitle("Total Number of Loans") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Date") +
        ylab("Loans")
#check_date <- data[data$ACT_PERIOD == "2010-07"]

#distinct loans
unique_loans <- data %>% distinct(LOAN_ID, .keep_all = TRUE) #total number of unique loans
unique_loans_count <- aggregate(LOAN_ID ~ ACT_PERIOD, data=unique_loans, length) #total loans per date
unique_loans_count$ACT_PERIOD <- paste(unique_loans_count$ACT_PERIOD, "01", sep="-")
unique_loans_count$ACT_PERIOD <- as.Date(unique_loans_count$ACT_PERIOD, format =  "%Y-%m-%d")
ggplot(data=unique_loans_count, aes(x=ACT_PERIOD, y=LOAN_ID)) +
        geom_line() +
        ggtitle("Total Distinct Number of Loans") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Date") +
        ylab("Loans")

#distinct act period
unique_act <- data %>% distinct(LOAN_ID, .keep_all = TRUE)
unique_act_count <- aggregate(LOAN_ID ~ ACT_PERIOD, data=unique_act, length) #unique loans per date

#check unique loans by origin date (and check one duplicate)
unique_loans_group[,2] <- sapply(unique_loans_group[,2], as.numeric)
unique_loans_group <- unique_loans_group %>% distinct(LOAN_ID, .keep_all = TRUE) #total number of unique loans
dupl <- duplicated(unique_loans_group$LOAN_ID)
dupl_loans <- unique_loans_group$LOAN_ID[dupl]
check_loan <- data[data$LOAN_ID==104131121564]

#check if original interest rate is equal to current interest rate
data_rates <- data %>% select(CURR_RATE, ORIG_RATE)
data_rates <- data_rates %>% drop_na(ORIG_RATE,CURR_RATE)
identical(data_rates[['CURR_RATE']],data_rates[['ORIG_RATE']])
data_rates$diff <- data_rates$ORIG_RATE - data_rates$CURR_RATE
data_rates_diff <- data_rates[diff != 0]

#replace values for channel
unique_loans$CHANNEL[unique_loans$CHANNEL == "B"] <- "Broker"
unique_loans$CHANNEL[unique_loans$CHANNEL == "C"] <- "Correspondent"
unique_loans$CHANNEL[unique_loans$CHANNEL == "R"] <- "Retail"

#plot histogram of original loan values
hist(unique_loans$ORIG_UPB,
     breaks=40,
     main="Histogram of original loan values",
     xlab="Loan value",
     ylab="Frequency")

#plot histogram of original loan percentages
hist(unique_loans$ORIG_RATE,
     breaks=20,
     main="Histogram of original loan percentages",
     xlab="Loan Percentage",
     ylab="Frequency")

#plot time series of original loan percentages
orig_rate_average <- aggregate(ORIG_RATE ~ ACT_PERIOD, data=data, mean)
orig_rate_average$ACT_PERIOD <- paste(orig_rate_average$ACT_PERIOD, "01", sep="-")
orig_rate_average$ACT_PERIOD <- as.Date(orig_rate_average$ACT_PERIOD, format =  "%Y-%m-%d")
ggplot(data=orig_rate_average, aes(x=ACT_PERIOD, y=ORIG_RATE)) +
        geom_line() +
        ggtitle("Development of average original interest rates") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Date") +
        ylab("Interest rate")

#barplot for channel
barplot(table(unique_loans$CHANNEL),
        main="Frequency of loans per channel",
        xlab="Channel",
        ylab="Frequency")

#filter interest only mortgages
#interest_only <- data %>% filter(IO == "N")

#filter fixed rate mortgages
#amortization <- data %>% filter(PRODUCT == "FRM")

#select data for calculations
data_dlq <- data %>% select(LOAN_ID, ORIG_RATE, CURRENT_UPB, CURR_RATE, ORIG_UPB, ORIG_TERM,
                            DLQ_STATUS, CHANNEL, OLTV, DTI, PURPOSE, CSCORE_B, CSCORE_C, LOAN_AGE, FIRST_FLAG,
                            OCC_STAT,MOD_FLAG)

data_dlq$ANNUITY <- with(data_dlq, (1-(1/(1+ORIG_RATE/1200)^ORIG_TERM))/(ORIG_RATE/1200))
data_dlq$MONTHLY_PAYMENT <- with(data_dlq, ORIG_UPB/ANNUITY)

#correct calculation using current UPB
#data_dlq$ANNUITY_2 <- with(data_dlq, (1-(1/(1+CURR_RATE/1200)^(ORIG_TERM-LOAN_AGE-1)))/(CURR_RATE/1200))
#data_dlq$MONTHLY_PAYMENT_2 <- with(data_dlq, CURRENT_UPB/ANNUITY_2)


#filter XX
data_dlq <- data_dlq %>% filter(DLQ_STATUS != "XX")

#default calculation
data_dlq$DLQ_STATUS <- as.numeric(data_dlq$DLQ_STATUS)
data_dlq$DLQ_TOTAL <- data_dlq$MONTHLY_PAYMENT * data_dlq$DLQ_STATUS 
data_dlq$DEFAULT <- ifelse(data_dlq$DLQ_TOTAL > 0.01 * data_dlq$CURRENT_UPB & data_dlq$DLQ_STATUS > 2, 1, 0)

defaults <- data_dlq[DEFAULT == 1]

#make dataset for machine learning
features <- names(data_dlq) %>% .[. %nin% c("LOAN_ID", "ORIG_RATE", "CURRENT_UPB", "CURR_RATE",
                                            "ORIG_UPB", "ORIG_TERM", "DLQ_STATUS", "OLTV", "DTI",
                                            "ANNUITY", "MONTHLY_PAYMENT", "DLQ_TOTAL", "DEFAULT",
                                            "CSCORE_B", "CSCORE_C", "LOAN_AGE")]

#create dummy variables 
data_dummy <- dummy_cols(data_dlq, select_columns = features,
                         remove_selected_columns = TRUE)

data_dummy <- na.omit(data_dummy)

target <- as.factor(data_dummy$DEFAULT)

#remove columns
data_dummy <- data_dummy[,-c(1,6,7,13,15,16)]

#perform random split on transformed data set
sample      <- sample(rep(1:2, diff(floor(nrow(data_dummy) * c(0, 0.5, 1)))))
data_train   <- data_dummy[sample==1, ]
data_val     <- data_dummy[sample==2, ]
target_train <- target[sample==1]
target_val   <- target[sample==2]

#fit classification  tree
class_tree = rpart(data.frame(cbind(target_train,data_train)), method = "class", control = rpart.control(cp = 0, maxdepth = 5), model=TRUE)
rpart.plot(class_tree)
summary(class_tree)
pred_val <- predict(class_tree, data.frame(data_val))
aucNum = AUC::auc(roc(pred_val[,2], target_val))
plot(roc(pred_val[,2], target_val), main = paste("Model gives AUC = ", as.character(aucNum)))

#boosting algorithm
bst <- xgboost(data = as.matrix(data_train),
               label = as.numeric(target_train) - 1,
               max_depth = 5,
               eta = 0.15,
               nrounds = 50,
               objective = "binary:logistic")

pred_val <- predict(bst, as.matrix(data_val))
aucNum  <- AUC::auc(roc(pred_val, target_val))
plot(roc(pred_val, target_val), main = paste("AUC = ", as.character(round(aucNum,3))))

xgb.plot.importance(importance_matrix = xgb.importance(model = bst), top_n=15)

#lgd calculations
data_lgd <- data_dlq %>% select(LOAN_ID, ORIG_RATE, CURRENT_UPB, CURR_RATE, ORIG_UPB, ORIG_TERM,
                            DLQ_STATUS, LOAN_AGE, MONTHLY_PAYMENT, DEFAULT)

data_lgd <- data_lgd %>% filter(DEFAULT == 1)

data_lgd$LOSS <- with(data_lgd, MONTHLY_PAYMENT * (1-1/(1+0.06/12)^(ORIG_TERM-LOAN_AGE))/(0.06/12))
data_lgd <- data_lgd %>% distinct(LOAN_ID, .keep_all = TRUE)
data_lgd$LGD <- with(data_lgd, LOSS / CURRENT_UPB)

#make dummy variable for determining when to count
#data_dlq$DLQ_LARGER_THAN_1PERCENT <- if_else(data_dlq$DLQ_TOTAL > 0.01 * data_dlq$CURRENT_UPB, 1, 0)
#
#data_dlq <- data_dlq %>% filter(DLQ_LARGER_THAN_1PERCENT == 1)
#
#unique_dlq <- data_dlq %>% distinct(LOAN_ID, .keep_all = FALSE)
#
#data_dlq$DLQ_MIN <- 0
#
#for (i in 1:nrow(unique_dlq)) {
#        idx <- which(unlist(unique_dlq[i,1]) == unlist(data_dlq[,1]))
#        min_dlq <- min(data_dlq[idx,7])
#        data_dlq[idx,12] <- min_dlq
#}
#
#data_dlq$DLQ_FROM <- with(data_dlq, ORIG_UPB/ANNUITY)
