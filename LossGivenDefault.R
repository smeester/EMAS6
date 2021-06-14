#clear code and load libraries
cat("\014")
rm(list=ls())
gc()
library(lubridate)
library(data.table) 
library(parallel)
library(dplyr)
library(readr)
library(tidyverse)
library(tidyr)
#library(bit64)
library(ggplot2)
library(openxlsx)
library(EnvStats)
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
hpi <- read.xlsx("C:/Users/smeester/Projects/EMAS6/HPI_master_USA.xlsx")
gc()

#Verander de forbearence en Alternative Deliquency Resolution naar 7
#Verander paar variabelen zodat eea beetje handelbaarder wordt.
data$DLQ_STATUS <- as.numeric(data$DLQ_STATUS)
data$ADR_TYPE[data$ADR_TYPE=="7.0"] <- "7"
data$FORBEARANCE_INDICATOR[data$FORBEARANCE_INDICATOR == "7.0"] <- "7"
data$Zero_Bal_Code[is.na(data$Zero_Bal_Code)] <- 0

data$FORECLOSURE_COSTS[is.na(data$FORECLOSURE_COSTS)] <- 0
data$PROPERTY_PRESERVATION_AND_REPAIR_COSTS[is.na(data$PROPERTY_PRESERVATION_AND_REPAIR_COSTS)] <- 0
data$ASSET_RECOVERY_COSTS [is.na(data$ASSET_RECOVERY_COSTS)] <- 0
data$MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS [is.na(data$MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS)] <- 0
data$ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY [is.na(data$ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY)] <- 0
data$NET_SALES_PROCEEDS[is.na(data$NET_SALES_PROCEEDS)] <- 0
data$OTHER_FORECLOSURE_PROCEEDS[is.na(data$OTHER_FORECLOSURE_PROCEEDS)] <- 0
data$REPURCHASES_MAKE_WHOLE_PROCEEDS[is.na(data$REPURCHASES_MAKE_WHOLE_PROCEEDS)] <- 0
data$CREDIT_ENHANCEMENT_PROCEEDS[is.na(data$CREDIT_ENHANCEMENT_PROCEEDS)] <- 0

data$COSTS <- data %>% select(FORECLOSURE_COSTS,PROPERTY_PRESERVATION_AND_REPAIR_COSTS,
                              ASSET_RECOVERY_COSTS, MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS,
                              ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY) %>% 
                              transmute(x=rowSums(.))
data$PROCEEDS <- data %>% select(NET_SALES_PROCEEDS, OTHER_FORECLOSURE_PROCEEDS,
                                 REPURCHASES_MAKE_WHOLE_PROCEEDS,
                                 CREDIT_ENHANCEMENT_PROCEEDS) %>% 
                                transmute(x=rowSums(.))

data$VALUE <- with(data, ORIG_UPB/(OLTV/100))


#unique_dlq <- unique(data$DLQ_STATUS)
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

#Converteer alle datums van form yyyy-mm naar datum
hpi$PERIOD <- ym(hpi$PERIOD)

data$ANNUITY <- with(data, (1-(1/(1+ORIG_RATE/1200)^ORIG_TERM))/(ORIG_RATE/1200))
data$MONTHLY_PAYMENT <- with(data, ORIG_UPB/ANNUITY)
data$DLQ_TOTAL <- with(data, MONTHLY_PAYMENT * DLQ_STATUS )

data <- merge(data, hpi, by.x='ORIG_DATE', by.y='PERIOD')
data <- merge(data, hpi, by.x='ACT_PERIOD', by.y='PERIOD')

data$VALUE_INDEX <- with(data, INDEX.y/INDEX.x * VALUE) 

covid_loan_ids <- (data %>% filter(ADR_TYPE == "C") %>% 
                     select(LOAN_ID) %>% 
                     distinct(LOAN_ID))[[1]]

covid_Loans <- data %>% filter(LOAN_ID %in% covid_loan_ids)
non_covid_loans <- data %>% filter(LOAN_ID %nin% covid_loan_ids)

gc()

#loan_id_forbearence <- (non_covid_loans %>% 
#                        filter(FORBEARANCE_INDICATOR != "") %>% 
#                        select(LOAN_ID) %>%
#                        distinct(LOAN_ID))[[1]]
  
#loans_in_forbearance <- non_covid_loans %>% filter(LOAN_ID %in% loan_id_forbearence)
#forbrnce_ind <- unique(loans_in_forbearance$FORBEARANCE_INDICATOR)


# Iedereen met DQL_STATUS >= 3 sowieso in default
# Default flag 0 of 1 
default_flag <- as.numeric(non_covid_loans$DLQ_STATUS >= 3)
# Sommige waarden hebben 'xx' deze geven NA dus deze worden op 0 gezet. 
default_flag[is.na(default_flag)] <- 0

non_covid_loans <- non_covid_loans[,DEFAULT_FLAG := default_flag]

# We converteren naar een list van leningen zodat we op leningniveau een aantal dingen kunnen aanpassen
# Zonder rekening te hoeven te houden met andere leningen. 
l_non_covid_loans <- split(non_covid_loans, f = non_covid_loans$LOAN_ID)
#data <- NULL
gc()

# De deelnemers die onder de drie gaan maar die in default zijn die moeten in 
# Default blijven totdat ze weer op 0 staan.
l_non_covid_loans <- mclapply(l_non_covid_loans, function(x){
  # Bepaal van elk dataframe het aantal rijen
    aantal_rijen <- nrow(x)
  # minder dan twee rijen dan status laten zoals die is. 
    if(aantal_rijen <2){
      return(x)
  } else {
    for(i in 2:aantal_rijen){
      dlq <- x[i,]$DLQ_STATUS
      # Controleer of vorige regel in default was
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
l_non_covid_loans <- mclapply(l_non_covid_loans, function(x){
  
  df <- data.frame(CHANGE_DEFAULT = c(0, diff(x$DEFAULT_FLAG)))
  df$IN_DEFAULT <- as.numeric(df$CHANGE_DEFAULT == 1)
  df$OUT_DEFAULT <- as.numeric(df$CHANGE_DEFAULT == -1)
  df$TIMES_IN_DEFAULT <- cumsum(df$IN_DEFAULT)
  df$FORECLOSURE_INDICATOR <- as.numeric(!is.na(x$FORECLOSURE_DATE))
  df$COSTS_TOTAL <- with(x, cumsum(x$COSTS))
  cbind(x, df)
})
gc()

#Verander terug naar DataFRame
non_covid_loans <- bind_rows(l_non_covid_loans, .id = "LOAN_ID")

rfr <- 0.24688
discount <- (rfr + 5)/1200 #5 = discount rate, 1200 = monthly

non_covid_loans %>% group_by(OUT_DEFAULT + FORECLOSURE_INDICATOR) %>% summarise(n())

non_covid_loans$LOSS <- 0
df_results <- non_covid_loans %>% filter(OUT_DEFAULT ==1 | FORECLOSURE_INDICATOR == 1) %>%
  select(LAST_UPB, COSTS_TOTAL, NET_SALES_PROCEEDS, OTHER_FORECLOSURE_PROCEEDS, 
         FORECLOSURE_INDICATOR, Zero_Bal_Code, ADR_TYPE, VALUE_INDEX)
df_results$RESULT_COSTS <- with(df_results, (COSTS_TOTAL)/ LAST_UPB)
df_results$RESULT_SELL <- with(df_results, (VALUE_INDEX - NET_SALES_PROCEEDS - OTHER_FORECLOSURE_PROCEEDS)/ VALUE_INDEX)

df_results$RESULT_COSTS[is.na(a$RESULT_COSTS)] <- 0
df_results$RESULT_SELL[is.na(a$RESULT_SELL)] <- 0

df_results$RESULT_COSTS[a$FORECLOSURE_INDICATOR !=1] <- 0
df_results$RESULT_SELL[a$FORECLOSURE_INDICATOR !=1] <- 0

df_plot_costs <- df_results %>% filter(FORECLOSURE_INDICATOR == 1 & Zero_Bal_Code != 9)
ggplot(df_plot, aes(x=RESULT)) +
  geom_bar()
#a <- a %>% filter(FORECLOSURE_INDICATOR ==1 & RESULT >= 0 & RESULT < 1)
#a <- a %>% filter(FORECLOSURE_INDICATOR ==1)

#a$RESULT[a$RESULT < 0] <- 0
hist(df_plot_costs$RESULT_COSTS,
     breaks=50,
     main="Histogram of empirical Cost rate",
     xlab="Costs as fraction of UPB",
     ylab="Frequency",
     xlim=c(0,1))

hist(df_plot_costs$RESULT_SELL,
     breaks=50,
     main="Empirical Sell Result Cost rate",
     xlab="Price as fraction of UPB",
     ylab="Frequency",
     xlim=c(0,2))

scatter <- ggplot(data = df_plot_costs, aes(x = RESULT_COSTS, y = RESULT_SELL)) +
  geom_point()
scatter
rho <- with(df_plot_costs, cor(RESULT_SELL, RESULT_COSTS))

param_costs <- ebeta(df_plot_costs$RESULT_COSTS)
param_sell <- ebeta(df_plot_costs$RESULT_COSTS)
p_costs <- rbeta(10000, param_costs$parameters[[1]], param_costs$parameters[[2]])
p_sell <- rbeta(10000, param_sell$parameters[[1]], param_sell$parameters[[2]])
plot_costs <- density(p_costs)
plot_sell <- density(p_sell)
plot(plot_costs)
plot(plot_sell)


# PV (loss) == monthly payments*annuiteit = rest schuld
#fc_only <- non_covid_loans %>% filter(FORECLOSURE_INDICATOR == 1)
#fc_only$LOSS <- with(fc_only, MONTHLY_PAYMENT * ((1-1/(1+discount)^(ORIG_TERM-LOAN_AGE))/discount))

#non_covid_loans[non_covid_loans$FORECLOSURE_INDICATOR == 1, ] <- fc_only$LOSS

# value=(1-haircut HC)*value index;  value index=index now/index origin
#non_covid_loans$GAIN <- with(non_covid_loans, 0.8 * VALUE_INDEX)

#non_covid_loans$NET <- with(non_covid_loans, LOSS + COSTS - GAIN)
#View(non_covid_loans %>% filter(FORECLOSURE_INDICATOR==1))

loan_id_out_of_default <- (non_covid_loans %>% filter(OUT_DEFAULT == 1) %>% 
                             select(LOAN_ID) %>% distinct(LOAN_ID))[[1]]

# Maak Pie chart met aantal in foreclosure en niet in foreclosure
#df_out_of_default <- bind_rows(list_out_of_default, .id = "POLICY_NUMBER")

df_out_of_default <- non_covid_loans %>% filter(LOAN_ID %in% loan_id_out_of_default)

df_out_of_default[df_out_of_default$Zero_Bal_Code == 1 , ]$ADR_TYPE <- "P"
df_out_of_default[df_out_of_default$ADR_TYPE == "" , ]$ADR_TYPE <- "9"

# Maak dataframe voor aantal foreclosure vs niet foreclosed
df_fc_summary <- non_covid_loans %>% filter(OUT_DEFAULT == 1) %>% 
                 select(FORECLOSURE_INDICATOR) %>%
                 group_by(FORECLOSURE_INDICATOR) %>% summarise(n())
colnames(df_fc_summary) <- c("category", "number")
df_fc_summary$category[df_fc_summary$category == 0] <- "not foreclosed"
df_fc_summary$category[df_fc_summary$category == 1] <- "foreclosed"
df_fc_summary$number <- round((df_fc_summary$number / sum(df_fc_summary$number))*100, 1)

ggplot(df_fc_summary, aes(x="", y=number, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(number, "%")), position = position_stack(vjust=0.5))

# Maak een zelfde soort grafiek maar dan over tijd. 
# Maak een kolom waarin elke optie terugkomt
df_out_of_default$STATUS[df_out_of_default$DEFAULT_FLAG == 1] <-  "Default" 
df_out_of_default$STATUS[df_out_of_default$OUT_DEFAULT == 1] <- "Recovered" 
df_out_of_default$STATUS[df_out_of_default$FORECLOSURE_INDICATOR == 1] <- "Foreclosed"
df_out_of_default$STATUS[df_out_of_default$OUT_DEFAULT == 1 && df_out_of_default$Zero_Bal_Code != 1] <- "Sold"

sum(df_out_of_default$IN_DEFAULT)
sum(df_out_of_default$OUT_DEFAULT)

try <- df_out_of_default %>% filter(!is.na(STATUS)) %>% 
        group_by(ACT_PERIOD,  STATUS) %>% select(ACT_PERIOD, STATUS) %>% 
        summarise(n())
colnames(try) <- c("Date","Status","Number")
try$Status <- factor(try$Status, levels = c( "Recovered", "Foreclosed", "Default"))
try$Number[try$Status %in% c("Foreclosed", "Recovered")] <- -1 * try$Number[try$Status %in% c("Foreclosed", "Recovered")] 

ggplot(try, aes(x = Date, y = Number, fill = Status)) +
   geom_bar(stat="identity")


df_no_fc <- df_out_of_default[df_out_of_default$FORECLOSURE_INDICATOR ==0, ]

df_no_fc %>%
  group_by(ADR_TYPE) %>%
  summarise(number = n())

summary <- aggregate(ADR_TYPE ~ ADR_TYPE, data=df_no_fc, length)

table_fc <- aggregate(FORECLOSURE_INDICATOR, data = df_out_of_default, FUN = length)



mtimes_in_default <- sapply(data_list_try, function(x){
  any(x$TIMES_IN_DEFAULT >= 2)
})

list_mtimes_default <- data_list_try[mtimes_in_default]

recover_from_default <- unlist(mclapply(data_list_try, function(x){
  any(x$CHANGE_DEFAULT == -1)
}))
sum(recover_from_default)
has_once_recovered_from_default <- data_list[recover_from_default]

in_foreclosure <- sapply(data_list_try, function(x){
  any(!is.na(x$FORECLOSURE_DATE))
})

list_in_foreclosure <- data_list_try[in_foreclosure]

l_loans_out_of_default <- l_non_covid_loans[loan_id_out_default]

list_time_in_default <- lapply(l_loans_out_of_default, function(x){
  l <- rle(x$DEFAULT_FLAG)
  forb_ind <- x %>% select(FORBEARANCE_INDICATOR) %>% filter(OUT_DEFAULT == 1)
  no_default <- x %>% select(TIMES_IN_DEFAULT) %>% filter(OUT_DEFAULT == 1)
  tb <- tibble(values = l$values, length = l$lengths, forbearance = NA, no_default = NA )
  tb$forbearance[tb$values == 1, ] <- forb_ind
  tb$no_default[tb$values == 1, ] <- no_default
  return(tb)
})

time_in_default <- bind_rows(list_time_in_default, .id = "POLICY_NUMBER")
list_time_in_default <- lapply(list_time_in_default, function(x){
  
})

time_in_default <- unlist(list_time_in_default)
hist(time_in_default, breaks = c(1:100))
