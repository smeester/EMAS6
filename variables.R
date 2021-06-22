file_location <- 'C:/Users/smeester/Documents/AGAI/EMAS10/Case 6/Data aanlevering/emas_dataset.csv'



field_names_analysis <- c("POOL_ID","LOAN_ID","ACT_PERIOD","CHANNEL","SELLER","SERVICER","MASTER_SERVICER",
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
                 "HIGH_LOAN_TO_VALUE_HLTV_REFINANCE_OPTION_INDICATOR","DEAL_NAME","RE_PROCS_FLAG")

# Dit zijn kolomnamen die niet leeg zijn

field_names_filled <- c("LOAN_ID", "ACT_PERIOD", "CHANNEL", "SELLER", "SERVICER", 
                       "ORIG_RATE", "CURR_RATE", "ORIG_UPB", "CURRENT_UPB", 
                       "ORIG_TERM", "ORIG_DATE", "FIRST_PAY", "LOAN_AGE", 
                       "REM_MONTHS", "ADJ_REM_MONTHS", "MATR_DT", "OLTV", 
                       "OCLTV", "NUM_BO", "DTI", "CSCORE_B", "CSCORE_C", 
                       "FIRST_FLAG", "PURPOSE", "PROP", "NO_UNITS", "OCC_STAT", 
                       "STATE", "MSA", "ZIP", "MI_PCT", "PRODUCT", "PPMT_FLG", 
                       "IO", "DLQ_STATUS", "PMT_HISTORY", "MOD_FLAG", 
                       "Zero_Bal_Code", "ZB_DTE", "LAST_UPB", "TOT_SCHD_PRNCPL", 
                       "LAST_PAID_INSTALLMENT_DATE", "FORECLOSURE_DATE", 
                       "DISPOSITION_DATE", "FORECLOSURE_COSTS", 
                       "PROPERTY_PRESERVATION_AND_REPAIR_COSTS", 
                       "ASSET_RECOVERY_COSTS", 
                       "MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS", 
                       "ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY", 
                       "NET_SALES_PROCEEDS", "CREDIT_ENHANCEMENT_PROCEEDS", 
                       "REPURCHASES_MAKE_WHOLE_PROCEEDS", 
                       "OTHER_FORECLOSURE_PROCEEDS", "NON_INTEREST_BEARING_UPB", 
                       "PRINCIPAL_FORGIVENESS_AMOUNT", "MI_TYPE", "SERV_IND", 
                       "HOMEREADY_PROGRAM_INDICATOR", 
                       "FORECLOSURE_PRINCIPAL_WRITE_OFF_AMOUNT", 
                       "RELOCATION_MORTGAGE_INDICATOR", 
                       "PROPERTY_INSPECTION_WAIVER_INDICATOR", 
                       "HIGH_BALANCE_LOAN_INDICATOR", "FORBEARANCE_INDICATOR", 
                       "HIGH_LOAN_TO_VALUE_HLTV_REFINANCE_OPTION_INDICATOR", 
                       "RE_PROCS_FLAG", "ADR_TYPE", "ADR_COUNT", "ADR_UPB")

field_names_empty <- c("POOL_ID", "MASTER_SERVICER", "ISSUANCE_UPB", 
                       "FIRST_PAY_IO", "MNTHS_TO_AMTZ_IO", "MI_CANCEL_FLAG", 
                       "RPRCH_DTE", "CURR_SCHD_PRNCPL", "UNSCHD_PRNCPL_CURR", 
                       "ORIGINAL_LIST_START_DATE", "ORIGINAL_LIST_PRICE", 
                       "CURRENT_LIST_START_DATE", "CURRENT_LIST_PRICE", 
                       "ISSUE_SCOREB", "ISSUE_SCOREC", "CURR_SCOREB", 
                       "CURR_SCOREC", "CURRENT_PERIOD_MODIFICATION_LOSS_AMOUNT", 
                       "CUMULATIVE_MODIFICATION_LOSS_AMOUNT", 
                       "CURRENT_PERIOD_CREDIT_EVENT_NET_GAIN_OR_LOSS", 
                       "CUMULATIVE_CREDIT_EVENT_NET_GAIN_OR_LOSS", 
                       "ZERO_BALANCE_CODE_CHANGE_DATE", 
                       "LOAN_HOLDBACK_INDICATOR", "LOAN_HOLDBACK_EFFECTIVE_DATE", 
                       "DELINQUENT_ACCRUED_INTEREST", "ARM_5_YR_INDICATOR", 
                       "ARM_PRODUCT_TYPE", "MONTHS_UNTIL_FIRST_PAYMENT_RESET", 
                       "MONTHS_BETWEEN_SUBSEQUENT_PAYMENT_RESET", 
                       "INTEREST_RATE_CHANGE_DATE", "PAYMENT_CHANGE_DATE", 
                       "ARM_INDEX", "ARM_CAP_STRUCTURE", 
                       "INITIAL_INTEREST_RATE_CAP", 
                       "PERIODIC_INTEREST_RATE_CAP", 
                       "LIFETIME_INTEREST_RATE_CAP", 
                       "MARGIN", "BALLOON_INDICATOR", "PLAN_NUMBER", "DEAL_NAME")



field_names_lgd <- c("LOAN_ID", "ACT_PERIOD", "ORIG_RATE", "CURR_RATE", "ORIG_UPB", "CURRENT_UPB", 
                     "ORIG_TERM", "ORIG_DATE", "FIRST_PAY", "LOAN_AGE", 
                     "REM_MONTHS", "ADJ_REM_MONTHS", "MATR_DT", "OLTV", 
                     "OCLTV", "NUM_BO", "DTI", "CSCORE_B", "CSCORE_C", 
                     "FIRST_FLAG", "PURPOSE", "PROP", "NO_UNITS", "OCC_STAT", 
                     "STATE", "MSA", "ZIP", "MI_PCT", "PRODUCT", "PPMT_FLG", 
                     "IO", "DLQ_STATUS", "PMT_HISTORY", "MOD_FLAG", 
                     "Zero_Bal_Code", "ZB_DTE", "LAST_UPB", "TOT_SCHD_PRNCPL", 
                     "LAST_PAID_INSTALLMENT_DATE", "FORECLOSURE_DATE", 
                     "DISPOSITION_DATE", "FORECLOSURE_COSTS", 
                     "PROPERTY_PRESERVATION_AND_REPAIR_COSTS", 
                     "ASSET_RECOVERY_COSTS", 
                     "MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS", 
                     "ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY", 
                     "NET_SALES_PROCEEDS", "CREDIT_ENHANCEMENT_PROCEEDS", 
                     "REPURCHASES_MAKE_WHOLE_PROCEEDS", 
                     "OTHER_FORECLOSURE_PROCEEDS", "NON_INTEREST_BEARING_UPB", 
                     "PRINCIPAL_FORGIVENESS_AMOUNT", "MI_TYPE", "SERV_IND", 
                     "HOMEREADY_PROGRAM_INDICATOR", 
                     "FORECLOSURE_PRINCIPAL_WRITE_OFF_AMOUNT", 
                     "RELOCATION_MORTGAGE_INDICATOR", 
                     "PROPERTY_INSPECTION_WAIVER_INDICATOR", 
                     "HIGH_BALANCE_LOAN_INDICATOR", "FORBEARANCE_INDICATOR", 
                     "HIGH_LOAN_TO_VALUE_HLTV_REFINANCE_OPTION_INDICATOR", 
                     "RE_PROCS_FLAG", "ADR_TYPE", "ADR_COUNT", "ADR_UPB")


all_field_names <- c("POOL_ID", 
                     "LOAN_ID", 
                     "ACT_PERIOD", 
                     "CHANNEL", 
                     "SELLER", 
                     "SERVICER", 
                     "MASTER_SERVICER", 
                     "ORIG_RATE", 
                     "CURR_RATE", 
                     "ORIG_UPB", 
                     "ISSUANCE_UPB", 
                     "CURRENT_UPB", 
                     "ORIG_TERM", 
                     "ORIG_DATE", 
                     "FIRST_PAY", 
                     "LOAN_AGE", 
                     "REM_MONTHS", 
                     "ADJ_REM_MONTHS", 
                     "MATR_DT", 
                     "OLTV", 
                     "OCLTV", 
                     "NUM_BO", 
                     "DTI", 
                     "CSCORE_B", 
                     "CSCORE_C", 
                     "FIRST_FLAG", 
                     "PURPOSE", 
                     "PROP", 
                     "NO_UNITS", 
                     "OCC_STAT", 
                     "STATE", 
                     "MSA", 
                     "ZIP", 
                     "MI_PCT", 
                     "PRODUCT", 
                     "PPMT_FLG", 
                     "IO", 
                     "FIRST_PAY_IO", 
                     "MNTHS_TO_AMTZ_IO", 
                     "DLQ_STATUS", 
                     "PMT_HISTORY", 
                     "MOD_FLAG", 
                     "MI_CANCEL_FLAG", 
                     "Zero_Bal_Code",
                     "ZB_DTE", 
                     "LAST_UPB", 
                     "RPRCH_DTE", 
                     "CURR_SCHD_PRNCPL", 
                     "TOT_SCHD_PRNCPL", 
                     "UNSCHD_PRNCPL_CURR", 
                     "LAST_PAID_INSTALLMENT_DATE",
                     "FORECLOSURE_DATE", 
                     "DISPOSITION_DATE", 
                     "FORECLOSURE_COSTS", 
                     "PROPERTY_PRESERVATION_AND_REPAIR_COSTS", 
                     "ASSET_RECOVERY_COSTS", 
                     "MISCELLANEOUS_HOLDING_EXPENSES_AND_CREDITS", 
                     "ASSOCIATED_TAXES_FOR_HOLDING_PROPERTY", 
                     "NET_SALES_PROCEEDS", 
                     "CREDIT_ENHANCEMENT_PROCEEDS", 
                     "REPURCHASES_MAKE_WHOLE_PROCEEDS", 
                     "OTHER_FORECLOSURE_PROCEEDS", 
                     "NON_INTEREST_BEARING_UPB", 
                     "PRINCIPAL_FORGIVENESS_AMOUNT", 
                     "ORIGINAL_LIST_START_DATE", 
                     "ORIGINAL_LIST_PRICE", 
                     "CURRENT_LIST_START_DATE", 
                     "CURRENT_LIST_PRICE", 
                     "ISSUE_SCOREB", 
                     "ISSUE_SCOREC", 
                     "CURR_SCOREB", 
                     "CURR_SCOREC", 
                     "MI_TYPE", 
                     "SERV_IND", 
                     "CURRENT_PERIOD_MODIFICATION_LOSS_AMOUNT", 
                     "CUMULATIVE_MODIFICATION_LOSS_AMOUNT", 
                     "CURRENT_PERIOD_CREDIT_EVENT_NET_GAIN_OR_LOSS", 
                     "CUMULATIVE_CREDIT_EVENT_NET_GAIN_OR_LOSS", 
                     "HOMEREADY_PROGRAM_INDICATOR", 
                     "FORECLOSURE_PRINCIPAL_WRITE_OFF_AMOUNT", 
                     "RELOCATION_MORTGAGE_INDICATOR", 
                     "ZERO_BALANCE_CODE_CHANGE_DATE", 
                     "LOAN_HOLDBACK_INDICATOR", 
                     "LOAN_HOLDBACK_EFFECTIVE_DATE", 
                     "DELINQUENT_ACCRUED_INTEREST", 
                     "PROPERTY_INSPECTION_WAIVER_INDICATOR", 
                     "HIGH_BALANCE_LOAN_INDICATOR", 
                     "ARM_5_YR_INDICATOR", 
                     "ARM_PRODUCT_TYPE", 
                     "MONTHS_UNTIL_FIRST_PAYMENT_RESET", 
                     "MONTHS_BETWEEN_SUBSEQUENT_PAYMENT_RESET", 
                     "INTEREST_RATE_CHANGE_DATE", 
                     "PAYMENT_CHANGE_DATE", 
                     "ARM_INDEX", 
                     "ARM_CAP_STRUCTURE", 
                     "INITIAL_INTEREST_RATE_CAP", 
                     "PERIODIC_INTEREST_RATE_CAP", 
                     "LIFETIME_INTEREST_RATE_CAP", 
                     "MARGIN", 
                     "BALLOON_INDICATOR", 
                     "PLAN_NUMBER", 
                     "FORBEARANCE_INDICATOR", 
                     "HIGH_LOAN_TO_VALUE_HLTV_REFINANCE_OPTION_INDICATOR", 
                     "DEAL_NAME", 
                     "RE_PROCS_FLAG", 
                     "ADR_TYPE", 
                     "ADR_COUNT", 
                     "ADR_UPB")

create_default_graph <- function(df_input){
  df <- df_input
  df$STATUS[df$DEFAULT_FLAG == 1] <-  "Default" 
  df$STATUS[df$OUT_DEFAULT == 1] <- "Recovered" 
  df$STATUS[df$FORECLOSURE_INDICATOR == 1] <- "Foreclosed"
  df$STATUS[df$OUT_DEFAULT == 1 && df$Zero_Bal_Code != 1] <- "Sold"
  #df$COVID == 1 & df$STATUS == "Default"
  df$STATUS[df$COVID == 1 & df$STATUS == "Default" & df$ACT_PERIOD >= dmy("01-01-2019")] <- "Covid Default"
  df$STATUS[df$COVID == 1 & df$STATUS == "Recovered"] <- "Covid Recovered"
  
  sub_select <- df %>% filter(!is.na(STATUS)) %>% 
    group_by(ACT_PERIOD,  STATUS) %>% select(ACT_PERIOD, STATUS) %>% 
    summarise(n())
  
  colnames(sub_select) <- c("Date","Status", "Number")
  sub_select$Status <- factor(sub_select$Status, levels = c(  "Covid Recovered", "Recovered", "Foreclosed", "Covid Default", "Default" ))
  sub_select$Number[sub_select$Status %in% c("Foreclosed", "Recovered", "Covid Recovered")] <- -1 * sub_select$Number[sub_select$Status %in% c("Foreclosed", "Recovered", "Covid Recovered")] 
  
  ggplot(sub_select, aes(x = Date, y = Number, fill = Status)) +
    geom_bar(stat="identity") + 
    #scale_fill_manual(values = c("#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#E69F00"))+
    scale_fill_grey() + 
    ggtitle("Mortgage Default, recoveries and foreclosures over time")
  
}

create_pie_chart_fc <- function(df_input){
  df <- df_input
  df[df$Zero_Bal_Code == 1 , ]$ADR_TYPE <- "P"
  df[df$ADR_TYPE == "" , ]$ADR_TYPE <- "9"
  
  # Maak dataframe voor aantal foreclosure vs niet foreclosed
  df_fc_summary <- df %>% filter(OUT_DEFAULT == 1) %>% 
    select(FORECLOSURE_INDICATOR) %>%
    group_by(FORECLOSURE_INDICATOR) %>% summarise(n())
  colnames(df_fc_summary) <- c("category", "number")
  df_fc_summary$category[df_fc_summary$category == 0] <- "not foreclosed"
  df_fc_summary$category[df_fc_summary$category == 1] <- "foreclosed"
  df_fc_summary$number <- round((df_fc_summary$number / sum(df_fc_summary$number))*100, 1)
  
  ggplot(df_fc_summary, aes(x="", y=number, fill=category)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    scale_fill_grey() + 
    geom_text(aes(label = paste0(number, "%")), 
              position = position_stack(vjust=0.5),
              colour="#FFFFFF") + 
    theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
    ggtitle("Percentage of foreclosure vs recoveries")
}


create_hist_length_default <- function(df_input){
  df <- df_input
  moments_out_default <- df %>% filter(OUT_DEFAULT == 1) 
  summary <- moments_out_default %>% group_by(LENGTH_IN_DEFAULT, FORECLOSURE_INDICATOR) %>% 
    summarise(n())
  
  colnames(summary) <- c("Length", "Foreclosed", "Number")
  summary$Foreclosed[summary$Foreclosed == 1] <- "Foreclosed"
  summary$Foreclosed[summary$Foreclosed == 0] <- "Not Foreclosed"
  
  ggplot(summary, aes(x = Length, y = Number, fill = Foreclosed)) +
    geom_bar(stat="identity") +
    scale_fill_grey() + 
    scale_y_continuous(limits = c(0, 500)) + 
    xlab("Months in default") +
    ggtitle("Amount of time in months a defaulted mortgage is in default")
}
