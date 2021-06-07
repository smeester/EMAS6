cat("\014")
rm(list=ls())
gc()

library(data.table)

source("variables.R")

length(all_field_names)

filled_columns <- lapply(all_field_names, function(x){
  print(x)
  data <- fread(file_location,
                select=x)
  any(!is.na(data[,1]))
})

filled_columns <- unlist(filled_columns)

filled_field_names <- all_field_names[filled_columns]
empty_field_names <- all_field_names[!filled_columns]
View(filled_field_names)

names_for_declaration <- cat(paste(shQuote(filled_field_names , type="cmd"), collapse=", "))
empty_names_for_declaration <- cat(paste(shQuote(empty_field_names , type="cmd"), collapse=", "))
