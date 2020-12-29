# Author: Artemas Wang

# Loading required packages
library(data.table)
library(tidyverse)
library(tidyr)

# Reading in data
custom_fields <- fread(paste0("sample_data1.csv"), header = T, stringsAsFactors = F, data.table = T)
cleaned_data <- fread(paste0("sample_data2.csv"), header = T, stringsAsFactors = F, data.table = T)

custom <- custom_fields
custom$Associate_ID <- custom$`Associate ID`

custom <- custom %>% select("Associate_ID", "Position ID","First Name", "Last Name", "Race Description", "Ethnicity", 
                            "First Generation College Graduate")

cleaned_data_final <- cleaned_data

employees <- merge(x = custom, y = cleaned_data_final, by = "Associate_ID", all.x = TRUE)

employees$Race.Description <- ifelse(employees$`Race Description` == employees$Race_Description, 
                                     employees$Race_Description, employees$`Race Description`)

employees$Ethnicity <- ifelse(employees$Ethnicity.x == employees$Ethnicity.y, 
                              employees$Ethnicity.y, employees$Ethnicity.x)

employees$First.Gen.College.Grad <- ifelse(employees$`First Generation College Graduate` == employees$First_Generation_College_Graduate, 
                                           employees$First_Generation_College_Graduate, employees$`First Generation College Graduate`)

variable_selection <- c(1:4, 14:16)
employees <- employees %>% select(variable_selection)


# Recreating fields
employees$Race.Description <- ifelse(employees$Race.Description == "" |
                                        is.na(employees$Race.Description), "Not Specified", 
                                        employees$Race.Description)

employees$Ethnicity <- ifelse(employees$Race.Description == "" |
                                       is.na(employees$Ethnicity) , "Not Specified", 
                                       employees$Ethnicity)

employees$First.Gen.College.Grad <- ifelse(employees$First.Gen.College.Grad == "" |
                                       is.na(employees$First.Gen.College.Grad), "Not Specified", 
                                       employees$First.Gen.College.Grad)

# Writing to CSV
write.csv(employees, file="Employees Final.csv", row.names = FALSE)


