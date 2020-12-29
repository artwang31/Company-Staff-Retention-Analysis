# Author: Artemas Wang

# Loading required packages
library(data.table)
library(tidyverse)
library(tidyr)

# Reading in data
original_data <- fread(paste0("Sample Data.csv"), header = T, stringsAsFactors = F, data.table = T)
original_roster_data <- fread(paste0("Employee Roster-Table.1csv"), header = T, stringsAsFactors = F, data.table = T)

roster <- original_roster_data
data_sample <- original_data

# Learning about data
nrow(data_sample)
nrow(roster) 
nrow(colnames)

### Cleaning Org Health data

# Getting rid of the useless extra "Associate ID" column/variable
data_sample <- data_sample[3:nrow(data_sample),1:53]

# Selecting relevant variables and getting rid of duplicate Associate Id entries
data_sample <- data_sample %>% select("Race/Ethnicity", "FG", "Associate ID") %>% 
                    distinct(`Associate ID`,.keep_all = TRUE)
str(data_sample)

# Renaming Associate ID for future merge
data_sample$Associate_ID <- data_sample$`Associate ID`
data_sample$`Associate ID` <- NULL

# Checking to see if there are any NA values within observations
which(is.na(data_sample$`Race/Ethnicity`))
which(is.na(data_sample$FG))
which(is.na(data_sample$`Associate ID`))

## Checking to see unique values within each variable & fixing them

# Race and Ethnicity into Separate Columns
data_sample$Race_Description <- ifelse(data_sample$`Race/Ethnicity` == "" |
                                      data_sample$`Race/Ethnicity` == "I prefer not to respond" , "Not Specified", 
                                      data_sample$`Race/Ethnicity`)
data_sample$`Race/Ethnicity` <- NULL 

data_sample$Ethnicity <- ifelse(data_sample$Race_Description == "Not Specified" |
                               data_sample$Race_Description == "", "Not Specified",
                        ifelse(data_sample$Race_Description == "Asian" |
                               data_sample$Race_Description == "White" |
                               data_sample$Race_Description == "American Indian or Alaska Native" |
                               data_sample$Race_Description == "Native Hawaiian or Other Pacific Islander" |
                               data_sample$Race_Description == "Two or More Races" |
                               data_sample$Race_Description == "Black or African-American" |
                               data_sample$Race_Description == "Other", "",
                        ifelse(data_sample$Race_Description == "Hispanic or Latino", "Hispanic or Latino", "Not Hispanic or Latino")))
                        
# First Grad
data_sample$First_Generation_College_Graduate <- ifelse(data_sample$FG == "" |
                        data_sample$FG == "I prefer not to respond", "Not Specified", data_sample$FG)
data_sample$FG <- NULL

## Cleaning Roster data
# Selecting relevant variables and getting rid of duplicate Associate Id entries
roster <- roster %>% select("NAME","ASSOCIATE ID", "POSITION ID") %>% 
              distinct(`ASSOCIATE ID`,.keep_all = TRUE) # 2350 rows now

# Renaming Associate ID for future merge
roster$Associate_ID <- roster$`ASSOCIATE ID`
roster$`ASSOCIATE ID` <- NULL

# Checking to see if there are any NA values within observations=
which(is.na(roster$NAME))
which(is.na(roster$`ASSOCIATE ID`))
which(is.na(roster$`JOB TITLE`))

# Splitting Name into First and Last
roster <- separate(data = roster, col = NAME, into = c("Last Name", "First Name"), sep = "\\,")
trimws(roster$`Last Name`)
trimws(roster$`First Name`)

# Merging two datasets on Associate ID
employees_final <- merge(x = data_sample, y = roster, by = "Associate_ID", all.x = TRUE)
employees_final <- employees_final %>% select("Associate_ID", "POSITION ID", "First Name", "Last Name",
                                              "Race_Description", "Ethnicity", "First_Generation_College_Graduate")


# Writing to CSV
write.csv(employees_final, file="Employees to ADP.csv", row.names = FALSE)




