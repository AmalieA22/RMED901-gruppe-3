#----# SCRIPT HEADER #----------#
# Date: 12/09/22
# Author: Group 3
# File name: tidying_data
# Description: A script for tidying the exam dataset
#------------------------------#


#Loading and reading a copy version of the data

library(tidyverse)
library(here)


data_nontidy <- read_delim(here("data", "copy_exam_nontidy.txt"))

data_nontidy

#Exploring the data
head(data_nontidy)
tail(data_nontidy)
#Head() and tail() show that gender.age is in 1 column: they should be in 2
#No columns have numbers in the start or contain spaces

summary(data_nontidy)
#Age is a character

glimpse(data_nontidy)
#There are 15 columns and 34,048 rows in this dataset
#There are some NA in the dataset, for example in payor_group and patient_class

skimr::skim(data_nontidy)
#There is 15527 NA for payor_group and 15506 NA for patient_class
#There are 12344 unique values for subject, which may indicate that there are variables that lead to multiple observations
#This shows that time.measurement should be 2 variables: col_rec_tat and rec_ver_tat

naniar::gg_miss_var(data_nontidy)
#The plot shows that we are missing more than 15 000 values in payor_group and patient_class, and around 400 in ct_result


#Tidying the data

#We wrote a pipe that renames columns to not include space or "."
#Separates age and gender into 2 columns and subject into "ID", "first name" and "surname"
#Widens time.measurement to rec_ver_tat and col_rec_tat
data_tidy<-
  data_nontidy %>%
    rename(value=.value,
         pan_day="pan day")%>%
    separate(col = "gender-age",
           into = c("gender", "age"),
           sep = "-")%>%
  separate(col = subject,
           into = c("ID", "first_name", "last_name"),
           sep = " ") %>%
    distinct() %>%
      pivot_wider(names_from = "time measurement", values_from = "value")
#When first running the code without distinct, there would be a warning message since there were a lot of duplicates.
#Distinct() selected only unique/distinct rows from the dataframe. It is now 152 524 rows and 15 columns.

#Changing the type of variables for age, pan_day, drive_thru_ind, ct_result and ID to numeric
data_tidy <-
  data_tidy %>%
  mutate(age = as.numeric(age),
         pan_day = as.numeric(pan_day),
         drive_thru_ind = as.numeric(drive_thru_ind),
         ct_result = as.numeric(ct_result),
         ID = as.numeric(ID))

data_tidy
glimpse(data_tidy)

#Wrote code for arranging the variables correctly
data_tidy <-
  data_tidy %>%
  select(c(ID, age, gender), everything())

#Wrote code to arrange the table according to ID
data_tidy %>%
  arrange(ID)

#New numeric column showing multiplication of ct_result and orderset for each person
data_tidy %>%
  mutate(ct_order_result = ct_result * orderset)

#New column showing drive_thru_ind as Yes/No
data_tidy %>%
  mutate(drive_thru_ind = if_else(drive_thru_ind == 1, "Yes", "No"))