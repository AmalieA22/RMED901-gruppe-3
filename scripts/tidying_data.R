#----# SCRIPT HEADER #----------#
# Date: 22/09/22
# Author: Group 3
# File name: tidying_data
# Description: A script for tidying, wrangling and stratifying the exam dataset, corresponding to day 5 & 6
#------------------------------#

#Loading packages
library(tidyverse)
library(here)
library(ggplot2)

#Loading and reading a copy version of the data
data_nontidy <- read_delim(here("data", "copy_exam_nontidy.txt"))

data_nontidy

#EXPLORING THE DATA
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

#TIDYING THE DATA ---- Day 5

#We wrote a pipe that renames columns to not include space or "."
#Separates age and gender into 2 columns and subject into "ID", "first name" and "surname"
#Widens time.measurement to rec_ver_tat and col_rec_tat
#Changes the type of variables for age, pan_day, drive_thru_ind, ct_result and ID to numeric
data_tidy<-
  data_nontidy %>%
    rename(value=.value,
         pan_day="pan day")%>%
    separate(col = "gender-age",
           into = c("gender", "age"),
           sep = "-")%>%
  separate(col = subject,
           into = c("id", "first_name", "last_name"),
           sep = " ") %>%
  mutate(age = as.numeric(age),
         pan_day = as.numeric(pan_day),
         drive_thru_ind = as.numeric(drive_thru_ind),
         ct_result = as.numeric(ct_result),
         id = as.numeric(id)) %>%
    distinct() %>%
      pivot_wider(names_from = "time measurement", values_from = "value")
#When first running the code without distinct, there would be a warning message since there were a lot of duplicates.
#Distinct() selected only unique/distinct rows from the dataframe. It is now 152 524 rows and 15 columns.

data_tidy
glimpse(data_tidy)

#Loading data to be joined to the main dataset:
data_join <- read.delim(here("data", "copy_exam_joindata.txt"))


#Wrangling the data --- Day 6

#CREATING A PIPELINE for day 6 which:
#Removes the "row", "1_test_id" and "demo_group" columns
#Adds a column showing whether rec_ver_tat is higher than 100 or not: values High/Low
#A numeric column showing pan_day in weeks
#Arranges the variables correctly and to arrange the table according to ID
#Adds a numeric column showing multiplication of ct_result and orderset for each person
#Adds column showing drive_thru_ind as Yes/No
#Joins the extra data to the tidy data
data_wrangled <- 
  data_tidy %>% 
  select(-row,-"1_test_id", -demo_group) %>% 
  mutate(age = as.numeric(age),
         pan_day = as.numeric(pan_day),
         drive_thru_ind = as.numeric(drive_thru_ind),
         ct_result = as.numeric(ct_result),
         id = as.numeric(id),
         rec_ver_tat= if_else(rec_ver_tat>=100, "High", "Low"),
         pan_weeks = pan_day / 7,
         drive_thru_ind = if_else(drive_thru_ind == 1, "Yes", "No"),
         ct_order_result = ct_result * orderset) %>%
  select(c(id, age, gender), everything()) %>%
  arrange(id) %>% 
  inner_join(data_join)

#Exploring the data

glimpse(data_wrangled)
#This shows the 18 columns and the data in the columns. 

head(data_wrangled)
#When looking at this one can see the first 6 rows. Each row describes a patient, and the following columns is giving us info about them. 

tail(data_wrangled)
#Further exploring the data this shows the last 6 rows. This command is pretty much giving us the same info as the last one, but it looks the same, so that is good. 

#EXPLORING MISSING DATA

naniar::gg_miss_var(data_wrangled)
#This returns more than 8000 missing values for payor_group and patient_class, and <500 for ct_result

#Further exploring missing values in payor_group, patient_class and ct_result
data_wrangled %>%
  filter(is.na(payor_group))%>%
    count(payor_group)

data_wrangled %>%  
  filter(is.na(patient_class))%>%
    count(patient_class)
  
data_wrangled %>%
  filter(is.na(ct_result))%>%
    count(ct_result)
#Payor_group returns 7087 NA, patient class returns 7077 NA and ct_result returns 209 NA
#When looking at the dataset, it seems like the patient who tested for covid in a clinical lab does not have any data on payor group or patient class.

data_wrangled %>%
  filter(clinic_name == "clinical lab") %>%
    filter(is.na(payor_group)) %>%
      count(payor_group)

data_wrangled %>%
  filter(clinic_name == "line clinical lab-") %>%
    filter(is.na(payor_group)) %>%
      count(payor_group)

data_wrangled %>%
  filter(clinic_name == "clinical lab") %>%
    filter(is.na(patient_class)) %>%
      count(patient_class)

data_wrangled %>%
  filter(clinic_name == "line clinical lab-") %>%
  filter(is.na(patient_class)) %>%
  count(patient_class)

#Clincal lab: This returns 6407 NA for payor_group and 6406 NA for patient class.
#Line clinical lab: 218 NA for payor_group, 218 NA for patient_class
#The majority of the missing values are therefore connected to the fact that some are tested in a clinical lab


#STRATIFY DATA by a categorical column and report min, max, mean and sd of a numeric column
data_wrangled %>% 
  summarize(min(age, na.rm = T),max(age, na.rm = T),mean(age, na.rm = T), sd(age, na.rm = T))

#Only for persons with ct_result == 45
data_wrangled %>%
  filter(ct_result==45) %>% 
  summarize(min(pan_day, na.rm = T), max(pan_day, na.rm = T), mean(pan_day, na.rm = T), sd(pan_day, na.rm = T))
  
#Only for persons tested pan_day later than 50
#Here I have chosen age as the numeric column to check min, max, mean and sd
data_wrangled %>%
  group_by(pan_day > 50) %>%
  summarise(min(age), max(age), mean(age), sd(age))
  
#Only for drive_trhu_ind == 0 and ct_result < 35
data_wrangled %>%
  filter(drive_thru_ind == "No" & ct_result < 35) %>%
  head()
#There are no such individuals

#Code to make a table from 2 categorical columns, in this case "gender" and "payor_group"
gender_payor_table <- 
  data_wrangled %>%
  with(table(gender, payor_group))