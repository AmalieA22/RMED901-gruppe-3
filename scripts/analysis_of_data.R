#----# SCRIPT HEADER #----------#
# Date: 15/09/22
# Author: Group 3
# File name: analysis_of_data
# Description: A script for data analysis
#------------------------------#

#Loading packages
library(ggplot2)
library(tidyverse)
library(here)

#Reading the data from earlier script
data_nontidy <- read_delim(here("data", "copy_exam_nontidy.txt"))

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
  distinct() %>%
  pivot_wider(names_from = "time measurement", values_from = "value")

data_join <- read.delim(here("data", "copy_exam_joindata.txt"))

data_wrangled <- 
  data_tidy %>% 
  select(-row,-"1_test_id", -demo_group) %>% 
  mutate(age = as.numeric(age),
         pan_day = as.numeric(pan_day),
         drive_thru_ind = as.numeric(drive_thru_ind),
         ct_result = as.numeric(ct_result),
         id = as.numeric(id)) %>% 
  mutate(rec_ver_tat= if_else(rec_ver_tat>=100, "High", "Low")) %>% 
  mutate(pan_weeks = pan_day / 7) %>% 
  mutate(drive_thru_ind = if_else(drive_thru_ind == 1, "Yes", "No")) %>% 
  mutate(ct_order_result = ct_result * orderset) %>% 
  select(c(id, age, gender), everything()) %>%
  arrange(id) %>% 
  left_join(data_join)

#------------------------------#
#ANALYSIS 1

#Are there more positive tests in the drive-through?

#First, we count the number of positives test in the drive-through and elsewhere,
#to give us an estimate of the difference
data_wrangled %>%
  group_by(result == "positive") %>%
  count(drive_thru_ind == "Yes")
#This returns 479 patients with positive results at a drive-through
#It also returns 386 patients with positive results, but not at a drive-through

#We will conduct a t-test to see if there is a significant difference between the groups
data_wrangled %>%
  mutate(result = if_else(result == "positive", 1, 0)) %>% 
  t.test(result~drive_thru_ind, data = .) %>%
  broom::tidy()
#Invalid and negative results are coded as 0, positive results as 1.
#The table shows a p-value of 0.0172, which is below 0.05. 
#We can therefore say that there is a significant difference between the number 
#of positive results at the drive through and at other places.

#------------------------------#
#ANALYSIS 2

#Analysis to investigate if there is a difference in the distribution of ct_results between outcome groups

#First looking at the variables in question
glimpse(data_wrangled)

#I will make a dataset without "invalid" in result and "NA" in ct_result
data_result_ct_analysis <-
  data_wrangled %>%
  subset(ct_result != "NA") %>%
  subset(result != "invalid")

data_result_ct_analysis 
#This dataset should be more fitting. I will now recode results to positive=1 and negative=0
data_result_ct_analysis_2 <- 
  data_result_ct_analysis %>%
  mutate(result = if_else(result == "positive", 1, 0))

data_result_ct_analysis_2

t.test(ct_result ~ result, 
       data = data_result_ct_analysis_2)
#the t-test found a statistically significant difference between positive and negative tests in ct_results
#the positive group had a lower mean in ct_results

#----------------------------#
#ANALYSIS 3

#analyzing the data set to find out if there is an association between age of the individual and the test result

data_wrangled %>% 
  subset(result != "invalid") %>% 
  mutate(result = if_else(result == "positive", 1, 0)) %>% 
  group_by(age, result) %>% 
  mutate(age = log(age)) %>%
  t.test(age ~ result, data = .)

ttestresult

#I will make a dataset without "invalid" in result and "NA" in ct_result
data_result_ct_analysis <-
  data_wrangled %>%
  subset(result != "invalid")

data_result_ct_analysis 
#This dataset should be more fitting. I will now recode results to positive=1 and negative=0
data_result_ct_analysis_2 <- 
  data_result_ct_analysis %>%
  mutate(result = if_else(result == "positive", 1, 0))

ttestresult %>%
  summary()
