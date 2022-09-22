#----# SCRIPT HEADER #----------#
# Date: 15/09/22
# Author: Group 3
# File name: analysis_of_data
# Description: A script for data analysis
#------------------------------#

#Before running this script, make sure to run tidying_data.R

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

t.test(ct_result ~ result, data = data_result_ct_analysis_2)
#the t-test found a statistically significant difference between positive and negative tests in ct_results
#the positive group had a lower mean in ct_results

#---------------------------------#
#ANALYSIS 3
#Does the number of positive tests depend on the `pan_day`?
#Simply put, does the number of positive tests depend on how long it has been since the pandemic started 
lm(pan_day~result,data=data_wrangled) %>% 
  broom::tidy()

#The p-value is 0.643 for positive results, which is above < 0.05, and therefore not significant.
#The number of positive tests does therefore not depend on how long it had been since the pandemic. 


#---------------------------------#
#ANALYSIS 4
#Analyzing the data set to find out if there is an association between age of the individual and the test result
ttestresult <-
  data_wrangled %>% 
  group_by(age, result) %>% 
  mutate(age = log(age)) %>%
  t.test(age~result, data = .)
ttestresult

ttestresult %>%
  summary()