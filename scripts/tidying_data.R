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


#TIDYING THE DATA

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
           into = c("id", "first_name", "last_name"),
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
         id = as.numeric(id))

data_tidy
glimpse(data_tidy)


data_tidy <-
  data_tidy %>% 
  select(-row,-"1_test_id", -demo_group)

#A column showing whether rec_ver_tat is higher than 100 or not: values High/Low
data_tidy <-
  data_tidy %>%
  mutate(rec_ver_tat= if_else(rec_ver_tat>=100, "High", "Low"))

#A numeric column showing pan_day in weeks
data_tidy<-
  data_tidy %>% 
  mutate(pan_weeks = pan_day / 7)
glimpse(data_tidy)

#Wrote code for arranging the variables correctly and to arrange the table according to ID
data_tidy <-
  data_tidy %>%
  select(c(id, age, gender), everything()) %>%
  arrange(id)

#New numeric column showing multiplication of ct_result and orderset for each person
data_tidy <- 
  data_tidy %>%
  mutate(ct_order_result = ct_result * orderset)

#New column showing drive_thru_ind as Yes/No
data_tidy <- 
  data_tidy %>%
  mutate(drive_thru_ind = if_else(drive_thru_ind == 1, "Yes", "No"))
  
#Code to join data to the tidy data
data_join <- read.delim(here("data", "copy_exam_joindata.txt"))


data_joined <- 
  data_tidy %>%
  inner_join(data_join)

#CREATING A PIPELINE for day 6

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

glimpse(data_wrangled)

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
  filter(drive_thru_ind == "No"& ct_result < 35) %>%
  head()
#There are no such individuals

#Code to make a table from 2 categorical columns
gender_payor_table <- 
  data_wrangled %>%
  with(table(gender, payor_group))
  
#PLOTS

#Were there more females than males that took the test at a drive through?
data_drivethrough <- data_wrangled %>%
  count(gender, drive_thru_ind)

ggplot(data_drivethrough, aes(x = gender, y = n))+
  geom_col(aes(fill = gender), width = 0.7)+
  xlab("Gender")+
  ylab("Took the test at a drive-through")+
  facet_wrap(facets = vars(drive_thru_ind))+
  labs(fill = "Gender")

#The plot shows that there are slightly more females than males who took the test at a drive through.

#Plot to illustrate sex differences in testing
count_gender <- 
  data_wrangled %>%
  count(gender)
ggplot(data = count_gender, 
       aes(group = gender,
           x = gender, 
           y = n)) +
  geom_col(aes(fill = gender)) +
  scale_fill_brewer(type = "div", palette ="BuPu") +
  ylab("Number of Tests") +
  xlab ("Gender") +
  theme_classic()
#The visualization shows that a few more women got tested, but the difference
#is relatively small

#Plot to illustrate if time spent waiting for test reuslts improves 
#over the course of the pandemic
ggplot(data = data_wrangled,
       aes(x = pan_weeks ,
           y = col_rec_tat)) +
  geom_point() +
  xlab("Days into pandemic") +
  ylab("Time between collection and recieve time")
#When making this plot we see one severe outlier.
#This is likely an error and I will remove it from further vizualisation
#First we find the row with this value:
count_col_rec_tat <- 
  data_wrangled %>%
  count(col_rec_tat, id, pan_weeks)

tail(count_col_rec_tat, 8)

data_wrangled %>%
  group_by(data_wrangled$col_rec_tat)

col_week_data <- data_wrangled %>%
  subset(id != 801)

ggplot(data = col_week_data,
       aes(x = pan_weeks ,
           y = col_rec_tat)) +
  geom_point() +
  xlab("Days into pandemic") +
  ylab("Time between collection and recieve time")
#We still see that there are a few outliers, which make interpretation difficult
#We will remove these as well, to make interpretation easier

col_week_data_2 <- col_week_data %>%
  subset(id != (11684))
col_week_data_3 <- col_week_data_2 %>%
  subset(id != (214))
col_week_data_4 <- col_week_data_3 %>%
  subset(id != (2193))
col_week_data_5 <- col_week_data_4 %>%
  subset(id != (5018))
col_week_data_6 <- col_week_data_5 %>%
  subset(id != (1306))
col_week_data_7 <- col_week_data_6 %>%
  subset(id != (2609))
col_week_data_8 <- col_week_data_7 %>%
  subset(id != (4859))

ggplot(data = col_week_data_8,
       aes(x = pan_weeks ,
           y = col_rec_tat)) +
  geom_point(aes()) +
  geom_smooth(method = "lm") +
  xlab("Weeks into pandemic") +
  ylab("Time between collection and recieve time")



#create a plot that would help to find if the distribution of the ct_results differ with the sex group

data_wrangled_grouped <- data_wrangled %>%
  group_by(gender, ct_result) %>% 
  summarise(sum = sum(ct_result, na.rm=T))
data_wrangled_grouped

ggplot(data_wrangled_grouped,  
       aes(x = as.factor(gender), y = ct_result)) +
  xlab("gender")+
  ylab("distribution of ct_result")+
  geom_boxplot(aes(fill=gender))


