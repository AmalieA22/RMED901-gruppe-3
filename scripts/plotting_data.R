#----# SCRIPT HEADER #----------#
# Date: 15/09/22
# Author: Group 3
# File name: plotting_data
# Description: A script for plotting the data
#------------------------------#

#Loading packages
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

#----------------------------------------------#
#PLOT 1

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

#----------------------------------------------#
#PLOT 2

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

#----------------------------------------------#
#PLOT 3

#Plot to illustrate if time spent waiting for test results improves 
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

#----------------------------------------------#
#PLOT 4

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

#----------------------------------------------#
#PLOT 5

#Does the distribution of the `ct_result` differ with `payor_group`?
data_to_be_analyzed<-
  data_wrangled %>%
  count(payor_group,ct_result)

comparing_payor_group_ct <-
  data_to_be_analyzed %>% 
  ggplot(aes(x=payor_group, y = ct_result)) + 
  geom_boxplot(aes(fill=payor_group)) +
  scale_fill_brewer(type="div",palette="GnBu")+
  ylab("CT result") +
  xlab("Payor group") +
  theme_classic()

comparing_payor_group_ct

#The box plots displays the range and the median of ct_result in each group

#----------------------------------------------#
#PLOT 6
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

