#----# SCRIPT HEADER #----------#
# Date: 12/09/22
# Author: Group 3
# File name: tidying_data
# Description: A script for tidying the exam dataset
#------------------------------#


#Loading and reading a copy version of the data

library(tidyverse)
library(here)


data_nontidy <- read.delim(here("data", "copy_exam_nontidy.txt"))

data_nontidy

#Exploring the data
head(data_nontidy)
tail(data_nontidy)

summary(data_nontidy)

glimpse(data_nontidy)

skimr::skim(data_nontidy)

naniar::gg_miss_var(data_nontidy)





#Tidying the data