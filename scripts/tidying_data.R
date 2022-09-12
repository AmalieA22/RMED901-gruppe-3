#----# SCRIPT HEADER #----------#
# Date: 12/09/22
# Author: Group 3
# File name: tidying_data
# Description: A script for tidying the exam dataset
#------------------------------#

library(tidyverse)
library(here)

data_nontidy <- read.delim(here("data", "exam_nontidy.txt"))

data_nontidy
