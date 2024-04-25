# Miscellaneousな議論を確認するためのスクリプト
# This file tests miscellaneous things using the cleaned Reed-Smith dataset. 
library(tidyverse)

path.data <- "./data/dataverse_files/Reedsmith_complete.csv"

data <- read_csv(path.data)

# Are celebrity candidates younger than non-celebrity candidates?
data.celeb <- data %>% 
  filter(celeb == 1)
mean(data.celeb$age)  # 51 yrs old

# PRに限定する. 
data.celeb.pr <- data.celeb %>% 
  filter(prcode != 0)
data.celeb.purepr <- data.celeb %>% 
  filter(is.na(ken))
mean(data.celeb.pr$age)  # 50 yrs old
mean(data.celeb.purepr$age)  # 55 yrs old
