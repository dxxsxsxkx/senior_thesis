# create a dataset of first-time winners in the HR election
# based on JHRED dataset. 

library(tidyverse)

data.jhred <- read_csv("./data/dataverse_files/Reedsmith_complete.csv")

data.jhred.first <- data.jhred %>% 
  filter(
    # year >= 1996,  # after the electoral reform
    totcwins == 1,  # first-time win
    result %in% c(1, 2, 3, 4, 5)  # gets elected
  )

write_csv(
  data.jhred.first, 
  "./data/dataverse_files/firsttimeW.csv"
)
