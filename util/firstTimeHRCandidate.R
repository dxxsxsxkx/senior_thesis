# create a dataset of first-time winners in the HR election
# based on JHRED dataset. 

library(tidyverse)

data.jhred <- read_csv("./data/dataverse_files/Reedsmith_complete.csv")

data.jhred.firstrun <- data.jhred %>% 
  filter(
    # year >= 1996,  # after the electoral reform
    totcruns == 1
  )

write_csv(
  data.jhred.firstrun, 
  "./data/dataverse_files/firsttimeRun.csv"
)
