# This script creates a survival data. 

# Library
library("tidyverse")
library("survival")

# Paths
path.data.first.win <- "./data/dataverse_files/firsttimeW.csv"
path.data.first.run <- "./data/dataverse_files/firsttimeRun.csv"
path.save.first.win <- "./careerEffect/data/first_win.RDS"
path.save.first.run <- "./careerEffect/data/first_run.RDS"

# Data
data.firstwin  <- read_csv(path.data.first.win) %>% 
  mutate(
    age_eligible = 25, 
    status = 1
  )
data.firstrun  <- read_csv(path.data.first.run) %>% 
  mutate(
    age_eligible = 25, 
    status = 1
  )

# Create a survival object
data.firstrun$age.surv <- Surv(
  time = (data.firstrun$age - data.firstrun$age_eligible + 1),  # How many years after the age of 25
  event = data.firstrun$status, 
  type = "right"
)
data.firstrun$cohort <- cut(
  data.firstrun$byear, 
  breaks = seq(1850, 2020, by = 30), 
  labels = FALSE
)
data.firstrun$juku <- ifelse(
  data.firstrun$juku == 0, 
  0, 
  1
)
data.firstrun$is_pure_pr <- ifelse(
  is.na(data.firstrun$ken), 
  1, 
  0
)
# first win
data.firstwin$age.surv <- Surv(
  time = (data.firstwin$age - data.firstwin$age_eligible + 1), 
  event = data.firstwin$status, 
  type = "right"
)
data.firstwin$cohort <- cut(
  data.firstwin$byear, 
  breaks = seq(1850, 2020, by = 30), 
  labels = FALSE
)
data.firstwin$juku <- ifelse(
  data.firstwin$juku == 0, 
  0, 
  1
)
data.firstwin$is_pure_pr <- ifelse(
  is.na(data.firstwin$ken), 
  1, 
  0
)

saveRDS(data.firstwin, path.save.first.win)
saveRDS(data.firstrun, path.save.first.run)
