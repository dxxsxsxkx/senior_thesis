# 

# Library
library(tidyverse)
library(readxl)

# Paths
path.data <- "./data/local/NatoriData_20190125/"

# Data
# 2003
data.election.2003 <- read_excel(
  paste0(path.data, "data1/data2003.xlsx"), 
  na = c("", "無投票")
) %>% 
  group_by(
    `県選挙区名`, `名前`
  ) %>% 
  mutate(
    `得票数` = sum(`得票数`, na.rm = TRUE)
  ) %>% 
  distinct(
    `県選挙区名`, `名前`, .keep_all = TRUE
  )
data.candidate.2003 <- read_excel(
  paste0(path.data, "data3/cand2003.xlsx")
)
# 2007
data.election.2007 <- read_excel(
  paste0(path.data, "data1/data2007.xlsx")
)
data.candidate.2007 <- read_excel(
  paste0(path.data, "data3/cand2007.xlsx")
)
# 2011
data.election.2011 <- read_excel(
  paste0(path.data, "data1/data2011.xlsx")
)
data.candidate.2011 <- read_excel(
  paste0(path.data, "data3/cand2011.xlsx")
)
# 2015
data.election.2015 <- read_excel(
  paste0(path.data, "data1/data2015.xlsx")
)
data.candidate.2015 <- read_excel(
  paste0(path.data, "data3/cand2015.xlsx")
)

# Create merged data for each year
data.2003 <- data.election.2003 %>% 
  full_join(
    data.candidate.2003, 
    by = c("名前" = "氏名", "年齢")
  )
data.2003$名前[
  is.na(data.2003$ふりがな)
]
