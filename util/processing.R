# Data processing
reedsmith_19472020 <- read_csv("data/dataverse_files/Reed-Smith-1947-2017-for_Dai_Sasaki.csv") %>% 
  filter(!is.na(age)) %>% 
  filter(!is.na(result))

reedsmith_2021 <- read_csv("data/dataverse_files/candidates_2021_01Dec2021.csv") %>% 
  mutate(kobo = as.numeric(kobo), 
         ku_totvote = ku_valid, 
         inc = incumbency, 
         party_en = party) %>% 
  mutate(inc = ifelse(inc == "現", 1, 0)) %>% 
  filter(!is.na(age)) %>% 
  filter(!is.na(result))

# Merge two datasets for different periods
reedsmith <- bind_rows(reedsmith_19472020, reedsmith_2021)
summary(as.factor(reedsmith$dynasty))

# Handle missing data. Mainly record zeros for missing values in dummies.
reedsmith <- reedsmith %>% 
  mutate(
    # 1. kobo
    kobo = ifelse(
      is.na(kobo), 
      0, 
      ifelse(
        kobo == 0, 
        0, 
        1
      )
    ), 
    # 2. dynasty
    dynasty = ifelse(
      is.na(dynasty), 
      0, 
      ifelse(
        dynasty %in% c(1, 2017), 
        1, 
        0
      )
    )
  )

# Write the complete csv; do not forget to create a csv copy for inspection!
write_csv(reedsmith_19472020, "data/dataverse_files/Reedsmith_19472020.csv")
write_csv(reedsmith, "data/dataverse_files/Reedsmith_complete.csv")


# Check the datasets; especially those of 2021
summary(reedsmith$ku_vote)
summary(reedsmith_19472020$ku_vote)
summary(reedsmith_2021$ku_vote)

# missing data check
data <- read_csv("data/dataverse_files/Reedsmith_complete.csv") %>% 
  mutate(ku_voteshare = ku_vote / ku_totvote * 100, 
         inc = ifelse(inc == 0, 0, 1), 
         ldp = ifelse(party_en == "LDP", 1, 0))

# smaller dataset focusing on snv constituencies
snv <- data %>% 
  filter(year >= 1996) %>% 
  filter(result %in% c(0, 1, 2))

# further smaller dataset 
# retain observations for which some data are missing
snv.NA <- snv %>% 
  filter(is.na(ku_voteshare) == TRUE | 
         is.na(age) == TRUE |  # age missing
         is.na(exp) == TRUE) # exp / ldp / seshu missing 

