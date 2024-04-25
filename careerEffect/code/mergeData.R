# library
library(tidyverse)
library(stringi)

# data
# JMED
data.jmed <- read_csv(
  "../data/jmed/jmed.csv"
) %>% 
  # prepare hiragana names to match JHRED dataset
  mutate(
    candidate_name_hiragana = stri_trans_general(
      candidate_name_kana, 
      "Katakana-Hiragana"
    )
  ) %>% 
  # remove spaces
  mutate(
    candidate_name_hiragana = stri_replace_all_fixed(
      candidate_name_hiragana, 
      " ", ""
    ), 
    candidate_name = stri_replace_all_fixed(
      candidate_name, 
      " ", ""
    )
  )

# calculate total votes in district-year
data.jmed.grouped <- data.jmed %>% 
  group_by(municipality_code, year) %>%
  summarise(vote_total = sum(`number of votes`))

# merge the grouped data back into the original data frame
data.jmed <- merge(
  data.jmed, data.jmed.grouped, 
  by = c("municipality_code", "year"), 
  all.x = TRUE)  

# calculate vote share
data.jmed$voteshare <- data.jmed$`number of votes` / data.jmed$vote_total

data.jmed <- data.jmed %>% 
  group_by(municipality_code, year) %>% 
  mutate(
    # mark winners in each district-year, based on voteshare variable
    result = ifelse(voteshare == max(voteshare), 1, 0), 
    # mark close-races
    # include: candidates 
    # whose winning margin is less than 10 percentage points
    # exceptions: uncontested / tie
    is_closerace = ifelse(
      # impute 0 for uncontested districts
      (is.na(voteshare) == 1 | voteshare == 1), 
      0, 
      ifelse(
        voteshare == 0.5,  # impute 1 for ties
        1, 
        ifelse(
          (voteshare >= max(voteshare) - 0.15 & 
             voteshare <= max(voteshare[voteshare < max(voteshare)]) + 0.15), 
          1, 
          0
        )
      )
    )
  )

# Close-race dataset
data.jmed.close <- data.jmed %>% 
  filter(is_closerace == 1)

# JHRED
data.jhred <- read_csv("../data/dataverse_files/Reedsmith_complete.csv") %>% 
  filter(
    totcwins == 1,  # never won before
    result %in% c(1, 2, 3, 4, 5),  # and gets elected
    year >= 2006  # period corresponding to JMED
  )

# merge data.jmed.close and data.jhred
# retain only candidates who are present in data.jmed.close
data.jmed.jhred <- merge(
  data.jmed.close, data.jhred, 
  by.x = c("candidate_name_hiragana"), 
  by.y = c("name_kana"), 
  all.x = TRUE
) %>% 
  # remove candidates who are not present in data.jhred
  filter(!is.na(result.y)) %>%   
  # retain only relevant variables
  select(
    candidate_name, candidate_name_kana, 
    name_jp, 
    year.x,  # year in JMED
    municipality_name,  # municipality name in JMED
    voteshare,  # vote share in JMED
    result.x, # result in JMED
    year.y,  # year in JHRED
    result.y,  # result in JHRED
  ) %>% 
  filter(
    year.x < year.y, 
    candidate_name == name_jp
  )

# write datasets
write_csv(
  data.jmed.close, 
  "jmed_close.csv"
)
