# packages
library(tidyverse)

# data
data.jhred.first <- read_csv("./data/dataverse_files/firsttimeW.csv")
data.jhred.firstrun <- read_csv("./data/dataverse_files/firsttimeRun.csv")

# proportion of first-time candidates with backgrounds in certain professions 
mean(data.jhred.firstrun$hc, na.rm = TRUE)
mean(data.jhred.firstrun$assy, na.rm = TRUE)
mean(data.jhred.firstrun$mayor, na.rm = TRUE)
mean(data.jhred.firstrun$gov, na.rm = TRUE)
mean(data.jhred.firstrun$law, na.rm = TRUE)
mean(data.jhred.firstrun$sec, na.rm = TRUE)
mean(data.jhred.firstrun$bcrat, na.rm = TRUE)
mean(data.jhred.firstrun$locbur, na.rm = TRUE)
mean(data.jhred.firstrun$med, na.rm = TRUE)
mean(data.jhred.firstrun$kobo, na.rm = TRUE)
sum(data.jhred.firstrun[data.jhred.firstrun$assy == 1 & data.jhred.firstrun$sec == 1, ]$assy, na.rm = TRUE) / length(data.jhred.firstrun$assy)
sum(data.jhred.firstrun[data.jhred.firstrun$bcrat == 1 & data.jhred.firstrun$kobo == 1, ]$bcrat, na.rm = TRUE) / length(data.jhred.firstrun$assy)

# average age of first-time mps with certain career backgrounds
summary(data.jhred.firstrun$age, na.rm = TRUE)  # all
summary(data.jhred.firstrun$age[data.jhred.firstrun$hc == 1], na.rm = TRUE)  # upper house
summary(data.jhred.firstrun$age[data.jhred.firstrun$assy == 1], na.rm = TRUE)  # local assembly
summary(data.jhred.firstrun$age[data.jhred.firstrun$mayor == 1], na.rm = TRUE)  # mayor
summary(data.jhred.firstrun$age[data.jhred.firstrun$gov == 1], na.rm = TRUE)  # governor
summary(data.jhred.firstrun$age[data.jhred.firstrun$law == 1], na.rm = TRUE)  # lawyer
summary(data.jhred.firstrun$age[data.jhred.firstrun$sec == 1], na.rm = TRUE)  # secretary
summary(data.jhred.firstrun$age[data.jhred.firstrun$bcrat == 1], na.rm = TRUE)  # bureaucrat
summary(data.jhred.firstrun$age[data.jhred.firstrun$locbur == 1], na.rm = TRUE)  # local-level civil servant
summary(data.jhred.firstrun$age[data.jhred.firstrun$med == 1], na.rm = TRUE)  # medical
summary(data.jhred.firstrun$age[data.jhred.firstrun$kobo == 1], na.rm = TRUE)  # open recruitment
summary(data.jhred.firstrun[data.jhred.firstrun$assy == 1 & data.jhred.firstrun$sec == 1, ]$age)  # local assembly + secretary

# proportion of first-time mps with backgrounds in certain professions
mean(data.jhred.first$hc, na.rm = TRUE)  # upper house, 5%
mean(data.jhred.first$assy, na.rm = TRUE)  # local assembly, 30%
mean(data.jhred.first$mayor, na.rm = TRUE)  # mayor, 5%
mean(data.jhred.first$gov, na.rm = TRUE)  # prefectural governor, 0.5%
mean(data.jhred.first$law, na.rm = TRUE)  # lawyer, 5%
mean(data.jhred.first$sec, na.rm = TRUE)  # secretary, 15%
mean(data.jhred.first$bcrat, na.rm = TRUE)  # bureaucrat, 15%
mean(data.jhred.first$locbur, na.rm = TRUE)  # local-level civil servant, 5%
mean(data.jhred.first$med, na.rm = TRUE)  # medical, 5%
mean(data.jhred.first$kobo, na.rm = TRUE)  # open recruitment, 5%
sum(data.jhred.first[data.jhred.first$assy == 1 & data.jhred.first$sec == 1, ]$assy, na.rm = TRUE) / length(data.jhred.first$assy)

# average age of first-time mps with certain career backgrounds
summary(data.jhred.first$age, na.rm = TRUE)  # all, 47.9 yo
summary(data.jhred.first$age[data.jhred.first$hc == 1], na.rm = TRUE)  # upper house, 54.2yo
summary(data.jhred.first$age[data.jhred.first$assy == 1], na.rm = TRUE)  # local assembly, 50.0yo
summary(data.jhred.first$age[data.jhred.first$mayor == 1], na.rm = TRUE)  # mayor, 53.1yo
summary(data.jhred.first$age[data.jhred.first$gov == 1], na.rm = TRUE)  # governor, 56yo
summary(data.jhred.first$age[data.jhred.first$law == 1], na.rm = TRUE)  # lawyer, 47.4yo
summary(data.jhred.first$age[data.jhred.first$sec == 1], na.rm = TRUE)  # secretary, 43.7yo
summary(data.jhred.first$age[data.jhred.first$bcrat == 1], na.rm = TRUE)  # bureaucrat, 49.3yo
summary(data.jhred.first$age[data.jhred.first$locbur == 1], na.rm = TRUE)  # local-level civil servant, 52.4yo
summary(data.jhred.first$age[data.jhred.first$med == 1], na.rm = TRUE)  # medical, 47.9 yo
summary(data.jhred.first$age[data.jhred.first$kobo == 1], na.rm = TRUE)  # open recruitment, 41.4 yo
summary(data.jhred.first[data.jhred.first$assy == 1 & data.jhred.first$sec == 1, ]$age)  # local assembly + secretary

# age distribution of first-time winners
data.jhred.first %>% 
  count(age) %>% 
  mutate(prop = n / sum(n))
# How many candidates with specific backgrounds win at different ages? 
data.age <- data.jhred.first %>% 
  count(age) %>% 
  mutate(prop = n / sum(n)) %>% 
  select(-n) %>% 
  rename(all = prop) %>% 
  # candidates who served in local assemblies
  full_join(
    data.jhred.first[data.jhred.first$assy == 1, ] %>% 
      count(age) %>% 
      mutate(prop = n / sum(n)) %>% 
      select(-n) %>% 
      rename(assy = prop), 
    by = "age"
  ) %>% 
  # previous mayors
  full_join(
    data.jhred.first[data.jhred.first$mayor == 1, ] %>% 
      count(age) %>% 
      mutate(prop = n / sum(n)) %>% 
      select(-n) %>% 
      rename(mayor = prop), 
    by = "age"
  ) %>% 
  # previous governors 
  full_join(
    data.jhred.first[data.jhred.first$gov == 1, ] %>% 
      count(age) %>% 
      mutate(prop = n / sum(n)) %>% 
      select(-n) %>% 
      rename(gov = prop), 
    by = "age"
  ) %>% 
  # previous lawyers
  full_join(
    data.jhred.first[data.jhred.first$law == 1, ] %>% 
      count(age) %>% 
      mutate(prop = n / sum(n)) %>% 
      select(-n) %>% 
      rename(law = prop), 
    by = "age"
  ) %>% 
  pivot_longer(
    cols = -age
  )

data.age %>% 
  filter(name != "all") %>% 
  ggplot(aes(x = age, y = value, color = name, fill = name)) + 
  geom_histogram(stat = "identity", position = "dodge") + 
  stat_function(
    fun = dnorm, 
    args = list(
      mean = mean(data.jhred.first$age, na.rm = TRUE), 
      sd = sd(data.jhred.first$age, na.rm = TRUE)
    ), 
    color = "black", 
    linetype = "dashed"
  ) + 
  ylim(c(0, 0.075)) + 
  labs(
    x = "Age",
    y = "Proportion / density"
  ) + 
  theme_minimal()
