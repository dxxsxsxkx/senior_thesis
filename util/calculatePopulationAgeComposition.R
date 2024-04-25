# packages
library(tidyverse)

# parameters
age_group <- c("0~4", "5~9", "10~14", "15~19", "20~24", "25~29", 
               "30~34", "35~39", "40~44", "45~49", "50~54", "55~59",
               "60~64", "65~69", "70~74", "75~79", "80~84", "85~89",
               "90~94", "95~99", "100~")

# age demography of Japan
# source: 「令和2年人口動態統計 上巻 付録 第３表－１ 年次・性・年齢別人口 －総数－」
population_2020 <- read.csv("data/age/population_yearly.csv", row.names = "year", na.strings = "-") %>% 
  t() %>% 
  as.data.frame() %>% 
  slice(length(population$`0`)) #2020年のみ取り出す
  
population_2020_rate <- (population_2020 / population_2020[1,1]) %>% 
  select(-c(1:6,28:31)) %>% 
  select(-c())

population_2020_rate_longer <- population_2020_rate %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(age_id = seq(0, 100, 5))


# population data
population <- read.csv("data/age/population_202110.csv") %>% #2021年10月推計
  mutate(total = total*1000, male = male*1000, female = female*1000) %>% 
  mutate(prop = total / total[1])

# print
write_csv(population, "data/age/population_by_age.csv")



# plot
population %>% 
  filter(age != "total") %>% 
  mutate(age = as.numeric(age)) %>% 
  ggplot(aes(x = age, y = prop)) +
  geom_histogram(stat = "identity")