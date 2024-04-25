# packages
library(tidyverse)
library(zipangu)
library(lubridate)

# candidate data
reedsmith <- read_csv("data/dataverse_files/Reedsmith_complete.csv")
age_rs <- reedsmith %>% 
  select(pid, name_jp, year, yr, age, result)
age_rs_winner <- age_rs %>% #当選者のみ(当選の形は問わない). 
  filter(result != 0)
# use data modified in Excel
mp_table_read <- read.csv("data/age/mp_table_read.csv") %>% 
  mutate(born_converted = convert_jyear(kansuji2arabic_all(born))) %>% #変換
  mutate(age = year(now()) - born_converted) #年齢

# MPs' age composition
mp_age <- mp_table_read %>% 
  count(age) %>% 
  mutate(MPs = n / sum(n))

# 2014 candidates
comp_2014cand <- age_rs %>% #2014年衆院選候補者データ
  filter(yr == 25) %>% 
  count(age) %>% 
  mutate(Candidate = n/sum(n)) %>% 
  select(-n)

# 2014 winners
comp_2014winner <- age_rs_winner %>%  #2014年衆院選当選者データ
  filter(yr == 25) %>% 
  count(age) %>% 
  mutate(Winner = n/ sum(n)) %>% 
  select(-n)

# population data
population <- read_csv("data/age/population_by_age.csv") 

# over 18
pop_o18 <- population %>% 
  select(age, total) %>% 
  slice_tail(n = 83) %>% 
  mutate(Population = prop.table(total), age = as.numeric(age))

# over 20
pop_o20 <- population %>% 
  select(age, total) %>% 
  slice_tail(n = 81) %>% 
  mutate(Population = prop.table(total), age = as.numeric(age))

# over 25
pop_o25 <- population %>% 
  select(age, total) %>% 
  slice_tail(n = 76) %>% 
  mutate(Population = prop.table(total), age = as.numeric(age))

# 
comp_2021pop <- population%>% 
  filter(age != "total") %>% 
  select(-c(male, female, total)) %>% 
  mutate(age = as.numeric(age), Population = prop)


# histogram data
comp <- left_join(comp_2021pop, comp_2014cand) %>% #全年齢の人口 - 候補者割合比較
  pivot_longer(cols = c(Population, Candidate))
comp_o20 <- left_join(pop_o20, comp_2014cand) %>% #20歳以上の人口 - 候補者割合比較
  left_join(comp_2014winner) %>% 
  pivot_longer(cols = c(Population, Candidate, Winner))
comp_o25 <- left_join(pop_o25, comp_2014cand) %>% #25歳以上の人口 - 候補者割合比較
  pivot_longer(cols = c(Population, Candidate))
comp_o25w <- left_join(pop_o25, comp_2014cand) %>% #25歳以上の人口 - 当選者割合比較
  left_join(comp_2014winner) %>% 
  pivot_longer(cols = c(Population, Candidate, Winner))
comp_o25w$value[is.na(comp_o25w$value)] <- 0
comp_mp <- left_join(pop_o25, mp_age) %>% #25歳以上の人口 - MP割合比較
  pivot_longer(cols = c(Population, MPs))
comp_mp$value[is.na(comp_mp$value)] <- 0

# histograms
comp_mp %>% 
  ggplot(aes(
    x = age, 
    y = value, 
    fill = name
  )) + 
  geom_histogram(
    stat = "identity", 
    position = "identity", 
    alpha = 0.8, 
    binwidth = 1
  ) +
  scale_fill_manual(
    values = c("grey10", "grey70"), 
    labels = c("Population", "MPs"), 
    name = ""
  ) +
  labs(
    title = "Age composition", 
    subtitle = "Japanese Lower House MPs(2021) and population(2021)", 
    x = "Age", 
    y = "Proportion"
  ) + 
  theme_bw()
ggsave(
  plot = last_plot(),
  "ageEffect/figure/comp_age/comp_mp.pdf", 
  width = 6, 
  height = 4, 
  units = "in"
)

comp %>% #全年齢
  ggplot(aes(x = age, y = value, fill = name)) + 
  geom_histogram(
    stat = "identity", 
    position = "identity", 
    alpha = 0.8
  ) + 
  scale_fill_manual(
    values = c("grey20", "grey60"), 
    labels = c("Candidate", "Population"), 
    name = ""
  ) +
  labs(title = "Age composition", 
       subtitle = "Japanese Lower House candidates(2014) and population(2021)",
       x = "Age", 
       y = "Proportion", 
       fill = "") + 
  theme(plot.title = element_text(size = 15)) + 
  theme_bw()
ggsave("ageEffect/figure/comp_age/comp_all.pdf", width = 6, height = 4)
dev.off()

comp_o20 %>% #20歳以上
  ggplot(aes(x = age, y = value, fill = name)) + 
  geom_col(position = "identity", alpha = 0.6) + 
  labs(title = "Age composition", 
       subtitle = "Candidates/Winners(2014) and population over 20 (2021)",
       x = "Age", 
       y = "Proportion", 
       fill = "") + 
  theme(plot.title = element_text(size = 15))
ggsave("ageEffect/figure/comp_age/comp_o20.pdf", width = 6, height = 4)
dev.off()

comp_o25 %>% #25歳以上
  ggplot(aes(x = age, y = value, fill = name)) + 
  geom_histogram(
    stat = "identity",
    position = "dodge"
  ) + 
  scale_fill_manual(
    values = c("grey20", "grey60"), 
    labels = c("Candidate", "Population"), 
    name = ""
  ) +
  labs(title = "Age composition", 
       subtitle = "Candidates and population over 25 (2021)",
       x = "Age", 
       y = "Proportion", 
       fill = "") + 
  theme(plot.title = element_text(size = 15)) + 
  theme_bw()
ggsave(
  plot = last_plot(),
  "ageEffect/figure/comp_age/comp_o25.pdf", 
  width = 6, 
  height = 4
)

comp_o25w %>% 
  ggplot(aes(
    x = age, 
    y = value, 
    fill = name
  )) + 
  geom_histogram(
    stat = "identity", 
    position = "identity", 
    alpha = 0.8, 
    binwidth = 1
  ) +
  scale_fill_manual(
    values = c("grey10", "grey50", "grey90"), 
    labels = c("Population", "Candidates", "Winners"), 
    name = ""
  ) +
  labs(
    title = "Age composition", 
    subtitle = "Candidates/Winners(2014) and population overs 25 (2021)",
    x = "Age", 
    y = "Proportion", 
    fill = "") + 
  theme(plot.title = element_text(size = 15)) + 
  theme_bw()
ggsave("ageEffect/figure/comp_age/comp_o25w.pdf", width = 6, height = 4)
dev.off()
