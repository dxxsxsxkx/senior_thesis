# packages
library(tidyverse)
library(rvest)
library(zipangu)
library(lubridate)

# parameters 
kAgeGroupStr <- c("0~4", "5~9", "10~14", "15~19", "20~24", "25~29", 
               "30~34", "35~39", "40~44", "45~49", "50~54", "55~59",
               "60~64", "65~69", "70~74", "75~79", "80~84", "85~89",
               "90~94", "95~99", "100~")
kAgeGroupSeq <- list(
  seq(0, 4, 1), seq(5, 9, 1), seq(10, 14, 1), seq(15, 19, 1), 
  seq(20, 24, 1), seq(25, 29, 1), seq(30, 34, 1), seq(35, 39, 1),
  seq(40, 44, 1), seq(45, 49, 1), seq(50, 54, 1), seq(55, 59, 1), 
  seq(60, 64, 1), seq(65, 69, 1), seq(70, 74, 1), seq(75, 79, 1), 
  seq(80, 84, 1), seq(85, 89, 1), seq(90, 94, 1), seq(95, 99, 1), 
  seq(100, 104, 1)
)

# scrape MPs' data
# resulting data includes: name, party, constituency, 
# and the N of times they get elected.
for(i in 1:10){
  url <- paste0("https://www.shugiin.go.jp/internet/itdb_annai.nsf/html/statics/syu/", i, "giin.htm")
  mp_html_table <- read_html(url) %>% 
    html_table()
  if(i == 1){
    mp_table <- mp_html_table[[3]] %>% 
      filter(row_number() >= 2) #最初の行は不要
  }else if(i != 10){
    mp_table <- rbind(mp_table, mp_html_table[[3]] %>% 
                        filter(row_number() >= 2)) #最初の行は不要
  }else{
    mp_table <- rbind(mp_table, mp_html_table[[3]] %>% 
                        filter(row_number() >= 2))#最初の行は不要
    mp_table %>% 
      rename(name = X1, name_kana = X2, partisan = X3, district = X4, number_e = X5) %>% 
      write_excel_csv("data/age/mp_table.csv")
  }
  Sys.sleep(time = 1)
}

# use data modified in Excel
mp_table_read <- read.csv("data/age/mp_table_read.csv") %>% 
  mutate(born_converted = convert_jyear(kansuji2arabic_all(born))) %>% #変換
  mutate(age = year(now()) - born_converted) #年齢

# MPs' age composition
mp_age <- mp_table_read %>% 
  count(age) %>% 
  mutate(MPs = n / sum(n))

# MPs' age composition by 5 yrs
mp_age_by_five <-  data.frame(
  row.names = kAgeGroupStr
)
mp_age_by_five$prop <- 0
for (i in 1:length(mp_age_by_five$prop)) {
  for (j in 1:length(mp_age$age)) {
    if (mp_age$age[j] %in% kAgeGroupSeq[[i]]) {
      mp_age_by_five$prop[i] <- mp_age_by_five$prop[i] + mp_age$MPs[j]
    }
  }
}


# histogram
mp_age %>% 
  ggplot() + 
  geom_histogram(aes(x = age, y = rate), stat = "identity") + 
  labs(title = "Age composition", 
       subtitle = "Japanese Lower House", 
       y = "Proportion")

# write data
write_csv(mp_age, "data/age/mp_by_age.csv")
