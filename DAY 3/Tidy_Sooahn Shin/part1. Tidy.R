########################
### Sooahn Shin
## SNU PolMeth Boot Camp
## [Day 3] Tidy data: data preprocess & visualization
## Part 1. Tidy

# specify the working directory
setwd("~/Google Drive/Sooahn/2018-겨울/방법론 캠프/R/")
# load packages
library(tidyverse)

########################
# 0. What is tidy data?
########################
# read data
dat <- readxl::read_excel("Gender_StatsEXCEL.xlsx", sheet = "Data")
head(dat)
# employment by sector and sex
subdat <- dat[dat$`Indicator Code` %in% c("SL.AGR.EMPL.FE.ZS",   # Employment in agriculture, female (% of female employment)
                                          "SL.AGR.EMPL.MA.ZS",   # Employment in agriculture, male (% of male employment)
                                          "SL.IND.EMPL.FE.ZS",   # Employment in industry, female (% of female employment) (modeled ILO estimate)
                                          "SL.IND.EMPL.MA.ZS",   # Employment in industry, male (% of male employment) (modeled ILO estimate)
                                          "SL.SRV.EMPL.FE.ZS",   # Employment in services, female (% of female employment) (modeled ILO estimate)
                                          "SL.SRV.EMPL.MA.ZS"),] # Employment in services, male (% of male employment) (modeled ILO estimate)
# data after 1991
subdat <- subdat[, colnames(subdat) %in% c("Country Name","Country Code","Indicator Name","Indicator Code",
                                           as.character(1991:2017))]
head(subdat) # quite messy...

# This is tidy data!
subdat %>%
  select(-`Indicator Name`) %>%
  mutate(`Indicator Code` = gsub("SL.|EMPL.|.ZS", "",`Indicator Code`)) %>%
  gather(-`Country Name`, -`Country Code`, -`Indicator Code`, key = "year", value = "percent") %>%
  separate(`Indicator Code`, c("sector", "sex"), sep = "\\.")

########################
# 1. How to make tidy data?
########################
## pipeline %>% 
cos(sin(pi))
pi %>% sin() %>% cos()

# Hmm...
subdat1 <- select(subdat, -`Indicator Name`)
subdat2 <- mutate(subdat1, `Indicator Code` = gsub("SL.|EMPL.|.ZS", "",`Indicator Code`))
subdat3 <- gather(subdat2, -`Country Name`, -`Country Code`, -`Indicator Code`, key = "year", value = "percent") 
subdat4 <- separate(subdat3, `Indicator Code`, c("sector", "sex"), sep = "\\.")

# ...!?
subdat4 <- subdat %>% select(-`Indicator Name`) %>% mutate(`Indicator Code` = gsub("SL.|EMPL.|.ZS", "",`Indicator Code`)) %>% gather(-`Country Name`, -`Country Code`, -`Indicator Code`, key = "year", value = "percent") %>% separate(`Indicator Code`, c("sector", "sex"), sep = "\\.")

## Dplyr & Tidyr functions
# data
table1
# filter: to filter by value
table1 %>% filter(year==1999)
# select: to select column
table1 %>% select(country, population)
# arrange: order data by value
table1 %>% arrange(population)
table1 %>% arrange(-population)
# mutate: make new column
table1 %>% mutate(id = 1:nrow(table1))
# recode: recode values
table1 %>% mutate(code = recode(table1$country, `Afghanistan` = "A", 
                                `Brazil` = "B",
                                `China` = "C"))
# group & summarise
table1 %>%
  group_by(country) %>%
  summarise(mean.pop = mean(population))
# gather <-> spread
table1; table2
table1 %>% 
  gather(cases, population, key = "type", value = "count") %>%
  arrange(country, year)
table2 %>%
  spread(type,count)
# left_join (https://statkclee.github.io/data-science/fig/ds-basic-join.png)
table4a; table4b # -> table1
tidy4a <- table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")
tidy4a; tidy4b
left_join(tidy4a, tidy4b)

# Back to the tidy data!
subdat
subdat1 <- subdat %>%
  select(-`Indicator Name`) 
subdat1
subdat2 <- subdat1 %>%
  mutate(`Indicator Code` = gsub("SL.|EMPL.|.ZS", "",`Indicator Code`)) 
subdat2
subdat3 <- subdat2 %>%
  gather(-`Country Name`, -`Country Code`, -`Indicator Code`, key = "year", value = "percent") 
subdat3
subdat4 <- subdat3 %>%
  separate(`Indicator Code`, c("sector", "sex"), sep = "\\.")
subdat4

rm(list=setdiff(ls(), c("dat", "subdat4")))

########################
# Exercise
########################
# life expectancy by sex
exdat <- dat[dat$`Indicator Code` %in% c("SP.DYN.LE00.FE.IN",
                                          "SP.DYN.LE00.MA.IN"),]
exdat
# let's make a tidy data!


# See https://r4ds.had.co.nz/tidy-data.html for more details.