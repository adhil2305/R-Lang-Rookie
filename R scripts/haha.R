library(dplyr)
library(ISLR)
wage_1 = ISLR::Wage
ISLR::OJ
str(wage_1)
View(wage_1)
z1 = wage_1 %>% select_if(is.integer) %>% across(1:10,year)
single1 = wage_1 %>% mutate(maritl = case_when(maritl = "1.Never Married" ~ "single"))
m1 = which(is.na(wage_1) == TRUE)
m1

mar1 = wage_1 %>% mutate_(maritl = case_when is.factor(mariti = "1.Never Married" ~ "Single",
       
                                                                                                       mariti = "2.Married"~ "Married")
                        race = case_when(race = "1.White"~"white",
                                         race = "2.Black"~"Black",
                                         race = "3.Asian"~"Asian",
                                         race = "4.Others"~"Others"))

avg_ha = mean(wage_1$wage)
avg_ha
wal = wage_1 %>% group_by(wage)
summarise(avg_wage = mean(avg_ha,na.rm = TRUE))

