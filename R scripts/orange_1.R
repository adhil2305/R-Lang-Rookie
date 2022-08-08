library(tidyverse)
tidyverse::
library(ISLR)
require(tidyverse)
library(dplyr)
across_1 = dplyr::across
across_1
tidyverse::
dat1 = ISLR::Smarket
meanfun = function(x) mean(x)
meanfun
me = 0
for (j in 1:ncol(dat1)) {
  me[j] = ifelse(is.numeric(dat1[,j]),meanfun(dat1[,j])," ")
}
me = as.numeric(me[which(me!=" ")])
me

dat4 = dat1 %>% 
  mutate(new1 = case_when(Today>0~Today+lag(Today),
                          Today<=0~Today+lead(Today,n=2)))
View(dat4)
kable(head(dat4 %>% select(Today,new1))) %>% kable_styling()


dat8 = dat1 %>% mutate(new1 = lag(Today))
View(dat8)
str(dat8)


dat2 = dat1 %>% select_if(is.numeric) %>% 
  summarise(across(everything(),~meanfun(.x)))
 
str(dat2)
View(dat2)




v1 = data.frame(c(1,2,3,4),
                c(2,1,2,3),
                c(1,5,6,7),
                c(5,6,1,6))
View(v1)
v2 = v1 %>% select_if(is.numeric) %>% 
  summarise(across(everything(),~meanfun(.x)))
View(v2)
View(orange_1)

str(orange_1)
count(orange_1)
unique(orange_1$StoreID)
d1 = orange_1 %>% mutate(new2 = case_when (StoreID = 5 ~ "STOREfive",
                                                     StoreID = 6 ~ "STOREsix",
                                                     StoreID = 4 ~ "STOREfour",
                                                     StoreID = 1 ~ "STOREone",
                                                     StoreID = 2 ~ "STOREtwo",
                                                     StoreID = 3 ~ "STOREthree",
                                                     TRUE ~"ha ha"))
#flight = flight %>% mutate(speed = distance\time x 56)

l1
str(orange_1)
