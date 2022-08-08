library(ISLR)
orange_1 = ISLR::OJ
library(dplyr)
str(orange_1)
d1 = which(is.na(orange_1)==TRUE)
d1
str(orange_1)
View(orange_1)

# Select required values
sl1 = orange_1 %>% select("Purchase", StoreID, PriceCH)
sl1
# Arrange in ascending or descending order
arrange1 = orange_1 %>% arrange(LoyalCH)
arrange1
# To arrange in descending order
arrange2 = orange_1 %>% arrange(desc(StoreID))

# Mutation is done for

mutt1 = orange_1 %>% mutate(newvar5 = case_when(WeekofPurchase <= 250 ~ "very low",                            
                                                WeekofPurchase <= 260 & WeekofPurchase <= 270 ~"low", 
                                                TRUE~"High"))


mutt1
rlang::last_error()
rlang::last_trace()
str(orange_1)

# Case when usage when we have to replace ifelse statement
orange_1$d12 = case_when+(orange_1$WeekofPurchase <= 240~ "very low", 
                         orange_1$weekofPurchase <= 270~ "low",
                         TRUE ~ "HIGH")
                         

# Using the count function
count(orange_1,PriceCH)

# Trying out mean funciton 
dat1 = ISLR::Smarket
str(dat1)
meanfun = `function(x) mean(x)
me = 0
#for(j in 1:ncol(dat1)) 
{
  #me[j] = ifelse(is.numeric(dat1[,j], meanfun(orange_1[,j]), " ")
} 
 #      me = as.numeric(me[which(me!=" ")])
me
str(me)

str(dat1)
View(dat1)

#dat2=dat1 %>% select_if(is.numeric) %>% 
 # summarise(across(everything(),~meanfun(.x)))

dat2 = dat1 %>% select if(is.numeric) %>% 
  summarise(across(everything(),~meanfun(.x)))
str(dat2)
dat2


dat3=dat1 %>% add_row(Year=" ",dat2,Direction=" ") 
rownames(dat3)=c(1:1250,"Avg")
dat3

