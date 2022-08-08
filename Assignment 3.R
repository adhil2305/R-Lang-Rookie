require(dplyr)
#Load the data Churn
# Q2 find out the nature of the variables
c1 = read.csv("F:/Rappo/Data Sheets/Churn.csv")
c1
str(c1)
# FInding the mean median and mode
mean(c1)
median(c1)
mode(c1)
deviance(c1)
# Q2 Check for NA
na = which(is.na(c1))
# Q3 Change the nature of variables wherever necessary
 c1 = as.factor(c1$VMailPlan)
 c1 =as.factor(c1$State)
 c1 =as.factor(c1$IntlPlan)
 str(c1)
 c1 = c1 %>% mutate(IntlPlan = as.factor(IntlPlan))
 c1 = c1 %>% mutate(State = as.factor(State))
 c1 = c1 %>% mutate(VMailPlan = as.factor(VMailPlan))
 
str(c1)

# Q4 Renaming
c1 = c1 %>% rename(Internationalcalls = IntlCalls)
c1
table(c1)
ftable(daymins, Daymins, DayCharge)

#Q5 Creating newvariable

churn = c1 %>% mutate(daymin1 = case_when(DayMins<=100~"low",
                                          DayMins>100&DayMins<200~"medium",
                                          DayMins>=200~"high",
                                          TRUE~"VERY HI`GH"), 
                      daycall1 = case_when(DayCalls<=50~"Good caller",
                                           DayCalls>50&DayCalls<100~"super caller",
                                           TRUE~"superduper caller"),
                      cha1 = case_when(DayCharge<=20~"good",
                                       DayCharge>20~"Okay",
                                       DayCharge>50~"Better reduce calling",
                                       TRUE~"Call less"))
str(churn)
# Q6 Prepare metadata

# Present univariate variable summaries and record findings...
ftable(DayCalls,exclude = 1:2)
table(c1)

# Question 7
grouping1 = c1 %>% group_by() 
quantile(c1$DayCalls,0.5)
naruto = ftable(c1[c(DayMins, DayCalls, DayCharge)])
naruto
str (c1)
ftable(c1, row.vars = 1:2, col.vars = "DayCalls")


library(dplyr)
d1 = ISLR::Credit

cor(d1$Age, d1$Income)

gp1 = d1 %>% select(Age,Income,Balance)
m1 = cor(gp1)
upper.tri(m1)
lower.tri(m1)
diag(m1) = ""
str(m1)
d1$ID = as.character(d1$ID)
gp2 = d1 %>% select_if(is.numeric)
gp2
round(cor(gp2),3)
View(d1)

ftable(d1$Married)
ftable(d1$Married,d1$Ethnicity)
require(dplyr)
d13 = d1 %>% group_by(Ethnicity, Cards) %>% summarise(ctt = n() ) %>% mutate(PERC=round(100*ctt/nrow(d1),2))
d13
+