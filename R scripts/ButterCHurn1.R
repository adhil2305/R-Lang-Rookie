library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(gpairs)
churn1 = read.csv("F:/Rappo/Data Sheets/Churn.csv",header=T)

str(churn1)

which(is.na(churn1))

churn1$VMailPlan = as.factor(churn1$VMailPlan)
churn1$IntlPlan  =  as.factor(churn1$IntlPlan)
churn1$State =  as.factor(churn1$State)
churn1$AccountLength = as.factor(churn1$AccountLength)


internationalplan  = churn1 %>% group_by(IntlPlan,Churn) %>% summarise(ctt = n())

summary(churn1$VMailPlan,churn1$IntlPlan)

ggplot(churn1,aes(DayCalls)) + geom_histogram()+ xlab(NULL) +ylab(NULL) + facet_grid(IntlPlan~.) 

ggplot(churn1, aes(x = DayCalls,y= DayCharge)) + geom_point() + xlab(NULL)+ylab(NULL)+facet_grid(Churn~.) + theme_gdocs()
      

