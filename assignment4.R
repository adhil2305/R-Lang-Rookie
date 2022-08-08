# absentism
absents = read.csv("F:/Rappo/Data Sheets/Absenteeism_at_work.csv")
require(dplyr)

#Question 3 size and shape and find NA
f1 = which(is.na(absents))
f1
absents$ID = is.factor(ID)

#Transform the na   ture of the variable 
str(absents)
absents = absents %>%  rename(Sons = Son )
View(absents)


grp = absents %>% group_by(Absenteeism.time.in.hours, 
                           Distance.from.Residence.to.Work) %>%  summarise(mean = mean(Absenteeism.time.in.hours),
                           ctt = n(),
                           median = median()
                           )

r1 = cor(absents$Absenteeism.time.in.hours,absents$Work.load.Average.day, absents$Distance.from.Residence.to.Work)
  str(absents)

# Summarise univariate summaries


grp2 = absents %>% group_by(Absenteeism.time.in.hours, Social.drinker,Distance.from.Residence.to.Work) %>% summarise(mean = mean(Absenteeism.time.in.hours),ctt = n())
grp2
grp1

c1 = absents %>% select(Disciplinary.failure)
absents$Seasons = as.factor(absents$Seasons)
absents$Social.smoker = as.factor(absents$Social.smoker)
absents$ID = as.factor(absents$ID)
absents$Social.drinker = as.factor(absents$Social.drinker)
absents$Month.of.absence = as.factor(absents$Month.of.absence)
absents$Day.of.the.week = as.factor(absents$Day.of.the.week)

str(absents)

absents$Disciplinary.failure = as.factor(absents$Disciplinary.failure)
str(absents)
absents$Social.smoker = as.factor(absents$Social.smoker)
absents1 = absents %>% group_by(Seasons,Disciplinary.failure) %>% summarise(ctt = n(),mean = mean(Absenteeism.time.in.hours)) %>% 
  mutate(PERC =round(100*ctt/nrow(absents),2))
View(absents1)+
absents1

gp1 = absents %>% select(Absenteeism.time.in.hours, Disciplinary.failure,Social.smoker)

cor(gp1)

# Grouping data
gp2 = absents %>% select_if(is.numeric)
cor(gp2)

# Using ftable
ftable(absents$Social.drinker,absents$Age)

absents = absents %>% mutate(newvar = case_when(Work.load.Average.day<=240~"low",
                                                Work.load.Average.day>240&Work.load.Average.day<260~"Medium",
                                                Work.load.Average.day>=260&Work.load.Average.day<290~"Heavy",
                                                TRUE~"Rediculous"))
ftable(absents$newvar,absents$Absenteeism.time.in.hours)
cor(absents$Work.load.Average.day, absents$Absenteeism.time.in.hourstee)

gpr1 = absents %>% select_if(is.integer) 
gpr1 = absents %>% select_if(is.numeric)
View(gpr1)
View(round(cor(gpr1),2))
absents$Reason.for.absence = as.factor(absents$Reason.for.absence)

univar = absents %>% group_by(Social.drinker, Social.smoker  ) %>% summarise(ctt = n(), mean = mean(Absenteeism.time.in.hours))
View(univar)
str(absents)

d13=absents %>% group_by(Seasons,Absenteeism.time.in.hours) %>% summarise(CNT=n()) %>% mutate(PERC=round(100*CNT/nrow(absents),2))
View(d13)



d11 = absents %>% group_by(Reason.for.absence) %>% summarise(arrange(desc(mean = mean(Absenteeism.time.in.hours))), ctt = n())
View(d11)()





gpr1 = absents %>% select_if(is.integer) 
gpr1 = absents %>% select_if(is.numeric)
View(gpr1)
round(cor(gpr1),2)

univar = absents %>% group_by(Social.drinker, Social.smoker  ) %>% summarise(ctt = n(), mean = mean(Absenteeism.time.in.hours))
View(univar)
str(absents)

```

We have found out the factors that affect the the increase in the absentees
the increase in transport expenses increases the absentees by 3%
* The Distance from home to work increses absentees by 9%
* If service time increase by 2% absentees also increase by 2%
  * The more the number of children the employee has the more the employee takes a
day off
* The Person who is a smoker and a drinker has a high risk of being absent for a long time period


# Find the required Correlation for grouped and ungrouped
```{r , warnings = FALSE, message = FALSE,  comment=""}
d13=absents %>% group_by(Seasons,Absenteeism.time.in.hours) %>% summarise(CNT=n()) %>%  mutate(PERC=round(100*CNT/nrow(absents),2))

d11 = absents %>% group_by(Reason.for.absence, Absenteeism.time.in.hours) %>% summarise(mean = mean(Absenteeism.time.in.hours), ctt = n())

```
1. In the first Season 30 people are absent by 2 hours 
1. Twenty nine people are absent by 4 hours in the first season
1. The time of absenteeism is high for diseases of the circulatory system
1. The cause due to 23 and 28 are alarmingly high





absents$Seasons = as.factor(absents$Seasons)
absents$Social.smoker = as.factor(absents$Social.smoker)
absents$ID = as.factor(absents$ID)
absents$Social.drinker = as.factor(absents$Social.drinker)
absents$Reason.for.absence = as.factor(absents$Reason.for.absence)
absents$Month.of.absence = as.factor(absents$Month.of.absence)
absents$Month.of.absence = as.factor(absents$Month.of.absence)
absents$Disciplinary.failure = as.factor(absents$Disciplinary.failure)
absents$Day.of.the.week = as.factor(absents$Day.of.the.week)



summary(absents[,2])

absentdata_Num = absents %>% select_if(is.numeric)
absentdata_Num
absentdata_summary =absents %>% summarise(across(.cols=names(absentdata_Num),.fns=~summary(.x)))
View(absentdata_summary)



View(absentdata_summary)

summary(absents)


absentdata_N=c("ID","Reason.for.absence","Day.of.the.week","Seasons","Disciplinary.failure",
               "Social.drinker","Education","Social.smoker","Pet","Month.of.absence")
absentdata=absentdata %>% mutate(across(.cols=all_of(absentdata_N),.fns=as.factor))







