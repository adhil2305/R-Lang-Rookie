library(dplyr)
library(ggplot2)
library(ggridges)
library(gpairs)


bike1 = read.csv("F:/Rappo/Data Sheets/bike sharing data/day.csv",header = T)

str(bike1)

# Changing the nature of variables
bike1$weekday = as.factor(bike1$weekday) 
bike1$season = as.factor(bike1$season) 
bike1$yr = as.factor(bike1$yr) 
bike1$mnth = as.factor(bike1$mnth) 
bike1$hr = as.factor(bike1$hr) 
bike1$weathersit = as.factor(bike1$weathersit) 
bike1$workingday = as.factor(bike1$workingday) 
bike1$holiday = as.factor(bike1$holiday) 


bike1 = bike1 %>% mutate(season=case_when(season==1~"Spring",
                                          season==2~"Summer",
                                          season==3~"Autumn",
                                          season==4~"Winter"))

bike1 = bike1 %>% mutate(weathersit = case_when(weathersit==1~"clear",
                                          weathersit==2~"Mist",
                                          weathersit==3~"Light snow",
                                          weathersit==4~"heavy rain"))



quantile(bike1$windspeed)
median(bike1$windspeed)
selection = bike1 %>% select_if(is.numeric)
c1 = cor(selection)

selection2 = bike1 %>% select_if(is.numeric)
selection2

windspeed_at_season = bike1 %>% group_by(season) %>% summarise(mean = mean(hum),
                                                               median = median(temp),
                                                               varience =var(temp))
windspeed_at_season


weathercondition = bike1 %>% group_by(weathersit) %>% summarise(mean = mean(hum),
                                                               median = median(temp),
                                                               varience =var(temp),
                                                               mode = mode(hum))
weathercondition


ftable(selection2)

macros = ggplot(bike1, aes(windspeed,fill=weathersit)) + 
  geom_histogram() + 
  xlab(NULL) + 
  ylab(NULL)
macros

library(kableExtra)

c1[upper.tri(c1,diag = FALSE)]<-" "
diag(c1)=""
kable(rbind(c1)) %>%  kable_styling()

p0=ggplot(bike1, aes(atemp)) + 
  geom_histogram() + 
  xlab(NULL) + 
  ylab(NULL)

p0


PL4=ggplot(bike1, aes(x=hum,y=atemp,color=red))+ 
  geom_point()+ 
  xlab("Math")+ 
  ylab("Awards")+ 
  facet_grid(ses~Gender+honors,
             scales = "free",space="free")+
  theme(legend.position = "bottom")+
  scale_color_manual(values=c("pink","black",
                              "brown","green"))
PL4

bla2 = ggplot(choisir1,
              aes(x = factor(""), fill = prog) ) +
  geom_bar() +
  coord_polar(theta = "y") +
  scale_x_discrete("")+
  scale_y_discrete("")+
  theme(axis.ticks=element_blank(),  
        axis.title=element_blank(),  
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.grid  = element_blank(),
        legend.position = "bottom")+
  labs(title = "Bike of the year",
       caption="Bike analysis")
       bla2
       bla1 = ggplot(choisir1,
                     aes(x = factor(""), fill = schtyp) ) +
         geom_bar() +
         coord_p <- <- <- <- <- <- <- <- <- olar(theta = "y") +
         scale_x_discrete("")+
         scale_y_discrete("")+
         theme(axis.ticks=element_blank(),  
               axis.title=element_blank(),  
               axis.text.y=element_blank(),
               axis.text.x=element_blank(),
               panel.grid  = element_blank(),
               legend.position = "bottom")+
         labs(title = "curriculum",
              caption="Programchoice")
bla1
       
library(corrplot)
       
corrplot(bike1$windspeed,bike1$temp)


res_corr=cor(bike1[,c(11:14,1)])

corrplot(res_corr)


corrplot(res_corr,method="number",
         type="lower",diag = F,
         number.digits=2,
         cl.lim=c(-1,1),
         col=colorRampPalette(c("darkblue","red","green"))(10) ,
         tl.pos="n")

seasons  = ggplot(bike1,aes(y=windspeed,fill=seasons)) + geom_boxplot()

bike1 %>% group_by(weathersit) %>% summarise(mean(hum))

shapiro.test(bike1$casual)

mean(hum)
selection3 = bike1 %>% select_if(is.numeric)
summary(selection3)

ftable(selection3)

t.test(bike1$hum,mu = 0.6272, conf.level = 0.99)

anv=aov(windspeed~weathersit,data = bike1)
summary(anv)

gi square for categorical data 

hypothesis h not is null hypothesis 
quality of intrest







<div class="tocify-extend-page" data-unique="tocify-extend-page" style="height: 0;"></div>
   