Citypower = read.csv("F:/Rappo/Data Sheets/Tetuan City power consumption.csv", header = T)
library(dplyr)
str(Citypower)midwest$
data("midwest",mapping = aes(x = midwest$area,y = midwest$popdensity))
str(midwest)
citypower_g = c("DateTime")
Citypower=Citypower %>% m*utate(across(.cols=all_of(citypower_g),.fns=as.factor))


Citypower_Num=Citypower %>% select(where(is.numeric))
City_summary=Citypower_Num %>% summarise(across(.cols=names(Citypower_Num),.fns=~summary(.x)))


cor(Citypower$Temperature,Citypower$Humidity) 
cor(Citypower$Wind.Speed,Citypower$Zone.2..Power.Consumption)
cor(Citypower$Temperature,Citypower$Zone.2..Power.Consumption)
cor(Citypower$general.diffuse.flows,Citypower$Zone.1.Power.Consumption)
    cor(Citypower$Zone.3..Power.Consumption,Citypower$Temperature)

  Citypower$DateTime = as.factor(Citypower$DateTime)
    str(Citypower)
    
  Citypower %>% group_by(DateTime) %>% summarise(t=n(),
                                                        mean_ab=mean(Zone.1.Power.Consumption),
                                                        mean_ab1=mean(Zone.3..Power.Consumption),
                                                        mean_ab2=mean(Zone.2..Power.Consumption),
                                                        mean_ab3=mean(Temperature),
                                                        max_ab=max(Wind.Speed,Zone.1.Power.Consumption,Zone.2..Power.Consumption,Zone.3..Power.Consumption),
                                                        var_ab=var(Humidity))
 round(100*sd(Citypower$Zone.1.Power.Consumption)/mean(Citypower$Zone.1.Power.Consumption),2)

 
 suppressWarnings(suppressMessages(library(ggplot2)))
 
 suppressWarnings(suppressMessages(library(ggthemes)))

 
 x= seq(1,length(Citypower$Humidity))


  
  


  


