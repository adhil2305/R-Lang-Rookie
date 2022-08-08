require(datasets)
car12 = datasets::mtcars
car12
d1 = which(is.na(car12)==TRUE)
d1
dim(car12)
car12[c(1:20,25:30),c(2,4:6)]

car12$newvar = car12$mpg

for(i in 1:nrow(car12)){
  if(car12$mpg[i]<20){
    car12$newvar[i] = "low"
  }else{
    car12$newvar[i] = "high"
  }
}
View(car12)

car12$new = ifelse(car12$mpg)

ifelse(car12$mpg[j]<=20,"high","low")


library(dplyr)
# Ctrl+shift+m
%>% 
  # filter
f1 = car12 %>% filter(mpg<20,vs==1)  
f1
car12$new2 = car12 %>%  casewhen(car12<=15~"very low",
                                 car12)