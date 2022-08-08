library(dplyr)
require(ggplot2)
library(ggthemes)
require(GGally)
require(gpairs)

d1 = read.csv("F:/Rappo/Data Sheets/na-nov2021-hce-csv.csv")

str(d1)

d1$Series_reference = as.factor(d1$Series_reference)
d1$Subject = as.factor(d1$Subject)
d1$Series_title_1 = as.factor(d1$Series_title_1)
d1$Series_title_2 = as.factor(d1$Series_title_2)
d1$Series_title_3 = as.factor(d1$Series_title_3)
d1$MAGNTUDE  = as.factor(d1$MAGNTUDE)
d1$STATUS = as.factor(d1$STATUS)
d1$UNITS = as.factor(d1$UNITS)

ggplot(d1, aes(x = Data_value)) + 
  geom_histogram(aes(y = ..density.., 
                     fill=Series_title_1), 
                 position = "identity")+
  geom_density(aes(color = Series_title_1),size = 1) +
  scale_color_manual(values = c("red", "darkblue"))+
  scale_fill_discrete(guide=FALSE)+
  theme(legend.position="bottom")+
  labs(x="Weight",y="")