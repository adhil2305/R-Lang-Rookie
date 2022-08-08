library(dplyr)
kidney = read.csv("F:/Rappo/Data Sheets/KidneyCancerData.csv",header = T) 
library(tidyr)
str(kidney)

not2 = which(is.na(kidney))
not2

kidney$state = as.factor(kidney$state) 
kidney$county = as.factor(kidney$county)
kidney$dcV = as.factor(kidney$dcV)
kidney = kidney %>% rename(Gender = female)
kidney$dcCV = as.factor(kidney$dcCV)
kidney$aadcV = as.factor(kidney$aadcV)
kidney$popV = as.factor(kidney$popV)
kidney$good = as.factor(kidney$good)
str(kidney)
dim(kidney)

suppressWarnings(suppressMessages(library(ggplot2)))

suppressWarnings(suppressMessages(library(ggptheme)))

ggplot(kidney, aes(math)) + 
  geom_area(stat = "bin",color="red",fill='blue')

ggplot(kidney, aes(read))
geom_histogram()

single_num_plot1=ggplot(kidney, aes(x=write,y=science)) + 
  geom_point(shape=2,size=1.75,col="red3")

#TO retrieve we call the plot
single_num_plot1

#Layer 4 axes labels and plot titles
#We add new features to the existing graph
single_num_plot2=single_num_plot1 +
  labs(title="Area",
       subtitle="From midwest dataset", 
       y="read", 
       x="write", 
       caption="Midwest Demographics")
single_num_plot2




single_num_plot2A=ggplot(kidney, 
                         aes(x=read,y=math)) + 
  geom_point(shape=3,size=1.75,col="darkgreen")+
  labs(title="Read Vs Math",
       subtitle="From midwest dataset", 
       y="Adult Poverty", 
       x="Area", 
       caption="Midwest Demographics")
single_num_plot2A
#------------------------------------------------
#Layer 5 aesthetics for axes labels and plot titles 
single_num_plot3=single_num_plot2+
  theme(
    plot.title = element_text(color="red", size=14, 
                              face="bold.italic"),
    plot.subtitle = element_text(color="green4", size=14, 
                                 face="italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="brown", size=14, face="bold"),
    plot.caption =  element_text(color="steelblue", size=14, face="italic"))

single_num_plot3
#Few more options in positioning title, axis, tick marks 
#Usage of hjust and vjust for horizontal / vertical justification
#vjust, controls the vertical spacing between title (or label) and plot.
#hjust, controls the horizontal spacing. Setting it to 0.5 centers the title.
#face, sets the font face ("plain", "italic", "bold", "bold.italic")
#angle is used to change the orientation 

numplot1 =single_num_plot2+
  theme(plot.title=element_text(hjust=0.5,vjust=0.5,color="tomato",
                                size=14, face="bold.italic"),
        plot.subtitle=element_text(size=15, face="bold",hjust=0.5),  
        
        plot.caption=element_text(size=15,face="bold.italic",color="blue3"),  
        
        axis.title.x=element_text(vjust=10, hjust=0.1,size=10),  
        
        axis.title.y=element_text(size=15,angle = 270),  
        
        axis.text.x=element_text(size=10, angle = 30,vjust=.5),  
        
        axis.text.y=element_text(size=10)) 

str(kidney)

grp1 = kidney %>% group_by(popV) %>% summarise(ctt = n(),
                                               mean =f1)
grp1
kidney = na.omit(kidney) 
which(is.na(kidney$popV))
