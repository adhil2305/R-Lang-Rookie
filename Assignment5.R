library(dplyr)
choisir1 = read.csv("F:/Rappo/Data Sheets/5.ProgramChoice.csv",header = T) 

str(choisir1)

not = which(is.na(choisir1))
not

#choisir1$cid = as.factor(choisir1$cid) 
choisir1$honors = as.factor(choisir1$honors)
#choisir1$awards = as.factor(choisir1$awards)
choisir1 = choisir1 %>% rename(Gender = female)
choisir1$prog = as.factor(choisir1$prog)
choisir1$schtyp = as.factor(choisir1$schtyp)
choisir1$Gender = as.factor(choisir1$Gender)
choisir1$ses = as.factor(choisir1$ses)
str(choisir1)
dim(choisir1)

suppressWarnings(suppressMessages(library(ggplot2)))

suppressWarnings(suppressMessages(library(ggthemes)))

select = choisir1 %>% select_if(is.numeric)
select = cor(select)
select

suppressWarnings(suppressMessages(library(corrplot)))
corrplot(select,method="number",
         type="lower",
         number.digits=2,
         cl.lim=c(-1,1),
         col=colorRampPalette(c("darkblue","red","darkgreen"))(10) ,
         tl.pos="n")



ggplot(choisir1, aes(math)) + 
  geom_area(stat = "bin",color="red",fill='blue')

ggplot(choisir1, aes(read))
geom_histogram()

single_num_plot1=ggplot(choisir1, aes(x=write,y=science)) + 
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




single_num_plot2A=ggplot(choisir1, 
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


median(choisir1$math)

prog_group = choisir1 %>% group_by(is.factor(choisir1)) %>% summarise(ctt = n(),
                                                                    mean = mean(),
                                                                    median = median())
new_summary = summary(choisir1)
ftable(choisir1$ses)

prog_factr = choisir1 %>% select_if(is.factor)

prog_table = ftable(prog_factr)
View(prog_factr)

#numerical summary


PL1=ggplot(Wage, aes(x=age,y=wage,color=race))+ 
  geom_point()+ 
  xlab("Age")+ 
  ylab("Wage")+ 
  facet_grid(education~jobclass+health_ins)+
  theme(legend.position = "bottom")+
  scale_color_manual(values=c("pink","black",
                              "brown","green"))
PL2=ggplot(Wage, aes(x=age,y=wage,color=race))+ 
  geom_point()+ 
  xlab("Age")+ 
  ylab("Wage")+ 
  facet_grid(education~jobclass+health_ins,
             scales = "free")+
  theme(legend.position = "bottom")+
  scale_color_manual(values=c("pink","black",
                              "brown","green"))
#Compare
gridExtra::grid.arrange(PL1,PL2,ncol=2)
#--------------------------------------------
#When scale is free space may also be free
#box size is proportional
#Without any condition on space

library(ISLR)
ISLR::Wage

PL3=ggplot(Wage, aes(x=age,y=wage,color=race))+ 
  geom_point()+ 
  xlab("Age")+ 
  ylab("Wage")+ 
  facet_grid(education~jobclass+health_ins,
             scales = "free")+
  theme(legend.position = "bottom")+
  scale_color_manual(values=c("pink","black",
                              "brown","green"))
PL3

#Space - equal
PL4=ggplot(choisir1, aes(x=math,y=science,color=schtyp))+ 
  geom_point()+ 
  xlab("Math")+ 
  ylab("science")+ 
  facet_grid(ses~Gender+honors,
             scales = "free",space="free")+
  theme(legend.position = "bottom")+
  scale_color_manual(values=c("pink","black",
                              "brown","green"))
PL4

quantile(choisir1$math)
p0=ggplot(choisir1, aes(math)) + 
  geom_histogram() + 
  xlab(NULL) + 
  ylab(NULL)

p0 + facet_grid(~math)
p0

library(ISLR)
wage1  = ISLR::Wage

View(wage1)

# Histogram
macros = ggplot(choisir1, aes(read,fill=ses)) + 
  geom_histogram() + 
  xlab(NULL) + 
  ylab(NULL)
macros1 = ggplot(choisir1, aes(socst,fill=ses)) + 
  geom_histogram() + 
  xlab(NULL) + 
  ylab(NULL)


macros

cd = choisir1 %>% group_by(ses) %>% summarise(quantile(math),
                                              quantile(science),
                                              quantile(socst),
                                              quantile(read),
                                              quantile(write),
                                              mean  = mean(math))
cd

cd = choisir1 %>%  group_by(ses) %>% summarise(mean = (math))
cd
# scatter plot 
relation = ggplot(choisir1, aes(x=math, y=science)) + 
  geom_point(aes(shape=Gender,col=ses, size=NULL))+
  theme(legend.position="bottom")+
  coord_cartesian(ylim=c(1, 100))
relation

int_ch = choisir1 %>% select_if(ses, Gender)
cor(int_ch)
require(ISLR)
ISLR::midwest

# Geometric bar plot 
bla1 = ggplot(choisir1,
      aes(x = factor(""), fill = cid) ) +
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
  labs(title = "curriculum",
       caption="Programchoice")

bla1
bla2 = choisir1 %>% group_by(cid) %>% summarise(arrange(descctt = n()))
bla2

choisir1 %>% group_by(ses,schtyp) %>% summarise(mean = mean(math),
                                                median = median(math))
c1 = data.frame(c(3,7,5),
                c(4,6,7),
                c(1,6,3)
                )
median(1:20,by = 3)
