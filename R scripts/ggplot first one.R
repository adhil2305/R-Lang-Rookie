
Subbiah Dr.
4:06 PM (2 hours ago)
to me, bhavyachandrureddy, faizasamreen77, gsaalima, Munira, sadathrakiya, sydzedahmd691, yusrasky

#These codes provide an introduction of ggplot through
#a data set in ggplot package
#Learning is to understand the basic plotting with ggplot
#--------------------------------------------------------------------------
#loading essential packages
suppressWarnings(suppressMessages(library(ggplot2)))

suppressWarnings(suppressMessages(library(ggthemes)))
#--------------------------------------------------------------------------
#Data set from the package
data("midwest", package = "ggplot2")  # load the data
str(midwest)
midwest$inmetro=as.factor(midwest$inmetro)
midwest$state=as.factor(midwest$state)
#--------------------------------------------------------------------------
# Initial ggplot
#Plot a single metric variable, for example, "area"
#We require an index set to create the plot
#So we consider {1,2,.....437} 437 is the length of the variable considered
#Equally you can use nrow(midwest)

x= seq(1,length(midwest$area))

#Layer 1 setting a 2 dimensional space for plotting
ggplot(midwest, aes(x=x,y=area))

#Layer 2 Choosing a geometry to plot
# here we use points
ggplot(midwest, aes(x=x,y=area)) +
  geom_point()

#Layer 3 aesthetics to plot
#Three options: size, shape, color

ggplot(midwest, aes(x=x,y=area)) +
  geom_point(size=2)

ggplot(midwest, aes(x=x,y=area)) +
  geom_point(size=2,shape=4)

ggplot(midwest, aes(x=x,y=area)) +
  geom_point(col="blue")

#All three in one plot
#IMPORTANT
#A plot can be saved so that it can be retrieved /
#reused / modified later

single_num_plot1=ggplot(midwest, aes(x=x,y=area)) +
  geom_point(shape=2,size=1.75,col="red3")

#TO retrieve we call the plot
single_num_plot1

#Layer 4 axes labels and plot titles
#We add new features to the existing graph
single_num_plot2=single_num_plot1 +
  labs(title="Area",
       subtitle="From midwest dataset",
       y="Area",
       x="Data Series Number",
       caption="Midwest Demographics")
single_num_plot2

#Another option for x axis with an identifier from the DF
#area in this case
single_num_plot2A=ggplot(midwest,
                         aes(x=area,y=percadultpoverty)) +
  geom_point(shape=3,size=1.75,col="darkgreen")+
  labs(title="Area Vs Population",
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

single_num_plot2+
  theme(plot.title=element_text(hjust=0.5,vjust=0.5,color="tomato",
                                size=14, face="bold.italic"),
        plot.subtitle=element_text(size=15, face="bold",hjust=0.5),  
        
        plot.caption=element_text(size=15,face="bold.italic",color="blue3"),  
        
        axis.title.x=element_text(vjust=10, hjust=0.1,size=10),  
        
        axis.title.y=element_text(size=15,angle = 270),  
        
        axis.text.x=element_text(size=10, angle = 30,vjust=.5),  
        
        axis.text.y=element_text(size=10))
#---------------------------------
#Information aspects (informational)
#Layer 6 Axis Limits modifications
single_num_plot4_y=single_num_plot3+
  coord_cartesian(ylim=c(0.01, 0.07))
single_num_plot4_y

single_num_plot4_x=single_num_plot3+
  coord_cartesian(xlim=c(0, 250))

single_num_plot4_x

single_num_plot4=single_num_plot3+
  coord_cartesian(xlim=c(0, 250),
                  ylim=c(0, 0.075))

single_num_plot4

#Layer 7 Axis Break Point
#(Tic marks and its Labels) modifications

single_num_plot5_x=single_num_plot3+
  scale_x_continuous(breaks=seq(0, 500, 50)) 

single_num_plot5_x


single_num_plot5_y=single_num_plot3+
  scale_y_continuous(breaks=seq(0, 0.1, 0.01))

single_num_plot5_y


single_num_plot5=single_num_plot3+
  scale_x_continuous(breaks=seq(0, 500, 75))+
  scale_y_continuous(breaks=seq(0, 0.1, 0.01))

single_num_plot5

single_num_plot5_a=single_num_plot3+
  scale_y_continuous(breaks=seq(0, 0.12, 0.01),
                     labels = letters[1:13])

single_num_plot5_a

single_num_plot5_b=single_num_plot3+
  scale_y_continuous(breaks=seq(0, 0.12, 0.01),
                     labels = LETTERS[1:13])

single_num_plot5_b

single_num_plot5_c=single_num_plot3+
  scale_y_continuous(breaks=seq(0, 0.12, 0.01),
                     labels = c("O","t","th","f","fi","s","se","e","n",
                                "t","el","aa","b1"))

single_num_plot5_c
#--------------------------------------------------------
# Other options for plot single metric variable

#boxplot

ggplot(midwest, aes(y=area)) +
  geom_boxplot()

ggplot(midwest, aes(y=area)) +
  geom_boxplot(color="red",fill="yellow")

#histogram
ggplot(midwest, aes(area)) +
  geom_histogram()

ggplot(midwest, aes(area)) +
  geom_histogram(color="green",fill="yellow")

#density plot (smoothed histogram)
ggplot(midwest, aes(x=area)) +
  geom_density(color="red",size=2)

#histogram with density plot
ggplot(midwest, aes(x=area))+
  geom_histogram(aes(y = ..density..),
                 col="darkred",fill="yellow",
                 position = "identity")+
  geom_density(col="tan3",size = 2)


# dotplot in ggplot
ggplot(midwest, aes(x=area)) +
  geom_dotplot(dotsize=0.5,color="red",fill="yellow")

#Area Plot
ggplot(midwest, aes(percadultpoverty))+
  geom_area(stat = "bin",color="red",fill='blue')
#--------------------------------------------------------

Got it, thanks!Thanks a lot.Thank you!
  