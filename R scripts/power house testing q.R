data("midwest", package = "ggplot2" )
str(midwest)


suppressWarnings(suppressMessages(library(ggplot2)))

suppressWarnings(suppressMessages(library(ggthemes)))

midwest$state = as.factor(midwest$state)
midwest$inmetro = as.factor(midwest$inmetro)


x= seq(1,length(midwest$area))
ggplot(midwest, aes(x = x, y=area)) + 
geom_point(size = 2, shape = 4)

single_num_plot1 =ggplot(midwest, aes(x = x, y=area)) + 
  geom_point(size = 2, shape = 1.67,col = "blue")

single_num_plot2 = single_num_plot1 + 
  labs(title = "area",
       subtitle = "From midwest",
       y ="area",
       s = "data series number"
       caption = "midwest demographics" ) +
  theme(
    plot.title = element_text(color = "red", size = 4,face = "bold.italic"),
    plot.subtitle = element_text(color = "blue", size = 4,face = "bold.italic"),
    axis.title.x = element_text(color = "green", size = 4,face = "bold.italic",hjust = 10,vjust = 1.56),
    axi.s.titel.y = element_text(color = "otange", size = 4,face = "bold.italic"),
    plot.caption = element_text(color = "cyan", size = 4,face = "bold.italic")
     )
single_num_plot2

single_num_plot3 = single_num_plot2 + 
  coord_cartesian(ylim =c(0.01,0.07))
single_num_plot3

single_num_plot3 = single_num_plot1 + 
  cooord_cartesian(ylim = c(0.01,0.07))


ggplot(midwest, aes(percadultpoverty)) + 
  geom_area(stat = "bin", col="red",fill="blue")



new1 =ggplot(midwest, aes(percadultpoverty))+
  geom_area(stat = "bin",color="red",fill='blue')
new1
mean(midwest$percadultpoverty)

new2 = ggplot(Citypower, aes(Temperature))+
  geom_area(stat = "bin", color="red",fil`l="blue")
new2
