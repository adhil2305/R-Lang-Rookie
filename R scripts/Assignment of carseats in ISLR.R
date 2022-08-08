require(ISLR)
library(dplyr)
vehicle1 = ISLR::Carseats
str(vehicle1)
View(vehicle1)
# age_1 10 < 1
cars_data = vehicle1 %>%
  mutate(age_1 = case_when(Age<=10~"1",
                           Age>10 & Age<=45~"2",
                           Age>45 & Age<=60~"3",
                           TRUE~"4"),
         us_1 = case_when(US=="Yes"~"1",
                          US=="No"~"2"))
lowprice = vehicle1 %>% filter(Price<100,Education<15,ShelveLoc=="Bad",Income!=73)
-
  
  
  pooreducat = vehicle1 %>% filter(Education<15)
income73 = vehicle1 %>% filter(Income!=73)
badcondition = vehicle1 %>% filter(ShelveLoc=="Bad")
View(lowprice)
View(badcondition)
# age_1>=10<45
#>=45 <60 3
#4

#us us if yes 1
#no 2

#sunbset perice < 100
#education <15 
#shelve lock is bad create subsset income !=73
# create new data set in descending order of sales 

arrange3 = vehicle1 %>% arrange(desc(Sales))

# group_by
group1 = vehicle1 %>% filter(ShelveLoc == "Good") %>% group_by(US) %>% summarise(mean = mean(Income)) 
group1
View(group1)
readr::
tidyr
ggplot2




# slice(2)
# select(mpg,colnames)

# slice collects the rows
--
  # Loading datasets from librarry ...
  library(datasets)

#loading mtcars from the datasets
ca = datasets::mtcars

# shape and size of the dataset mtcars is found here
dim(ca)

# The structure and the nature of the dataset mtcars is calibrated 
str(ca)
# dplyr package contains a set of predefined functions to make our job easier
library(dplyr)
# As per the sequence request 1 to 20 rows are taken and the columns 2 to 6 are used
sequence1 = ca %>% slice(c(1:20,50:60),2:6)

# This command is used to view the data in .xls format
View(sequence1)

# The NA ie the not availabe data is checked here
nulldata = which(is.na(ca)==TRUE)
nulldata
 
ca[c(1:20,50:60),2:6]
   # Creating a new var1 using the given cases 
## The values of mpg less than 20 are marked low the values between 20to 23 are marked medium 
#and the values above twenty three miles per gallon are named as high in the new variable
ca1 = ca %>% mutate(newvar1 = case_when(mpg<=20~"low",
                                       mpg>20 & mpg<=23~"medium",
                                       TRUE~"high"))

# filtering the values less than 30 in miles per gallon 
mpgfilter = ca %>% filter(mpg<20)

View(ca)

