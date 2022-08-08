library(ISLR)
anna = ISLR::Default
str(anna)
hundred = anna[100,3]
hundred

anna$student
subset1 = anna[1:50,4]
subset

anna[1:50,4]
anna[1:10,1:4]
anna[1:20,1:4]
anna[1:25,1:4]
anna[9800:10000,1]
anna[9800:10000:3]
anna$newbal = as.numeric(anna$balance)
a2 = (anna$balance)
# Choose any one of the variable in the database and store it in a new variable
f1 = as.numeric(anna$balance)
# anna$newbal  = anna$balance[nrows(anna),ncol(anna)]
# f1 = d1$balance
# d1$newvar = d1$balance
# d1 is my database 
# balance is column name in my database

str(anna)
anna[10,3]
View(anna)
na = which(is.na(anna)==TRUE)
na
  
  anna[c(1:10,20:25),]
dim(anna)


