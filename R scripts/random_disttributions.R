# initialize random number generator
set.seed(12)

# Generate data uniformly within a given range
runif(100,min =0,max = 10)

# Generate randomly distributed data 
random_number = rnorm(100000,mean = 10,sd = 1)
plot(density(random_number))

# Generate data by sampling from a vector
die = c(1,2,3,4,5,6)

# Simulate rolling two dice and adding the result
two_dice = sample(die,size=10000,replace=T) 
table(two_dice)

# Sample rows from a data frame 

data = mtcars

index = sample(1:nrow(data),size=10,replace=FALSE)
data[index,]
