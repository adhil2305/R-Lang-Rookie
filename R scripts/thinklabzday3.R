d1 = read.csv("F:/Rappo/bike sharing data/day.csv",header =T)

# CAN BE DONE TO FIND THE SAME REPETITION USED FOR NUMERIC VALUES AS WELL AS CHARACTERS
unique(d1$mnth)

# creating a new column 
d1$newvar = seq(1:nrow(d1))

for(j in 1:22){
  d1$newvar[j]="USA"
}
