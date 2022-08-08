d1 = read.csv("F:/Rappo/Data Sheets/bike sharing data/day.csv",header =T)

# CAN BE DONE TO FIND THE SAME REPETITION USED FOR NUMERIC VALUES AS WELL AS CHARACTERS
unique(d1$mnth)

# creating a new column 
d1$newvar = seq(1:nrow(d1))
# ATTEMPTING FIBONACCI SERIES
for(j in 1:nrow(d1)){
  k = 0 
  z = 1
  d1$newvar[j]= "USA"
  q = k+z
  k = z
  z = q
  if(d1$C1[j]>=1){
    d1$newvar[j]="America"
  }
  
}
x = ifelse((d1$c1[1] <=4),1,2)
  x

