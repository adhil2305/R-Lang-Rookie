# Loading InsuranceData
library(insuranceData)

require(dplyr)

# Getting data SingaporeAuto from the package insuranceData
sing = get(data("SingaporeAuto"))
View(sing)

# Nature and Shape of the variable
dim(sing)

# Detecting NULL values in the data
nas1 = which(is.na(sing) == TRUE)
nas1
str(sing)
View(Sing)

# Extracting 1 to 20 cases and 50 to sixty cases First three 11 13 and 15 variables
filer_5 = sing %>% select(1:3,11,13,15) %>% slice(c(1:20,50:60))
View(filer_5)

# Create a variable by the name of exp_weights
w1 = sing %>% mutate(newvar1 = case_when(Exp_weights<0~"Low",
                                         TRUE~"High"),
                     ncd = case_when(NCD==0~"NIL",
                                     NCD>=1&NCD<=20~"level1",
                                     NCD==30~"level2",
                                     TRUE~"level3"))

# Create a subset that has only clm count and has only numeric variables
Claim = sing %>% filter(is.numeric(Clm_Count==0))
Claim


# FInding the count of certain variables
count1 = sing %>% group_by(SexInsured) %>% summarise(n = n())
count1 

vehtype = sing %>% group_by(VehicleType , SexInsured) %>% summarise(n = n())
View(vehtype)

vehtype2 = sing %>% group_by(SexInsured, VehicleType) %>% summarise(n = n())
View(vehtype2)


exp1 = sing %>% group_by(Exp_weights<0.75, VehicleType) %>% summarise(n = n())
View(exp1)

 # renaming variables 
renamed = sing %>% rename(Claims = Clm_Count)
renamed




