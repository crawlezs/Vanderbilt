
install.packages("psychometric")
install.packages("polycor")
install.packages("ltm")

#R version 4.1.2

# Call libraries
library(psychometric)
library(polycor)
library(ltm)

## Binary responses
# Import binary data (binary.txt)
binary <- read.table("C:\\Teaching\\IRT I_2023 Spring\\Labs\\Lab 1\\binary.txt",header=TRUE)
binary[1:10,]

# Item analysis with psychometric
item.exam(binary, discrim=T)

# Item analysis with ltm
descript(binary)



## Polytomous responses

# Import polytomous data (polytomous.txt)
polytomous <- read.table("C:\\Teaching\\IRT I_2023 Spring\\Labs\\Lab 1\\polytomous.txt",header=TRUE)
polytomous[1:10,]

# Item analysis with psychometric
item.exam(polytomous, discrim=F)

# Item analysis with ltm
descript(polytomous)
