
# Update 'irtoys'
update.packages(ask='irtoys',checkBuilt=TRUE)
#R.4.1.2
# Check versions
packageVersion("irtoys")
packageVersion("ltm")
packageVersion("eRm")
packageVersion("mirt")

## Call libraries
library(irtoys)
library(ltm)
library(mirt)

##########################################
#########   BINARY RESPONSES   ###########
##########################################



## Import binary data (binary.txt)

binary <- read.table("C:/Teaching/IRT I_2023 Spring/Labs/Lab 4/binary.txt",header=TRUE)

binary[1:10,]

## Fit a Rasch model or 1PL model
# Item parameter estimates and SEs of item estimates
Rasch <- est(binary,model="1PL",engine="ltm",rasch=TRUE)
Rasch


## Item fit for Item 2
itemfit <- itf(resp=binary, ip=Rasch$est, stat = "lr", item=2)
itemfit




## Item fit for all items

nrep <- 25   # Num. of items

itemfit <- rep(list(NA), 25)
for (i in 1:nrep){
  
  itemfit[[i]] <- itf(resp=binary, ip=Rasch$est, stat = "lr", item=i)
}
itemfit
Statistic <- DF <- P_Value <- rep(NA, nrep)

for (i in 1:nrep){
  Statistic[i] <- itemfit[[i]][1]
  DF[i] <- itemfit[[i]][2]
  P_Value[i] <- itemfit[[i]][3]
}

output <- cbind(Statistic, DF, P_Value)
output




## Person fit: Residuals
personfit <- api(binary, Rasch$est)

write.table(personfit,file="C:/Teaching/IRT I_2023 Spring/Labs/Lab 4/personfit.txt")

personfit


## Model fit

# Fit a Rasch (1PL) model using ltm library to obtain a log-likelihood value
Rasch1 <- rasch(binary)
Rasch1


# Fit a 2PL model using ltm library to obtain a log-likelihood value
Two1 <- ltm(binary ~ z1, constr = rbind(c(1, 1, 1)))
Two1

# Fit a 3PL model using ltm library to obtain a log-likelihood value
Three1 <- tpm(binary, type = c("latent.trait", "rasch"), constraint = NULL,
max.guessing = 1, IRT.param = TRUE)


# Model comparisons
anova(Rasch1,Two1)






##########################################
#######   POLYTOMOUS RESPONSES ###########
##########################################

## Call library
library(eRm)

## Importing polytomous data (polytomous.txt)
poly <- read.table("C:/Teaching/IRT I_2023 Spring/Labs/Lab 4/polytomous.txt",header=TRUE)



## Fitting a rating scale model using 'eRm': Conditional MLE
# RSM, estimation of item and person parameters
rsm <- RSM(poly, se = TRUE, sum0 = TRUE)
rsm

p.rsm <- person.parameter(rsm)


## Matrix with expected probabilities (optional)
# Using a pmat function
expected  <- pmat(p.rsm)

# Creating a matrix
expected1 <- matrix(expected)   #314 persons (two persons who got all 0s were excluded automatically), 25 items X (3-1) categories

# Save results as a datafile, expected.txt
write.table(expected1,file="C:/Teaching/IRT I_2023 Spring/Labs/Lab 4/expected.txt",col.names= NA)


## Matrix with corresponding residuals
# Using a residuals function
residuals <- residuals(p.rsm)

# Creating a matrix
residuals1 <- matrix(residuals, nrow=314,ncol=25) #314 persons (two persons who got all 0s were excluded automatically; Please track the person ids), 25 items

# Save results as a datafile, residuals.txt
write.table(residuals1,file="C:/Teaching/IRT I_2023 Spring/Labs/Lab 4/residuals.txt",col.names= NA)



## Item fit
itemfit <- itemfit(p.rsm)
itemfit

## Person fit
personfit <- personfit(p.rsm)

pp <- print(personfit)

write.table(pp,file="C:/Teaching/IRT I_2023 Spring/Labs/Lab 4/personfit.txt")


## Model comparisons: RSM vs. PCM
# Rating scale model
rsm <- RSM(poly, se = TRUE, sum0 = TRUE)
rsm

IC(p.rsm)

# Partial credit model
pcm <- PCM(poly, se = TRUE, sum0 = TRUE)
pcm

p.pcm <- person.parameter(pcm)

IC(p.pcm)



###########################Added (use this code if you need to compare models with GPCM)

# Item fit and person fit for GPCM

## Fitting a generalized partial credit model using 'ltm': Marginal MLE

packageVersion("ltm")

## Importing polytomous data (polytomous.txt)
poly <- read.table("C:/Teaching/IRT I_2023 Spring/Labs/Lab 4/polytomous.txt",header=TRUE)


# Item parameters
gpcm <- gpcm(poly, IRT.param = TRUE)
summary(gpcm)


# Person parameters
gpcm_person <- factor.scores(gpcm)


###########################Added (use this code if you need to compare models with GRM)

# Item fit and person fit for GRM

## Fitting a graded response model using 'mirt'
# Item parameters

grm <- mirt(poly, 1, itemtype='graded', method="EM", SE=TRUE) 
coef.grm <-coef(grm, printSE=TRUE, as.data.frame=TRUE) 
coef.grm

# Person parameters
theta<-fscores(grm, full.scores.SE=TRUE) 
theta[1:10,]
write.table(theta, file="C:/Teaching/IRT I_2022 Spring/Labs/Lab 3/theta.txt", sep = "\t", row.names=F)

## Item fit
itemfit <- itemfit(grm)
itemfit

#Results of "S_X2" and "p.S_X2" are for chi-square statistic and its p-value, which are explained in the 13th course slide.

##Graphical approach
itemfit(grm, group.bins=15, empirical.plot = 1, method = 'ML') 

## Person fit
personfit <- personfit(grm)
personfit

#Result of "Zh" is residual-based statistic, which is explained in the 28th course slide.   
 
## Log-likelihood for AIC and BIC

## fitting RSM in mirt for the comparison with GRM
rsm <- mirt(poly, 1, itemtype='rsm', method="EM", SE=TRUE) 

## fitting PCM in mirt for the comparison with GRM
## If the data have more than two categories then a partial credit model is used instead 
pcm <- mirt(poly, 1, itemtype='Rasch', method="EM", SE=TRUE) 


anova(rsm,grm)
anova(pcm,grm)




