

update.packages(ask='irtoys',checkBuilt=TRUE)
packageVersion("irtoys")



## Call libraries irtoys and ltm
library(irtoys)
library(ltm)

## Import binary data (binary.txt)

binary <- read.table("C:\\Teaching\\IRT I_2023 Spring\\Labs\\Lab 2\\binary.txt",header=TRUE)

binary[1:10,]

## Fit a Rasch model or 1PL model
# Item parameter estimates
#"rasch"=T, the common value for discriminations is forced to 1

Rasch <- est(binary,model="1PL",engine="ltm",rasch=TRUE)
Rasch

Rasch <- est(binary, model="1PL", engine="ltm")
Rasch




# Person scores
Rasch.th <- eap(resp=binary,ip=Rasch$est,qu=normal.qu())
Rasch.th

write.table(Rasch.th,file="C:\\Teaching\\IRT I_2023 Spring\\Labs\\Lab 2\\Rasch_Score.txt",sep="\t")



# Wright Map with a "ltm" library
fsc <- factor.scores(rasch(binary))
plot(fsc,include.items=TRUE)



# Item response function: P_ji
irf(Rasch$est[1,]) # For Item 1
irf(Rasch$est[,]) # For all items


# Item characteristic curves
plot(irf(Rasch$est[c(1),]),label=TRUE) # Plot item response function for item 1
plot(irf(Rasch$est[c(2,3,7),]),label=TRUE) # Plot item response function for items 2,3,7
plot(irf(Rasch$est),label=TRUE) # Plot item response function for all items


# Test charactersitic curves (IRT true scores)
scp(binary,Rasch$est) #The observed sum scores are shown in red.


# Item information curve

Rasch_iif <- iif(Rasch$est)
plot(iif(Rasch$est[c(1),]),label=TRUE) # Plot item information function for item 1
plot(iif(Rasch$est),label=TRUE) # Plot item information function for all items



# Test information curve
plot(tif(Rasch$est),label=TRUE) # Plot test information function



## Fit a 2PL model

# Item parameter estimates
Two <- est(binary,model="2PL",engine="ltm")
Two

# 2PL person scores
Th <- eap(resp=binary, ip=Two$est, qu=normal.qu())

write.table(Th,file="C:\\Teaching\\IRT I_2023 Spring\\Labs\\Lab 2\\2PL_Score.txt",sep="\t")

# Item response function: P_ji
irf(Two$est[1,]) # For Item 1
irf(Two$est[,]) # For all items


# Item characteristic curves
plot(irf(Two$est[c(1),]),label=TRUE) # Plot item response function for item 1
plot(irf(Two$est),label=TRUE) # Plot item response function for all items


# Test charactersitic curves (IRT true scores)
scp(binary,Two$est) #The observed sum scores are shown in red.


# Item information curve

Two_iif <- iif(Two$est)
plot(iif(Two$est[c(1),]),label=TRUE) # Plot item information function for item 1
plot(iif(Two$est),label=TRUE) # Plot item information function for all items



# Test information curve
plot(tif(Two$est),label=TRUE) # Plot test information function




## Fit a 3PL model

# Item parameter estimates: There is a covergence problem for the 3PL.
Three <- est(binary,model="3PL",engine="ltm")
Three



##Calculating correlations

#correlation(Rasch difficulty,2PL difficulty)
Rasch.difficulty <- as.matrix(Rasch$est[,2])
Two.difficulty <- as.matrix(Two$est[,2])

corr.difficulty <- cor(Rasch.difficulty,Two.difficulty)
corr.difficulty


#correlation(Rasch difficulty SE,2PL difficulty SE)
Rasch.difficulty.SE <- as.matrix(Rasch$se[1:25,2])
Two.difficulty.SE <- as.matrix(Two$se[,2])

corr.difficulty.SE <- cor(Rasch.difficulty.SE,Two.difficulty.SE)
corr.difficulty.SE


#correlation(Rasch scores,2PL scores)
Rasch.scores <- as.matrix(Rasch.th[,1])
Two.scores <- as.matrix(Th[,1])
 
corr.scores <- cor(Rasch.scores,Two.scores)
corr.scores


#correlation(Rasch scores SE,2PL scores SE)
Rasch.scores.SE <- as.matrix(Rasch.th[,2])
Two.scores.SE <- as.matrix(Th[,2])
 
corr.scores.SE <- cor(Rasch.scores.SE,Two.scores.SE)
corr.scores.SE





