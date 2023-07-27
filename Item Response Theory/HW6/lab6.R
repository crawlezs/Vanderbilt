


library(lordif)


data <- read.table("C:\\Teaching\\IRT I_2023 Spring\\Labs\\Lab 6\\DIF for polytomous responses\\data_children_gender.txt", header=T)

data <- na.omit(data)

y <- subset(data, select = -group)

#group=gender

group <- data$group

DIF <- lordif(y,group)

DIF

summary(DIF)

plot(DIF,labels=c("male","female"),width=8,height=7,cex=0.8,lwd=1)







