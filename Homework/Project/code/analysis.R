libs = c("foreign", "plyr", "dplyr", "ggplot2")
sapply(libs, require, character.only = TRUE)
setwd(dir = "~/Documents/NCCU/Course/DSP/project/")
data <- read.table("data/ticdata2000.txt")
#dim(data)
#summary(data)
table(data[, 1])
prop.table(table(data[, 1]))
barplot(table(data[, 1]))
barplot(prop.table(table(data[, 1])))

table(data[, c(1,5)])
prop.table(table(data[, c(1,5)]))
barplot(table(data[, c(1,5)]))
barplot(prop.table(table(data[, c(1,5)])))

caravan.purchase = data[, 86]
a<-table(data$V1[data$V2==2])
barplot(a,border="dark blue",main="BAR PLOT FOR CUSTOMER SUBTYPE 1 ",xlab=" customer subtype 1 ",ylab=" Number of customers " )
