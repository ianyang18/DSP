libs = c("foreign", "plyr", "dplyr", "ggplot2", "class")
sapply(libs, require, character.only = TRUE)
setwd(dir = "~/Documents/NCCU/Course/DSP/project/")

train.data <- read.table("data/ticdata2000.txt")
test.data <- read.table("data/ticeval2000.txt")
gt <- read.table("data/tictgts2000.txt")

# Processing the command arguments
args <- commandArgs(trailingOnly=TRUE)
i <- 1
kfold <- 2
l <- list()
algo <- c()
arg <- ""
out <- ""
while (i <= length(args)) {
  if(grepl('-', args[i])) {
    arg <- tolower(sub("-", "", args[i]))
    i <- i+1
  }
  if (arg == 'fold') {
    kfold <- args[i]
    l <- 1:kfold
  } else if (arg == 'algo') {
    algo <- c(algo, args[i])
  } else if (arg == 'out') {
    out <- args[i]
    out <- gsub('^.+\\/(.+)$', '\\1', out)
    out <- paste("../result", out,sep="/")
  } else {
    stop("USAGE: RScript prject.R -fold n -out performance.csv", call.=FALSE)
  }
  i <- i+1
}

# Function: calculate the accuracy 
# Input: table, type
# Output: accuracy
accuracy <- function(t, l) {
  TN <- 0
  N <- 0
  for(i in l) {
    for(j in l) {
      if(i == j) TN <- TN + t[i, j]
      N <- N + t[i, j]
    }
  }
  return(TN/N)
}

# Functino: k-fold cross validation
# Input: k
# Output: average error value

# Function: execute an algorithm
# Input: algorithm
# Output: result


# Spliting data into k-fold by marking each data to several groups
train.data$id <- sample(1:kfold, nrow(train.data), replace=TRUE)

kfold.err <- rep.int(0, 6)

for(kp in 1:6) {
  kp.err <- rep.int(0, kfold)
  for(i in 1:kfold) {
    training <- subset(train.data, id %in% l[-i])
    validation <- subset(train.data, id %in% c(i))
    
    data_pred <- knn(train = training[, 1:85], test = validation[, 1:85], cl = training[, 86], k = kp)
    resultFrame <- data.frame(target = data_pred, predict = validation[, 86])
    t <- table(resultFrame$target, resultFrame$predict)
    a <- accuracy(t, levels(factor(validation[, 86])))
    kp.err[i] <- a
    print(kp.err)
  }
  avg.kp.err <- mean(kp.err)
  print(avg.kp.err)
  kfold.err[kp] <- avg.kp.err
}

kp <- which(kfold.err == max(kfold.err))

data_pred <- knn(train = train.data[, 1:85], test = test.data[, 1:85], cl = train.data[, 86], k = kp)
resultFrame <- data.frame(target = data_pred, predict = gt[, 1])
t <- table(resultFrame$target, resultFrame$predict)
a <- accuracy(t, levels(factor(train.data[, 86])))
a

gt_sample <- sample(0:1, nrow(gt), replace=TRUE) 
t <- table(gt_sample, resultFrame$predict)
a <- accuracy(t, levels(factor(train.data[, 86])))
a

library(ElemStatLearn)
require(class)
x <- mixture.example$x
g <- mixture.example$y
xnew <- mixture.example$xnew
mod15 <- knn(x, xnew, g, k=15, prob=TRUE)
prob <- attr(mod15, "prob")
prob <- ifelse(mod15=="1", prob, 1-prob)
px1 <- mixture.example$px1
px2 <- mixture.example$px2
prob15 <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
          "15-nearest neighbour", axes=FALSE)
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
box()
