libs = c("class")
sapply(libs, require, character.only = TRUE)

getwd()
setwd('..')
data.path <- paste(getwd(), "data", sep="/")
train.path <- paste(data.path, "ticdata2000.txt", sep="/")
test.path <- paste(data.path, "ticeval2000.txt", sep="/")
gt.path <- paste(data.path, "tictgts2000.txt", sep="/")

train.data <- read.table(train.path)
test.data <- read.table(test.path)
gt <- read.table(gt.path)

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
    out <- paste("result", out,sep="/")
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

outData <- data.frame("test data"= a)

dir.create("result", recursive=TRUE)
write.table(outData, file=out, sep=',', row.names=F, col.names=T)
