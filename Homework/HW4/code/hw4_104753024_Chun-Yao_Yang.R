library(class)
library(dplyr)
data<-read.csv("../data/Archaeal_tfpssm.csv", header=F, sep=',', na.strings=c('NA',''))

# Processing the command arguments
args <- commandArgs(trailingOnly=TRUE)
i <- 1
k <- 2
l <- list() 
arg <- ""
out <- ""
while (i <= length(args)) {
    if(grepl('-', args[i])) {
        arg <- tolower(sub("-", "", args[i]))
        i <- i+1
    }
    if (arg == 'fold') {
        k <- args[i]
        l <- 1:k
    } else if (arg == 'out') {
        out <- args[i]
        out <- gsub('^.+\\/(.+)$', '\\1', out)
        out <- paste("../result", out,sep="/")
    } else {
        stop("USAGE: RScript hw4.R -fold n -out performance.csv", call.=FALSE)
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

# Initializing the data frame of the accuracy
accuracyDF <- data.frame("train"=c(0.0), "valid"=c(0.0), "testing"=c(0.0))

# Spliting data into k-fold by marking each data to several groups
data$id <- sample(1:k, nrow(data), replace=TRUE)

for(i in 1:k) {
    training <- subset(data, id %in% l[-i])
    testing <- subset(data, id %in% c(i))

    # Split training data into training/validation data(9/1)
    splitList <- 1:10
    x <- sample(1:10, 1)
    training$id2 <- sample(1:10, nrow(training), replace=TRUE)
    train <- subset(training, id2 %in% splitList[-x])
    valid <- subset(training, id2 %in% c(x))

    dataSet <- list("train"=train, "valid"=valid, "testing"=testing)
    row <- c()

    for(i in dataSet) {
        data_pred <- knn(train = i[, 3:5602], test = i[, 3:5602], cl = i[, 2], k = 4)
        resultFrame <- data.frame(target = data_pred, predict = i[, 2])
        t <- table(resultFrame$target, resultFrame$predict)
        a <- accuracy(t, levels(i[, 2]))
        row <- c(row, a)
    }
    accuracyDF <- rbind(accuracyDF, row)
}

# Remove the first row of the data
accuracyDF <- accuracyDF[-1,]

outData <- data.frame()
for (i in 1:length(names(accuracyDF))) {
    col_name <- colnames(accuracyDF)[i]
    col_avg <- colMeans(accuracyDF[i])
    data <- data.frame("set"=col_name ,"Accuracy"= col_avg)
    outData <- rbind(outData, data)
}

dir.create("../result", recursive=TRUE)
write.table(outData, file=out, sep=',', row.names=F, col.names=T)
