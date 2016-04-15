library(ROCR)
library(ggplot2)
args <- commandArgs(trailingOnly=TRUE)
i <- 1
arg <- ""
target <- ""
query <- ""
files <- list()
out <- ""
while (i <= length(args)) {
	if (grepl('-',args[i])) {
		arg <- tolower(sub("-","",args[i]))
		i <- i+1
	}
	if (arg == 'target') {
        target <- tolower(args[i])
    } else if (arg == 'query') {
		query <- tolower(args[i])
	} else if (arg == 'files') {
		file <- args[i]
		files[[file]] <- file
	} else if (arg == 'out') {
		out <- args[i]
	} else {
		stop("USAGE: Rscript hw1.R -query min/max -files file1 file2 -out out.csv", call.=FALSE)
	}
	i <- i+1
}

CM<-function(table,target) {
    if (target=="female") {
        ntarget = 'Male'
        target = 'Female'
    }
    else if (target=="male"){
        ntarget = 'Female'
        target = 'Male'
    }
    tru <- target
    fal <- ntarget
    pos <- target
    neg <- ntarget
    TP <- table[tru,pos]
    TN <- table[fal,neg]
    FP <- table[fal,pos]
    FN <- table[tru,neg]
    CM <- list("TP"=TP,"TN"=TN,"FP"=FP,"FN"=FN)
}

F1<-function(tb,target) {
    cm <- CM(tb,target)
    P = cm$TP/(cm$TP+cm$FP)
    R = cm$TP/(cm$TP+cm$FN)
    f1 <- (2*P*R/(P+R))
}

SS <- function(tb,target){
    cm <- CM(tb,target)
    ss <- cm$TP/(cm$TP+cm$FN)
}

SC <- function(tb,target) {
    cm <- CM(tb,target)
    sc <- cm$TN/(cm$TN+cm$FP)
}

names<-c()
f1<-c()
ss<-c()
sc<-c()
for (set in files) {
    for (file in list.files(set)) {
        name<-gsub(".csv","",basename(file))
        d<-read.csv(paste(set,file,sep="/"),header=T,sep=",",stringsAsFactors=FALSE)
        resultframe <- data.frame(target=d$reference,pred=d$prediction,score=d$pred.score)
        t <- table(resultframe$target, resultframe$pred)
        #print(as.vector(resultframe$score))
        eval <- prediction(resultframe$score,resultframe$target)
        plot(performance(eval,'tpr','fpr'))
        f1 <- c(f1,F1(t,target))
        ss <- c(ss,SS(resultframe,target))
        sc <- c(sc,SC(resultframe,target))
        break
    }
}
outData<-data.frame()

