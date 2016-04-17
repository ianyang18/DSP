library(ROCR)
library(ggplot2)

# Creating the confusion matrix
# Input: data.frame table and the target of male/female
# Output: confusion matrix

CM<-function(table, target) {
    if (target == "female") {
        ntarget = 'Male'
        target = 'Female'
    }
    else if (target == "male"){
        ntarget = 'Female'
        target = 'Male'
    }
    tru<-target
    fal<-ntarget
    pos<-target
    neg<-ntarget
    TP<-table[tru,pos]
    TN<-table[fal,neg]
    FP<-table[fal,pos]
    FN<-table[tru,neg]
    CM<-list("TP"=TP,"TN"=TN,"FP"=FP,"FN"=FN)
}

# Calculating the F1 score
# Input: confusion matrix
# Output: F1 score

F1<-function(cm) {
    P = cm$TP/(cm$TP+cm$FP)
    R = cm$TP/(cm$TP+cm$FN)
    f1<-round((2*P*R/(P+R)),digits=2)
}

# Calculating the sensitivity value
# Input: confusion matrix
# Output: sensitivity value

SS<-function(cm){
    ss<-round(cm$TP/(cm$TP+cm$FN),digits=2)
}

# Calculating the specificity value
# Input: confusion matrix
# Output: specificity value

SC<-function(cm) {
    sc<-round(cm$TN/(cm$TN+cm$FP),digits=2)
}

# Calculating the AUC
# Input: evaluation
# Outpu: area under curve(ROC) values

AUC<-function(eval) {
    auc<-attributes(performance(eval,'auc'))$y.values[[1]]
    auc<-round(auc,digits=2)
}

# Calculating the p-value
# Input: 2 by 2 table
# Output: p-value

significance<-function(table) {
    #st <- fisher.test(table,alternative='g')$p.value
    st<-fisher.test(table)$p.value
}

args <- commandArgs(trailingOnly=TRUE)
i <- 1
arg <- ""
target <- ""
queries <- list()
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
        if (query == "f1")
            queries[[query]] = F1
        else if (query == "auc")
            queries[[query]] = AUC
        else if (query == "sensitivity")
            queries[[query]] = SS
        else if (query == "specificity")
            queries[[query]] = SC
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


for (set in files) {
    names <- c()
    f1 <- c()
    ss <- c()
    sc <- c()
    auc <- c()
    sft <- c()
    sname<-gsub("../data/","",set)
    count <- 1
    outfolder <- paste(getwd(),out,sep="/")
    outfolder <- paste(outfolder,sname,sep="/")
    dir.create(outfolder,recursive=TRUE)
    out_name <- paste(outfolder,sname,sep="/")
    out_name <- paste(out_name,".csv",sep="")
    for (file in list.files(set)) {
        name<-gsub(".csv","",basename(file))
        names<-c(names,name)
        d<-read.csv(paste(set,file,sep="/"),header=T,sep=",",stringsAsFactors=FALSE)
        resultframe <- data.frame(target=d$reference,pred=d$prediction,score=d$pred.score)
        t <- table(resultframe$target, resultframe$pred)
        cm <- CM(t,target)
        eval <- prediction(resultframe$score,resultframe$target)
        if (count == 1) {
            roc_name <- paste(sname,"_ROC.png",sep="")
            roc_name <- paste(outfolder,roc_name,sep="/")
            png(roc_name)
            plot(file=roc_name,performance(eval,'tpr','fpr'),main="ROC Curve")
        }
        else {
            plot(performance(eval,'tpr','fpr'),add=TRUE)
        }
        count <- count + 1
        for (q in names(queries)) {
            if (q=="f1") {
                f1 <- c(f1,queries[[q]](cm))
            }
            else if (q=="sensitivity") {
                ss <- c(ss,queries[[q]](cm))
            }
            else if (q=="specificity") {
                sc <- c(sc, queries[[q]](cm))
            }
            else if (q=="auc") {
                auc <- c(auc, queries[[q]](eval))
            }
        }
        sf <- significance(t)
        if (sf > auc[1]) {
            sft <- c(sft,"T")
        }
        else {
            sft <- c(sft,"F")
        }
    }
    outData <- data.frame(set=names,stringsAsFactors=F)
    for (q in names(queries)) {
        if (q=="f1") {
            outData["F1"]=f1
        }
        else if (q=="auc") {
            outData["AUC"]=auc
        }
        else if (q=="sensitivity") {
            outData["sensitivity"]=ss
        }
        else if (q=="specificity") {
            outData["specificity"]=sc
        }
    }
    cc <- c()
    for (col in names(outData)) {
        if (col=="F1") {
            cc <- c(cc,"F1")
        }
        else if (col=="AUC") {
            cc <- c(cc,"AUC")
        }
        else if (col=="sensitivity") {
            cc <- c(cc,"sensitivity")
        }
        else if (col=="specificity") {
            cc <- c(cc, "specificity")
        }
    }
    dev.off()
    index <- sapply(outData[,cc],function(x) which.max(x))
    outData <- rbind(outData,c("highest",names[index],"nan"))
    write.table(outData,file=out_name,sep=',',row.names=F,quote=F)
}
