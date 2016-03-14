###################################################################################
# Author: Chun-Yao Yang (104753024) 
# E-mail: ianyang18@gmail.com (104753024@nccu.edu.tw)
# Project: hw1.R
# Usage: Rscript hw1.R -query <min/max> -files <file_name> -out <output_file_name>
###################################################################################
# get the command arguments and store it in 3 variables 
# query: store the execution information, min/max
# files: store the input files' name in list
# out: store the ouput file's name
args <- commandArgs(trailingOnly=TRUE)
i <- 1
arg <- ""
query <- ""
files <- list()
out <- ""
while (i <= length(args)) {
	if (grepl('-',args[i])) {
		arg <- tolower(sub("-","",args[i]))
		i <- i+1
	}
	if (arg == 'query') {
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

# function: get the weight/height max/min value in the file
# input: data(data-type:data.frame), query(action-value:min/max)
# output: weight/height separated min/max value
value <- function(data, query) {
	nrow <- dim(data)[[1]]
	cValue <- c(data[['weight']][[nrow]], data[['height']][[nrow]])
	uValue <- cValue
	if(query == 'max') {
		while(nrow > 1){
			nrow <- nrow - 1
			cValue <- c(data[['weight']][[nrow]], data[['height']][[nrow]])
			uValue <- ifelse((cValue > uValue), cValue, uValue)
		}
	} 
	else if (query == 'min'){
		while(nrow > 1){
			nrow <- nrow - 1
			cValue <- c(data[['weight']][[nrow]], data[['height']][[nrow]])
			uValue <- ifelse((cValue < uValue), cValue, uValue)
		}
	}
	data.frame('weight'=uValue[1],'height'=uValue[2])
}

# store the weight/height value in each file according to the query
# variable: df (data-type: data.frame)
i <- 1
df <- data.frame('weight'=numeric(0),'height'=numeric(0))
while (i <= length(files)) {
	d <- read.table(
		files[[i]],
		sep=",",
		header=T
	)
	df[i,] <- c(value(d,query)['weight'],value(d,query)['height'])
	i <- i+1
}

# function: find the min/max value in which target file is
# input: data(data-type:data.frame), query(action-value:min/max), type(data attribute)
# output: the index of the min/max data file
targetFile <- function(df,query,type) {
	index <- match(value(df,query)[[type]],df[[type]])
	index
}

# record the weight/height min/max indices over all files
index <- list('weight'=targetFile(df,query,'weight'),'height'=targetFile(df,query,'height'))
# transpose the data.frame, exchange the row and column.
df <- as.data.frame(t(round(df,digits=2)))
# adding the column which indicates the min/max file in each weight/height categories
df[,query] <- c(basename(files[[index[['weight']]]]),basename(files[[index[['height']]]]))

# labelling the name with each row
df <- cbind(Type=rownames(df),df)
# unlist the list type(file) to get the file's basename
files <- basename(unlist(files))
colnames(df) <- c('Type',files,query)

# write the result into the output file
write.table(
df,
file=out,
sep=',',
quote=FALSE,
row.names=FALSE
)
