#Goal
The dataset is provided by CoIL. The data consists of 86 variables and includes product usage data and socio-demographic data derived from zip area codes. 

Here we use the knn model to train the data and predict its test data.

Prerequisite:

-  require "class" library

#Usage

##Input
```
> rscript <fileName.R> -fold <int: number> -out <string: output file>
```

The argument flag's order doesn't matter.

##Example
```
> rscript model.R -fold 2 -out performance.csv
```
