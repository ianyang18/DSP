#Goal
Implementing the k-fold cross-validation method in R.

Here we use the knn model to train the data and test its accuracy.

Prerequisite:

-  require "class", "dplyr" library

#Usage

##Input
```
> rscript <fileName.R> -fold <int: number> -out <string: output file>
```

The argument flag's order doesn't matter.

##Example
```
> rscript hw4_104753024_Chun-Yao_Yagn.R -fold 5 -out performance.csv
```