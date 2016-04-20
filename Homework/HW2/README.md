# Usage

##Input
```
> rscript <fileName.R> -target <male/female> -query <F1/AUC/sensitivity/specificity> -files <input data's directory> -out <output folder>
```

The argument flag's order doesn't matter and the query/files could input single or multiple parameters.

##Example
```
> rscript hw2_104753024_Chun-Yao_Yagn.R -target male -query F1 AUC sensitivity specificity -files ../data/set1 -out out_folder
```

