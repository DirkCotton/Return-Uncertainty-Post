library(outliers)
library(ggplot2) 

#X <- c(152.36,130.38,101.54,96.26,88.03,85.66,83.62,76.53,74.36,73.87,73.36,73.35,68.26,65.25,63.68,63.05,57.53)

X <- scatplot.df$values

grubbs.flag <- function(x) {
  outliers <- NULL
  test <- x
  grubbs.result <- grubbs.test(test)
  pv <- grubbs.result$p.value
  while(pv < 0.05) {
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test <- x[!x %in% outliers]
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
  }
  return(data.frame(X=x,Outlier=(x %in% outliers)))
} 
flagged.df <- grubbs.flag(X)

