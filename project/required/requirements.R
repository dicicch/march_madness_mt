packages = c("data.table", "optparse",
             "tidyr", "plyr", "outliers",
             "glmnet", "corrplot", "e1071",
             "ggplot2", "caret", "Metrics")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

rm(packages)