library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)


rbc_var["pheno"] <- c("Red blood cell count")
tt %>% ggplot(aes(tile10,pheno_var)) + geom_point() + geom_line() + facet_wrap(~pheno)
