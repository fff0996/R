library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)


rbc_var["pheno"] <- c("Red blood cell count")
tt %>% ggplot(aes(tile10,pheno_var)) + geom_point() + geom_line() + facet_wrap(~pheno)+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10))
labs(x="Average of Max and Min Price", y="Maximum Horsepower")


