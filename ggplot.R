library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)


rbc_var["pheno"] <- c("Red blood cell count")
tt %>% ggplot(aes(tile10,pheno_var)) + geom_point() + geom_line() + facet_wrap(~pheno)+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10))
labs(x="Average of Max and Min Price", y="Maximum Horsepower")

ggplot(d,aes(x=factor(Group),y=mres2,fill=factor(subgroup),group=factor(subgroup))) + 
geom_bar(position="dodge", stat="identity",width = 0.7,size =.9 ) +
ylab("Mean Square of errors") + xlab("Decile of GPS") + ggtitle("Acohol-BMI") + scale_fill_brewer(name= "Decile of Alcohol")


ggplot(d,aes(x=factor(Group),y=var.of.res,fill=factor(subgroup),group=factor(subgroup))) +
geom_bar(position="dodge", stat="identity",width = 0.7,size =.9 ) + ylab("Variance of Residuals") + xlab("") + 
ggtitle("Alcohol-BMI") + scale_fill_discrete(name=" ",labels =c("Non-drinker","Moderate drinker","Severe drinker"))
