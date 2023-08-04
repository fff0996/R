#!/bin/usr/R
library(data.table)
library(dplyr)

argv = commandArgs(trailingOnly =T)

#data including actual value of risk factor 
a = argv[1]
# Disease case data
b = argv[2]
# risk factor PRS
c = argv[3]

a <- read.csv(paste(a,sep=""),header=T,sep="\t")
b <- read.csv(paste(b,sep=""),header=F,sep="\t")
c <- read.csv(paste(c,sep=""),header=T)
names(b) <- c("eid")
b["Disease"] <- 2

m <- left_join(a,b,by="eid")
m[is.na(m$Disease),]$Disease <- 1

m2 <- left_join(m,c,by="FID")
m2["st_pred_inf"] <- scale(m2$pred_inf)


mod <- glm(as.factor(Disease) ~ age + sex.x + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + array + st_pred_inf,data=m2,family=binomial)


beta <- summary(mod)$coef["st_pred_inf",1]
se <- summary(mod)$coef["st_pred_inf",2]
pvalue <- summary(mod)$coef["st_pred_inf",4]
