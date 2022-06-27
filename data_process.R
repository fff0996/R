#list같은 경우 matrix처럼 차원이 정해져 있지 않다보니 R object(객체)자체로 저장해야한다.

setwd("D:\\R")
list1 <- list()
for(i in 1:10){
  
  a <- c(1,2,3)
  list1[[i]] <- a
}
list1
save(list1,file="list.RData")
load("list.RData")





gapmider %>% group_by(continent,country) %>% summarize(pop_avg = mean(pop))


#and는 &
pa_alt["sub9"] <- ifelse(pa_alt$Group ==1 & pa_alt$tile3 ==1,1,
ifelse(pa_alt$Group ==1 & pa_alt$tile3 ==2,2,
ifelse(pa_alt$Group ==1 & pa_alt$tile3 == 3,3,
ifelse(pa_alt$Group ==2 & pa_alt$tile3 ==1,4,
ifelse(pa_alt$Group == 2 & pa_alt$tile3 == 2,5,
ifelse(pa_alt$Group ==2 & pa_alt$tile3 == 3,6,
ifelse(pa_alt$Group ==3 & pa_alt$tile3 ==1,7,
ifelse(pa_alt$Group ==3 & pa_alt$tile3 == 2,8,
ifelse(pa_alt$Group ==3 & pa_alt$tile3 == 3,9,0)
       )
       )
       )
       )
       )
       )
       )
                         
                         )
#벡터원소를 역순으로 배열할때는 rev함수를 사용한다. 
a=c(1,2,3,4,5)
rev(a)
# 5 4 3 2 1

library(data.table)
library(dplyr)
d <- fread('METAANALYSIS_DIAGRAM_SE1.txt')
dim(d)
our_bim <- fraed("../../../GPS/match_mer_vali.bim")
our_bim <- fread("../../../GPS/match_mer_vali.bim")
dim(our_bim)
head(our_bim)
head(d)
V7 <- paste(our_bim$V1,our_bim$V4,sep=":")
head(V7)
our_bim[V7] <- V7
our_bim <- data.table(our_bim)
our_bim[V7] <- V7
V7 <- data.frame(V7)
our_bim[V7] <- V7
head(V7)
our_bim["V7"] <- V7
sibal <- cbind(our_bim,V7)
head(sibal)
library(dplyr)
names(d)[1] <- c("V7")
head(d)
mer <- left_join(sibal,d,by="V7")
head(mer)
mer2 <- na.omit(mer)
dim(mer2)
dim(mer)
history(Inf)
history(Inf)


