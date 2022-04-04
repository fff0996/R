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
