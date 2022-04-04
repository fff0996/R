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
