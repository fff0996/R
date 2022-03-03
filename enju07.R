for(i in 1:10){
 assign(paste("alp_group",i,sep=""),alp[alp$pred_inf<(0.9086449*i),])}


for(i in 1:10){
print(dim(get(paste("alp_group",i,sep=""))))}


for(i in 1:10){
print(var(get(paste("alp_group",i,sep=""))$norm_alp))}


ggplot(alp,aes(x=norm_alp,color=as.factor(tile10)))+geom_histogram()
ggplot(alp,aes(x=norm_alp,color=as.factor(tile10)))+geom_density()

alp1 <- alp %>% mutate(alp_group = ifelse(pred_inf<0.9086449,1,
ifelse(pred_inf<0.9086449*2,2,
ifelse(pred_inf<0.9086449*3,3,
ifelse(pred_inf<0.9086449*4,4,
ifelse(pred_inf<0.9086449*5,5,
ifelse(pred_inf<0.9086449*6,6,
ifelse(pred_inf<0.9086449*7,7,
ifelse(pred_inf<0.9086449*8,8,
ifelse(pred_inf<0.9086449*9,9,10))))))))))




hist(alp1$alp,breaks=500)
