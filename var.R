
qnorm((rank(x,na.last="keep")-0.5)/sum(!is.na(x)))

st_pred_inf <- scale(tg$pred_inf)
lm
res <- residuals(c)
abs_res
aetd <- all_env_merge_testing_data %>% mutate(tile20 = ntile(PRS,20))

aggregate(hyper1_normal0 ~ tile20,aetd,var)



library(ggplot2)

plt %>% ggplot(aes(tile10,drug_SBP)) + geom_point() + geom_line()


plot(d2$tile10,d2$abs_res,xlab=c("Decile of GPS"),ylab=c("Mean of residauls"),ylim=c(0.4,0.8))
ggplot(bmi,aes(x= bmi,y= as.factor(tile10))) + geom_density_ridges()

tp_2_1 <- tp_2_1[c(order(tp_2_1$Ob_Pre)),]

#리스트 내 최빈 값 출력 함수 
v <- c(2,1,2,3,1,2)
getmode( v )
#2
mer <- mer %>% mutate(t2d = ifelse(is.na(T2D),0,T2D))
b <- transform(b, `Fixed-effects_p-value` = as.numeric(`Fixed-effects_p-value`))
