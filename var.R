ㄴ

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
