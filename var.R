aetd <- all_env_merge_testing_data %>% mutate(tile20 = ntile(PRS,20))

aggregate(hyper1_normal0 ~ tile20,aetd,var)



library(ggplot2)

plt %>% ggplot(aes(tile10,drug_SBP)) + geom_point() + geom_line()
