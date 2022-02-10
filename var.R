aetd <- all_env_merge_testing_data %>% mutate(tile100 = ntile(PRS,20))

aggregate(hyper1_normal0 ~ tile100,aetd,mean)
