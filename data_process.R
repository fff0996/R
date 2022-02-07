gapmider %>% group_by(continent,country) %>% summarize(pop_avg = mean(pop))
