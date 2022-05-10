origi <- read.csv("trait root")
origi_alp <- origi[,c("eid","X30610.0.0")]
origi_alp <- na.omit(origi_alp)
