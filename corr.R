 head(df)
 c <- df[,-c(1)]
 M <- cor(c)
 library(corrplot)
 corrplot(M,method="shade",shade.col = NA,tl.srt=45,type="upper",addCoef.col = "black",order="hclust",cl.pos="r",cl.ratio = 0.1,tl.col="black")
