args = commandArgs(trailingOnly=TRUE)

library(dplyr)



trait <- read.csv("/BiO/Hyein/90Traits/BT/QT_BT/OR/3%/FI.txt",sep="\t",header=F)
trait <- trait$V1
trait <- as.character(trait)
vali_eid <- read.csv("/BiO/Hyein/90Traits/BT/QT_BT/HR_intersection/vali_eid.txt",sep="\t")


Dic <- args[1]
dic <- read.csv(Dic,sep="\t")



ICD10 <- dimnames(dic)[[1]]


for (t in trait){

        GPS <- read.csv(paste("/BiO/Hyein/90Traits/BT/QT_BT/2nd_validation_GWAS/44_GPS_outcome/X",sep=""))
        GPS <- GPS[,c(1,7)]
        GPS <- left_join(vali_eid,GPS,by="FID")
        GPS["st_pred_inf"] <- scale(GPS$pred_inf)


        GPS <- GPS %>% mutate(tile100 = ntile(st_pred_inf,100))
        high_sample <- GPS[GPS$tile100 > 97,]
        normal_sample <- GPS[GPS$tile100 > 40,]
        normal_sample <- normal_sample[normal_sample$tile100 < 61,]

        normal_sample["nor_high"] <- 1
        high_sample["nor_high"] <- 2



        normal_high_sample <- rbind(normal_sample,high_sample)

        for (i in ICD10){
                normal_high_Disease_sample <- normal_high_sample
                eid <- unlist(dic[i,])
                names(eid) <- NULL
                eid <- eid[! eid %in% -9]
                if(length(eid != 0)){
                        case <- data.frame(eid)
                        #names(case)[2] <- c("IID")
                        case["disease"] <- 2

        #print(case)
                        normal_high_Disease_sample <- left_join(normal_high_Disease_sample,case,by="eid")
                        normal_high_Disease_sample$disease[is.na(normal_high_Disease_sample$disease)] <- 1
        #normal_high_Disease_sample <- rbind(Disease_case,Disease_control)
                        names <- names(normal_high_Disease_sample)
        #print(names)
                        d <- which(names == 'disease')
        #print(index)
                        beta <- summary(glm(as.factor(normal_high_Disease_sample[[d]]) ~as.factor(normal_high_Disease_sample$nor_high) + normal_high_Disease_sample$array + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + age + sex ,data=normal_high_Disease_sample,family = binomial))$coef["as.factor(normal_high_Disease_sample$nor_high)2",1]
                        se <- summary(glm(as.factor(normal_high_Disease_sample[[d]]) ~as.factor(normal_high_Disease_sample$nor_high) + normal_high_Disease_sample$array + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + age + sex ,data=normal_high_Disease_sample,family = binomial))$coef["as.factor(normal_high_Disease_sample$nor_high)2",2]

                        Pvalue <- summary(glm(as.factor(normal_high_Disease_sample[[d]]) ~as.factor(normal_high_Disease_sample$nor_high) + normal_high_Disease_sample$array + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + age + sex ,data=normal_high_Disease_sample,family = binomial))$coef["as.factor(normal_high_Disease_sample$nor_high)2",4]
                        print(paste(i,t,exp(beta),se,Pvalue))
                }
                else{
                        next
                }
        }
}
