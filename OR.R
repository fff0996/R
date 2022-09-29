args = commandArgs(trailingOnly=TRUE)

library(dplyr)
#PRS trait
trait <- args[1]
#Result of PRS
GPS <- args[2]
#Disease_FID_dictionary
Disease <- args[3]

GPS <- read.csv(GPS,sep="\t")

names <- names(GPS)
if(is.element("st_pred_inf",names)){
	GPS <- GPS[c(order(GPS$st_pred_inf)),]
	print("I have st_pred_inf")
}else if(is.element("norm_pred_inf",names)){
	GPS <- GPS[c(order(GPS$norm_pred_inf)),]
	print("I have norm_pred_inf")
}else{
	GPS["st_pred_inf"] <- scale(GPS$pred_inf)
	GPS <- GPS[c(order(GPS$st_pred_inf)),]
	print("I have pred_inf")
}
names <- names(GPS)

if(!is.element("FID",names)){
	print("We dont have FID name in GPS result file")
	#exit()
}

sample_num <- nrow(GPS)

high_sample_num = round(sample_num * 0.97)


high_sample = GPS[c(high_sample_num:sample_num),]



normal_sample_num_start = round(sample_num * 0.4)
normal_sample_num_end = round(sample_num * 0.6)


normal_sample = GPS[c(normal_sample_num_start:normal_sample_num_end),]


normal_sample["nor_high"] = 1
high_sample["nor_high"] = 2

normal_high_sample <- rbind(normal_sample,high_sample)




dic <- read.csv(Disease,sep="\t")




ICD10 <- dimnames(dic)[[1]]
for (r in 1:nrow(dic)){
	normal_high_Disease_sample <- normal_high_sample
	FID <- unlist(dic[r,])
	names(FID) <- NULL
	FID <- FID[! FID %in% -9]
	if(length(FID) != 0){
		case <- data.frame(FID,FID)
		names(case)[2] <- c("IID")
		case["disease"] <- 2
	
	#print(case)
		normal_high_Disease_sample <- left_join(normal_high_Disease_sample,case,by="FID")
		normal_high_Disease_sample$disease[is.na(normal_high_Disease_sample$disease)] <- 1
	

	#Disease_case <- normal_high_case_sample[normal_high_case_sample$disease == 2,]
	#str(Disease_case)
	#print(Disease_case)
	#Disease_control <- normal_high_case_sample[normal_high_case_sample$disease == 1,]
	#Disease_control["disease"] <- 1
	#normal_high_Disease_sample <- rbind(Disease_case,Disease_control)
		names <- names(normal_high_Disease_sample)
	#print(names)
		if(is.element("sex.x",names)){
			        names(normal_high_Disease_sample)[names == "sex.x"] <- c("sex")
		}	
		index <- which(names == 'disease')
	#print(index)
		beta <- summary(glm(as.factor(normal_high_Disease_sample[[index]]) ~as.factor(normal_high_Disease_sample$nor_high) + normal_high_Disease_sample$array + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + age + sex ,data=normal_high_Disease_sample,family = binomial))$coef["as.factor(normal_high_Disease_sample$nor_high)2",1]


		Pvalue <- summary(glm(as.factor(normal_high_Disease_sample[[index]]) ~as.factor(normal_high_Disease_sample$nor_high) + normal_high_Disease_sample$array + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + age + sex ,data=normal_high_Disease_sample,family = binomial))$coef["as.factor(normal_high_Disease_sample$nor_high)2",4]
		print(paste(ICD10[r],trait,exp(beta),Pvalue))
	}
	else{
		next
	}
}












