

args = commandArgs(trailingOnly=TRUE)

library(dplyr)

#exit <- function() {
#	  .Internal(.invokeRestart(list(NULL, NULL), NULL))
#}

trait <- args[1]

GPS <- args[2]

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


#Disease_case <- args[4]
#Disease_control <- args[5]


#Disease_case <- read.csv(Disease_case,sep="\t")
#Disease_control <- read.csv(Disease_control,sep="\t")

#names <- names(Disease_case)


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
#if(is.element("FID",names)){
#	Disease_case <- Disease_case[,c("FID","FID")]
#	names(Disease_case)[2] <- c("IID")
#	Disease_case[ICD10] = 2

#}else{
#	print("Disease case file  columns not have FID")
	#exit()
#}

#names <- names(Disease_control)
#if(is.element("FID",names)){
#	Disease_control <- Disease_control[,c("FID","FID")]
#	names(Disease_control)[2] <- c("IID")
#	Disease_control[ICD10] = 1
#}else{
#	print("Disease control file columns not have FID")
	#exit()
#}


#Disease_sample <- rbind(Disease_case,Disease_control)


#normal_high_Disease_sample <- left_join(normal_high_sample,Disease_sample,by="FID")

#names <- names(normal_high_Disease_sample)
#if(is.element("sex.x",names)){
#	names(normal_high_Disease_sample)[names == "sex.x"] <- c("sex")
#}

#names(normal_high_Disease_sample)
#a <- normal_high_Disease_sample$ICD10
#print(a)
#index <- which(names == ICD10)
#beta <- summary(glm(as.factor(normal_high_Disease_sample[[index]]) ~as.factor(normal_high_Disease_sample$nor_high) + normal_high_Disease_sample$array + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + age + sex ,data=normal_high_Disease_sample,family = binomial))$coef["as.factor(normal_high_Disease_sample$nor_high)2",1]


#Pvalue <- summary(glm(as.factor(normal_high_Disease_sample[[index]]) ~as.factor(normal_high_Disease_sample$nor_high) + normal_high_Disease_sample$array + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + age + sex ,data=normal_high_Disease_sample,family = binomial))$coef["as.factor(normal_high_Disease_sample$nor_high)2",4]
#print(exp(beta))
#df <- DataFrame(Normal = normal_pro,High = high_pro)

#write.table(df,"result.txt",sep="\t",quote=FALSE,row.names=FALSE)

#print(paste(ICD10,trait,exp(beta),Pvalue))
#	print(paste(ICD10,high_pro,trait,"high group"))
#}else{
	













