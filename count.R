args = commandArgs(trailingOnly=TRUE)

library(dplyr)


case <- args[1]

case <- read.csv(case,header=F)


QT <- args[2]

QT <- readLines(QT)

vali_eid <- read.csv("/BiO/Hyein/90traits/validation/root_vali_eid.txt",sep="\t")

#paste gps result
for ( i in 1:length(QT)){
	gps <- read.csv(paste("/BiO/Hyein/90traits/BT/QT_BT/2nd_validation_GWAS/44_GPS_outcome/X",i,sep=""),sep="\t")
	gps <- gps[,c(1,7)]
	names(gps)[2] <- c(paste("pred_inf_",i,sep=""))
	vali_eid <- left_join(vali_eid,gps,by="FID")
}


#paste gps tile

start <-18
end <- 18 + length(QT)-1

for ( i in 1:nrow(vali_eid)){
	sc <- scale(vali_eid[i,start])
	vali_eid <- vali_eid %>% mutate(tile100 = ntile(sc,100))
	names(vali_eid)[end+1] <- c(paste("tile100_",i,sep=""))
	start <- start + 1
	end <- end + 1
}
start <- 18 + length(QT)
end <- start + length(QT) -1 

tmp <- vali_eid[,c(1,17,start:end)]

case["case"] <- 2
names(case)[1] <- c("eid")

tmp <- left_join(tmp,case,by="eid")

tmp[is.na(case),]$case <- 1

cnt <- rep(1:length(QT))

for ( i in 1:legth(QT)){
	assign(paste("FID_",i,sep=""),vector())
	assign(paste("case_",i,sep=""),vector())
}

for ( i in 1:nrow(tmp)){
	cc <- 0
	for ( ind in 3:(3+length(QT)-1)){
		if(tmp[i,ind] > 97){
			cc <- cc + 1
		}
	}
	if(cc == 0){
		next
	}
	else{
		x <- which(cnt ==cc)
		assign(paste("FID_",cnt[x],sep=""),c(get(paste("FID_",cnt[x],sep="")),tmp[i,1]))
		assign(paste("case_",cnt[x],sep=""),c(get(paste("case_",cnt[x],sep="")),tmp[i,(3+length(QT))]))
	}
}

for ( i in 1:length(QT)){
	assign(paste("count_",i,sep=""),data.frame(FID = get(paste("FID_",i,sep="")),case = get(paste("case_",i,sep=""))))
}

for ( i in 1:length(QT)){
	if(nrow(get(paste("count_",i,sep=""))) == 0){
		next
	}
	else{
		print(i)
		pro <- get(paste("count_",i,sep="")) %>% .[.$case == 2,]
		print(nrow(pro)/nrow(get(paste("count_",i,sep=""))))
	}
}








