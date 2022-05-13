library(bigstatsr)
library(bigsnpr)
library(dplyr)
library(data.table)
library(magrittr)
library(R.utils)

#만명 rds가 만들어지지 않았다면
snp_readBed("만명.bed")


obj.bigSNP <- snp_attach("만명.rds")
G   <- obj.bigSNP$genotypes
CHR <- obj.bigSNP$map$chromosome
POS <- obj.bigSNP$map$physical.pos
G2 <- snp_fastImputeSimple(G)

sumstats <- read.csv("summary data.txt",sep="\t")
sumstats2 <- sumstats[,c("column chr","column rsid","column pos","column tested allele","column other allele","column beta","column beta_se","column p","column n_eff")]
names(sumstats2) <- c("chr","rsid","pos","a0","a1","beta","beta_se","p","n_eff")


map <- obj.bigSNP$map[-(2:3)]
names(map) <- c("chr", "pos", "a0", "a1")
info_snp <- snp_match(sumstats2, map)

NCORES <- nb_cores()
tmp <- tempfile(tmpdir = "tmp-data가 있는 경로/tmp-data")
setwd('절대 경로 설정(tmp-data 디렉토리가 있는 경로 )')
on.exit(file.remove(paste0(tmp, ".sbk")), add = TRUE)
corr <- NULL
ld <- NULL
fam.order <- NULL
CHR <- map$chr
chr <- map$chr
POS <- map$pos
POS2 <- snp_asGeneticPos(CHR, POS, dir = ".")
corr0 <- NULL

for (chr in 1:22) {
    # Extract SNPs that are included in the chromosome
    ind.chr <- which(info_snp$chr == chr)
    ind.chr2 <- info_snp$`_NUM_ID_`[ind.chr]
    # Calculate the LD
    corr0 <- snp_cor(
            G2,
            ind.col = ind.chr2,
            ncores = NCORES,
            infos.pos = POS2[ind.chr2],
            size = 3 / 1000
        )
    if (chr == 1) {
        ld <- Matrix::colSums(corr0^2)
        corr <- as_SFBM(corr0, tmp)
    } else {
        ld <- c(ld, Matrix::colSums(corr0^2))
        corr$add_columns(corr0, nrow(corr))
    }
}


obj.bigSNP <- snp_attach("validation.rds")
G   <- obj.bigSNP$genotypes
CHR <- obj.bigSNP$map$chromosome
POS <- obj.bigSNP$map$physical.pos
G2 <- snp_fastImputeSimple(G)

fam.order <- as.data.table(obj.bigSNP$fam)
setnames(fam.order, c("family.ID", "sample.ID"), c("FID", "IID"))
df_beta <- info_snp[,c("beta", "beta_se", "n_eff", "_NUM_ID_")]
ldsc <- snp_ldsc( ld, length(ld), chi2 = (df_beta$beta / df_beta$beta_se)^2, sample_size = df_beta$n_eff, blocks = NULL)
h2_est <- ldsc[["h2"]]

ind.test <- 1:nrow(G2)
beta_inf <- snp_ldpred2_inf(corr, df_beta, h2 = h2_est)
pred_inf <- big_prodVec( G2, beta_inf, ind.row = ind.test, ind.col = info_snp$`_NUM_ID_`)


#validation set sample에 gps값 붙여주기 
names(fam.order)[1] <- c("FID")
vali <- read.csv("validation set.csv")
fam.order <- cbind(fam.order,pred_inf)
vali_fam <- left_join(vali,fam.order,by="FID")

vali_fam <- vali_fam[,c(필요한 컬럼 선택)]

write.table(vali_fam,"경로",sep="\t",quote=FALSE,row.names=FALSE)


#cortest
cor.test(vali_fam$pred_inf,vali_fam$표현형)

#lm분석
summary(lm(표현형 ~ age + sex + bmi + array + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pred_inf, data = vali_fam))
 



#auc분석 
all_env_merge_testing_data <- vali_gps_disease

sample_num = sample(1:nrow(all_env_merge_testing_data), size = round(0.2 * nrow(all_env_merge_testing_data)))

all_env_merge_testing_data_2 = all_env_merge_testing_data[ sample_num, ]
all_env_merge_testing_data_8 = all_env_merge_testing_data[ -sample_num, ]
    
model <- glm ( as.factor(all_env_merge_testing_data_8$disease) ~ age + sex + bmi +  gps  + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 +pc7+ pc8 +pc9 + pc10 + array, data = all_env_merge_testing_data_8 ,family = binomial)



p <- predict(model, newdata=all_env_merge_testing_data_2,type="response")
pr <- prediction(p,all_env_merge_testing_data_2$real_hyper)
prf <- performance(pr,measure="tpr",x.measure="fpr")
auc <- performance(pr,measure="auc")
auc <- auc@y.values[[1]]

#false_true check 

p <- predict(model, newdata=all_env_merge_testing_data_2,type="response")
d <- table(round(p),all_env_merge_testing_data_2$disease)


p_v2 <- round(p)

false_check <- cbind(all_env_merge_testing_data_2,p_v2)

false_check_case_obesity <- false_check[false_check$disease(정답) == "2",] 

false_check_case_obesity_true <- false_check_case_obesity[false_check_case_obesity$p_v2 == "1",]

false_check_case_obesity_false <- false_check_case_obesity[false_check_case_obesity$p_v2 =="0",]   





