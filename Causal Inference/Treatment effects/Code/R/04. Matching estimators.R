####################################
#### Estimating treatment effects 
####################################

.libPaths()
print(R.version.string)

Macdrive<-"/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/"
UCLdrive<-"C:/Users/rmjdshc/OneDrive - University College London/Learning/"
folder<-"Causal Inference/Treatment Effects"  
setwd(paste0(UCLdrive, folder))
#setwd(paste0(Macdrive, folder))

library(PSW)
library(optmatch)
library(glue)

url_stem <- "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
data2 <-haven::read_dta(glue(url_stem,'Data/cattaneo2.dta'))
head(data2)

m.out <- matchit(mbsmoke ~ mmarried + mage + medu + fbaby, 
                   data = data2,
                   distance = "glm",
                   estimand = "ATT",
                   method = "nearest")
m.out
summary(m.out)   # 864 matched pairs
md<-match.data(m.out) # matched dataset for estimation
#View(md)
fit <- lm(bweight ~ mbsmoke, data = md, weights = weights)
fit

m.out <- matchit(mbsmoke ~ mage, data = data2,
                    distance = "glm",
                    estimand = "ATT",
                    method = "nearest")
m.out
summary(m.out)
md<-match.data(m.out)
md
fit <- lm(bweight ~ mbsmoke, data = md, weights = weights)
fit

####################################################
#same result by user-supplied propensity scores
####################################################

pscore <- fitted(glm(mbsmoke ~ mage, data = data2,family = binomial))
summary(pscore)
#match on the pscore
m.out <- matchit(mbsmoke ~ mage, data = data2, distance = pscore)
summary(m.out)
md<-match.data(m.out)
md
fit1 <- lm(bweight ~ mbsmoke, data = md, weights = weights)
print(fit1)




print(paste("Matching estimators finished at", format(Sys.time(), 
                              "%Y-%m-%d %H:%M:%S")))


############# 
#FINISHED
#############
























