#######################.
#Regression adjustment
#######################.

.libPaths()
print(R.version.string)

Macdrive <- "/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/"
UCLdrive <-"C:/Users/rmjdshc/OneDrive - University College London/Learning/"
folder <- "Causal Inference/Treatment Effects"  
setwd(paste0(UCLdrive, folder))
#setwd(paste0(Macdrive, folder))

library(haven)
library(ipw)
library(tidyverse)
library(survey)
library(MatchIt)
library(tableone)
library(sandwich)
library(Matching)
library(glue)

#data2 <- haven::read_dta("data/cattaneo2.dta")
#Github location.
url_stem <- "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
data2 <-haven::read_dta(glue(url_stem,'Data/cattaneo2.dta'))

#regression for control.
lm.treat0 <- lm(bweight~1 + mage,data=data2[data2$mbsmoke==0,])
lm.treat0
data2$yhat0 <- (3108.28) + (11.36*data2$mage) 
mean(data2$yhat0)

#regression for treatment.
lm.treat1 <- lm(bweight~1 + mage,data=data2[data2$mbsmoke==1,])
lm.treat1
data2$yhat1 <- (3237.091) + (-3.951*data2$mage) 
mean(data2$yhat1)

data2$diff<-data2$yhat1  - data2$yhat0

#ate
summary(data2$diff)

#new tibble.
data2 %>% dplyr::summarize(
  ate = mean(diff)
)

#atet: (=ate for the observed Tx: mbsmoke=1)

data2 %>%
  dplyr::group_by(mbsmoke) %>%
  dplyr::summarize(atet = mean(diff))

##########################
#Regression adjustment
##########################

#data2 <- haven::read_dta("data/cattaneo2.dta")
#Github location.
url_stem <- "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
data2 <-haven::read_dta(glue(url_stem,'Data/cattaneo2.dta'))
#regression for control.
lm.treat0 <- lm(bweight~1 + fage + mage + mmarried + prenatal1 
                ,data=data2[data2$mbsmoke==0,])
lm.treat0
data2$yhat0 <- (3120.4836) + (-0.3222 * data2$fage) + 
  (4.9787*data2$mage) + (162.2292*data2$mmarried) + (55.6804*data2$prenatal1)
mean(data2$yhat0)

#regression for treatment.
lm.treat1 <- lm(bweight~1 + fage + mage + mmarried + prenatal1 
                ,data=data2[data2$mbsmoke==1,])
lm.treat1
data2$yhat1 <- (3267.10555) + (-0.03353 * data2$fage) + (-8.51209*data2$mage) 
+ (131.72129*data2$mmarried) 
+ (33.70559*data2$prenatal1)
mean(data2$yhat1)

data2$diff<-data2$yhat1  - data2$yhat0

#ate
summary(data2$diff)

#new tibble.
data2 %>% dplyr::summarize(
  ate = mean(diff)
)

#atet: (=ate for the observed Tx: mbsmoke=1)

data2 %>%
  dplyr::group_by(mbsmoke) %>%
  dplyr::summarize(atet = mean(diff))

print(paste("Regression adjustment finished at", format(Sys.time(), 
                                  "%Y-%m-%d %H:%M:%S")))

##################
###FINISHED
##################



