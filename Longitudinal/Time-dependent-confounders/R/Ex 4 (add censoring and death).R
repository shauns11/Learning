library(haven)
library(ipw)
library(survey)
library(dplyr)
library(tidyverse)
library(tibble)

#Ex4.
rm()
#data <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/01. IPTW/data/ipw_Ex4_R.dta")
data <- read_dta("N:/Learning/Longitudinal/01. IPTW/data/ipw_Ex4_R.dta")
data = as.data.frame(data)
#missing values are 99. 

dataTx <- data %>%
    filter(treat==0|treat==1|drop_tx==1)
dataCx <- data %>%
    filter(cens==0|cens==1|drop_c==1)
datad <- data %>%
    filter(death==0|death==1|drop_d==1)

#ipwtm.
  Tx<-ipw::ipwtm(exposure=treat,
             family="binomial",
             link="logit",
                         numerator=~sex,
                           denominator=~sex + cov,
                           id=id,
                           tstart=start,
                           timevar=occ,
                           type="first",
                           data=dataTx)
length(Tx$ipw.weights)  

Cens<-ipw::ipwtm(exposure=cens,
             family="binomial",
             link="logit",
                           numerator=~sex,
                           denominator=~sex + cov,
                           id=id,
                           tstart=start,
                           timevar=occ,
                           type="first",
                           data=dataCx)
length(Cens$ipw.weights) 

Death<-ipw::ipwtm(exposure=death,
              family="binomial",
              link="logit",
                            numerator=~sex,
                            denominator=~sex + cov,
                            id=id,
                            tstart=start,
                            timevar=occ,
                            type="first",
                            data=datad)
length(Death$ipw.weights) 

#create new vectors.
iptw<-Tx$ipw.weights
ipcw<-Cens$ipw.weights
ipdw<-Death$ipw.weights

#add the weights to the dataset used to create them.
#keep just id, occ and the weight for merging.
dataTx2 <- dataTx %>% 
tibble::add_column(iptw) %>%                       
dplyr::select(id,occ,iptw)

dataCx2 <- dataCx %>% 
    tibble::add_column(ipcw) %>%                       
    dplyr::select(id,occ,ipcw)

datad2 <- datad %>% 
   tibble::add_column(ipdw) %>%                       
    dplyr::select(id,occ,ipdw)

#merge the datasets.
#multiply the 3 weights.
#remove records with missing weight.
data <- dplyr::left_join(data, dataTx2, by = c("id","occ")) %>%
   dplyr::left_join(.,dataCx2, by = c("id","occ")) %>%
    dplyr::left_join(.,datad2, by = c("id","occ")) %>% 
  mutate(ipw = iptw*ipcw*ipdw) %>% 
    filter(!is.na(ipw))

msm <- (survey::svyglm(y ~ treat + sex, 
                               design = survey::svydesign(id =~ id, weights = data$ipw,data = data)))
msm
coef(msm)
confint(msm)

##########.
#FINISHED.
##########.







