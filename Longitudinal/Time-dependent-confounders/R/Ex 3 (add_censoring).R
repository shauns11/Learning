################################.
#IPTW (time-varying treatment).
################################.

library(haven)
library(ipw)
library(survey)
library(dplyr)
library(tidyverse)
library(tibble)

#Ex3a.
#df <- haven::read_dta("C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/01. IPTW/data/ipw_Ex3a_R.dta")
df <- haven::read_dta("N:/Learning/Longitudinal/01. IPTW/data/ipw_Ex3a_R.dta")
df = as.data.frame(df)
head(df)

#use drop_tx as datasets for iptw cannot have missing values
#Notice how this is not survival data (no use of tstart).

#sort by id and time.
df <- df %>%
dplyr::arrange(df, id, time)

#create lagged variables.
df <- df %>%  
    group_by(id) %>%
    dplyr::mutate(lagTx = dplyr::lag(treat, n = 1, default = NA),
                                  lagx = dplyr::lag(x, n = 1, default = NA)) %>% 
as.data.frame()
head(df)

#replace missing lagged (_n=1) with baseline value.
df <- df %>% 
  group_by(id) %>% 
  dplyr::mutate(lagx = ifelse(time==0,x_0,lagx))
head(df)

#datasets for whom Tx weight is created.
#In the model OR carried forward (assumed to stay on TX).
dfTx <- df %>%
  filter(treat==0|treat==1|drop_tx==1) %>% 
  as.data.frame()
nrow(dfTx)
dfTx = as.data.frame(dfTx)

#datasets for censoring (no censoring at baseline).
dfCx <- df %>%
filter((time==1|time==2)) %>% 
as.data.frame()
dfCx = as.data.frame(dfCx)

#time-variant confounders in denominator.
Tx<-ipw::ipwtm(exposure=treat,
                       family="binomial",
                       link="logit",
                       numerator=~1 + sex + x_0,
                       denominator=~1 + sex + x_0 + x,
                       id=id,
                       timevar=time,
                       type="first",
                       data=dfTx)
length(Tx$ipw.weights)


Cens<-ipw::ipwtm(exposure=cens,
                         family="binomial",
                         link="logit",
                         numerator=~1 + sex + x_0,
                         denominator=~1 + sex + x_0 + lagx,
                         id=id,
                         timevar=time,
                         type="first",
                         data=dfCx)
length(Cens$ipw.weights) 

#create new vectors.
iptw<-Tx$ipw.weights
ipcw<-Cens$ipw.weights

#add the weights to the dataset used to create them.
#keep just id, occ and the weight for merging.
dfTx2 <- dfTx %>% 
tibble::add_column(iptw) %>%                       
dplyr::select(id,time,iptw)

dfCx2 <- dfCx %>% 
tibble::add_column(ipcw) %>%                       
dplyr::select(id,time,ipcw)

#merge the datasets.
#need censoring weight to be 1 when time=0
#remove records with missing weight.
  
df2 <- dplyr::left_join(df, dfTx2, by = c("id","time")) %>%
dplyr::left_join(.,dfCx2, by = c("id","time")) 
df2$ipcw[is.na(df2$ipcw) & df2$time==0] <-1

df2 <- df2 %>%
mutate(ipw = iptw*ipcw) %>% 
filter(!is.na(ipw)) %>% 
  filter(!is.na(y))

#sort by id and occasion.
df2 <- df2 %>%
dplyr::arrange(df2, id, time)
head(df2)

msm <- (survey::svyglm(y ~ treat, design = survey::svydesign(id =~ id, weights = df2$ipw,
        data = df2)))
msm
coef(msm)
confint(msm)

###################
#Example 3b.
###################

#data <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/01. IPTW/data/ipw_Ex3b_R.dta")
data <- haven::read_dta("N:/Learning/Longitudinal/01. IPTW/data/ipw_Ex3b_R.dta")
data = as.data.frame(data)

#data in long form.
summary(data$outcome)   

#sort by id and occasion.
data <- data %>%dplyr::arrange(data, id, time)

#lagged values.
data <- data %>%  
group_by(id) %>%
dplyr::mutate(lagTx = dplyr::lag(treat, n = 1, default = NA),
lagx = dplyr::lag(x, n = 1, default = NA)) %>% 
as.data.frame()

#datasets for whom weights are created.
#in the model OR carried forward (stay on TX).
dataTx <- data %>% filter(treat==0|treat==1|dropTx==1)

dataCx <- data %>%filter((time==2|time==3) & drop_c==0)

#iptw.
Tx<-ipw::ipwtm(exposure=treat,
             family="binomial",
             link="logit",
                     numerator=~1,
                       denominator=~1 + x,
                       id=id,
                       timevar=time,
                       type="first",
                       data=dataTx)
length(Tx$ipw.weights)

#ipcw.
Cens<-ipw::ipwtm(exposure=dropout,
               family="binomial",
               link="logit",
                           numerator=~1,
                          denominator=~1 + lagx,
                           id=id,
                           timevar=time,
                           type="first",
                           data=dataCx)
length(Cens$ipw.weights)

#create new vectors.
iptw<-Tx$ipw.weights
ipcw<-Cens$ipw.weights


#add the weights to the dataset used to create them.
#keep just id, occ and the weight for merging.
dataTx <- dataTx %>% 
    tibble::add_column(iptw) %>%                       
    dplyr::select(id,time,iptw)

dataCx <- dataCx %>% 
tibble::add_column(ipcw) %>%                       
dplyr::select(id,time,ipcw)

#merge the datasets.
#need censoring weight to be 1 when time=0
#remove records with missing weight.
data <- dplyr::left_join(data, dataTx, by = c("id","time")) %>%
dplyr::left_join(.,dataCx, by = c("id","time")) 
data$ipcw[is.na(data$ipcw) & data$time==1] <-1

data <- data %>%
mutate(ipw = iptw*ipcw) %>% 
filter(!is.na(ipw)) %>% 
  filter(!is.na(outcome))

#sort by id and occasion.
data <- data %>%
dplyr::arrange(data, id, time)

msm <- (survey::svyglm(outcome ~ treat, 
               design = survey::svydesign(id =~ id, 
          weights = data$ipw,data = data)))
msm
coef(msm)
confint(msm)

#################
#FINISHED
#################
  
  
  
  
  
  
  
