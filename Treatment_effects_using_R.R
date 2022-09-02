library(tidyverse)
library(ipw)
library(survey)
library(haven)
library(Matching)
library(tableone)
library(MatchIt)
library(gtools)
library(sandwich)
#library(ivpack)
library(psych)
library(stats)
library(readxl)
library(PSW)
library(mvtnorm)
library(broom)  
library(optmatch)
library(modelsummary) 
library(dplyr)
library(MatchIt)
library(ggdag)  
library(dagitty)

###############
#SMD
###############

set.seed(1234)
d <- data.frame(age = rnorm(n = 200, mean = 50, 9),
                hair = as.factor(sample(x = c("brown", "black", "blond"), 200, replace = T)),
                group = sample(x = c("sick", "healthy"), 200, replace = T))
str(d)
# calculate and print the table
tabUnmatched <- tableone::CreateTableOne(vars = c("age", "hair"), strata = "group", data = d, 
                test = FALSE, smd = TRUE)
print(tabUnmatched, smd = TRUE)


#==============================.
#1A. Regression adjustment.
#===============================.

data <- tribble(
  ~id,~treat,~score,~income,
  1,0,73,2761,
  2,0,74,1254,
  3,0,70,2607,
  4,0,69,1966,
  5,0,73,2760,
  6,1,68,3385,
  7,1,68,3472,
  8,1,71,3914,
  9,1,70,2438,
  10,1,72,3020
)
data

#regression for control.

lm.treat0 <- lm(score~1 + income,data=data[data$treat==0,])
lm.treat0
data$yhat0 <- (72.8736710) + (-0.0004731 * data$income)
mean(data$yhat0)

#regression for treatment.

lm.treat1 <- lm(score~1 + income,data=data[data$treat==1,])
lm.treat1
data$yhat1 <- (71.1673963) + (-0.0004213 * data$income)
mean(data$yhat1)

data$diff<-data$yhat1  - data$yhat0

#ate:
summary(data$diff)
#atet: (=ate for the Tx)
psych::describeBy(data$diff,data$treat)

#new tibble.
data %>% dplyr::summarize(
  ate = mean(diff)
)

#atet: (=ate for the observed Tx: treat=1)
data %>%
  group_by(treat) %>%
  summarize(atet = mean(diff))

#==============================.
#1B. Regression adjustment.
#===============================.

data2 <- read_dta("D:/Causal_Inference/Datasets/cattaneo2.dta")
#View(data2)

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

data2$yhat1 <- (3267.10555) + (-0.03353 * data2$fage) + 
  (-8.51209*data2$mage) + (131.72129*data2$mmarried) + (33.70559*data2$prenatal1)
mean(data2$yhat1)

data2$diff<-data2$yhat1  - data2$yhat0
#ate
summary(data2$diff)

#atet: (=ate for the Tx)
psych::describeBy(data2$diff,data2$mbsmoke)

#new tibble.
data2 %>% dplyr::summarize(
  ate = mean(diff)
)

#atet: (=ate for the observed Tx: mbsmoke=1)
data2 %>%
  group_by(mbsmoke) %>%
  summarize(atet = mean(diff))

#====================================.
#2A IPW estimator.
#ipwpoint to generate weights 
#regression model using weight
#stabilised weights: numerator as ~1
#=========================================.

data2 <- tribble(
  ~id,~treat,~score,~income,
  1,0,73,2761,
  2,0,74,1254,
  3,0,70,2607,
  4,0,69,1966,
  5,0,73,2760,
  6,1,68,3385,
  7,1,68,3472,
  8,1,71,3914,
  9,1,70,2438,
  10,1,72,3020
)
data2

data2 = as.data.frame(data2)
data2

temp <- ipwpoint(
  exposure = treat,
  family = "binomial",
  link = "logit",
  numerator = ~ 1,
  denominator = ~ income,
  data = data2)
summary(temp$ipw.weights)
data2$ipw <- temp$ipw.weights

msm <- (svyglm(score ~ treat, design = svydesign(~ 1, weights = ~ ipw,
                                                 data = data2)))
coef(msm)
confint(msm)


#############################
#IPW: same result as above
############################

data31 <- tribble(
  ~id,~treat,~score,~income,
  1,0,73,2761,
  2,0,74,1254,
  3,0,70,2607,
  4,0,69,1966,
  5,0,73,2760,
  6,1,68,3385,
  7,1,68,3472,
  8,1,71,3914,
  9,1,70,2438,
  10,1,72,3020
)
data31 = as.data.frame(data31)

psreg <- glm(treat ~ income, 
             data = data31, 
             family = binomial(link = 'logit'))

data31 <- data31 %>%
  mutate(prob = predict(psreg, type = 'response')) %>%
  mutate(invwt = treat/prob + (1-treat)/(1-prob)) 

# Weighted least squares estimation 
ipwreg <- lm(score ~ treat, 
             data = data31, 
             weights = invwt)
summary(ipwreg) 

#######
#IPW.
#######

data22 <- read_dta("D:/Causal_Inference/Datasets/cattaneo2.dta")
data22 = as.data.frame(data22)

temp22 <- ipwpoint(
  exposure = mbsmoke,
  family = "binomial",
  link = "logit",
  numerator = ~ 1,
  denominator = ~ fage + mage + mmarried + fbaby,
  data = data22)

summary(temp22$ipw.weights)
data22$ipw <- temp22$ipw.weights

msm2 <- (svyglm(bweight ~ mbsmoke, design = svydesign(~ 1, weights = ~ ipw,
                                                      data = data22)))
coef(msm2)
confint(msm2)

##########################
#IPW: in a single step
##########################

rhc <- read_excel("D:/Causal_Inference/Datasets/rhc.xlsx")
head(rhc)

ARF<-as.numeric(rhc$cat1=='ARF')
CHF<-as.numeric(rhc$cat1=='CHF')
Cirr<-as.numeric(rhc$cat1=='Cirrhosis')
colcan<-as.numeric(rhc$cat1=='Colon Cancer')
Coma<-as.numeric(rhc$cat1=='Coma')
COPD<-as.numeric(rhc$cat1=='COPD')
lungcan<-as.numeric(rhc$cat1=='Lung Cancer')
MOSF<-as.numeric(rhc$cat1=='MOSF w/Malignancy')
sepsis<-as.numeric(rhc$cat1=='MOSF w/Sepsis')
female<-as.numeric(rhc$sex=='Female')
died<-as.numeric(rhc$death=='Yes')
age<-rhc$age
treatment<-as.numeric(rhc$swang1=='RHC')
meanbp1<-rhc$meanbp1
aps<-rhc$aps1

#new dataset.

mydata<-cbind(ARF,CHF,Cirr,colcan,Coma,lungcan,MOSF,sepsis,age,female,meanbp1,aps,treatment,died)
mydata<-data.frame(mydata)
head(mydata)

model<-ipwpoint(exposure=treatment,family="binomial",
                link="logit",denominator=~age+female+meanbp1+ARF+CHF+Cirr+colcan+Coma
                +lungcan+MOSF+sepsis,data=mydata)
#summary of weights
summary(model$ipw.weights)
ipwplot(weights=model$ipw.weights,logscale=FALSE,main="weights",xlim=c(0,22))

#attach weight to mydata.
mydata$wt=model$ipw.weights

#fit msm using svy design.
#risk difference.
msm<-(svyglm(died~treatment,
             design=svydesign(~1,
                              weights=~wt,
                              data=mydata)))
coef(msm)
confint(msm)


###########################
####rhc data ##############
######## IPW ############## 
###########################

rm(rhc)
rhc <- read_excel("D:/Causal_Inference/Datasets/rhc.xlsx")
head(rhc)

ARF<-as.numeric(rhc$cat1=='ARF')
CHF<-as.numeric(rhc$cat1=='CHF')
Cirr<-as.numeric(rhc$cat1=='Cirrhosis')
colcan<-as.numeric(rhc$cat1=='Colon Cancer')
Coma<-as.numeric(rhc$cat1=='Coma')
COPD<-as.numeric(rhc$cat1=='COPD')
lungcan<-as.numeric(rhc$cat1=='Lung Cancer')
MOSF<-as.numeric(rhc$cat1=='MOSF w/Malignancy')
sepsis<-as.numeric(rhc$cat1=='MOSF w/Sepsis')
female<-as.numeric(rhc$sex=='Female')
died<-as.numeric(rhc$death=='Yes')
age<-rhc$age
treatment<-as.numeric(rhc$swang1=='RHC')
meanbp1<-rhc$meanbp1
aps<-rhc$aps1

#new dataset.

mydata<-cbind(ARF,CHF,Cirr,colcan,Coma,lungcan,MOSF,
              sepsis,age,female,meanbp1,aps,treatment,died)
mydata<-data.frame(mydata)
head(mydata)

#list of xvars.
xvars<-c("ARF","CHF","Cirr","colcan","Coma","lungcan",
         "MOSF","sepsis","age","female","meanbp1","aps")

#estimate a propensity score.
psmodel <-glm(treatment~age+female+meanbp1+ARF+CHF+Cirr+colcan
              +Coma+lungcan+MOSF+sepsis,
              family = binomial(link="logit"))
psmodel

#p_score (Pr Tx).
ps<-predict(psmodel,type="response")
summary(ps)

#create weights and check balance.
#probability of treatment for treated.
#probability of not being treated for control.

weight<-ifelse(treatment==1,1/(ps),1/(1-ps))
summary(weight)
wtd_data<-svydesign(ids=~1,data=mydata,weights=~weight)

wtd_table1<-svyCreateTableOne(vars=xvars,
                              strata="treatment",
                              data=wtd_data,
                              test=FALSE)
print(wtd_table1,smd=TRUE)

#weighted mean for the Tx group.
#matches the value in Table 1 (mean age;strata==treatment).
mean(weight[treatment==0]*age[treatment==0])/mean(weight[treatment==0])
mean(weight[treatment==1]*age[treatment==1])/mean(weight[treatment==1])

a<-mean(weight[treatment==1]*age[treatment==1])  
b<-mean(weight[treatment==1])
a/b

#Marginal structural models: outcome = died.
#note we have the weights related to probability of treatment.

#causal relative risk (weighted GLM).
glm2.obj<-glm(died~treatment,
              weights=weight,
              family=binomial(link=log))
summary(glm2.obj)
beta_iptw<-coef(glm2.obj)
SE<-sqrt(diag(vcovHC(glm2.obj,type="HC0")))
summary(SE)

#Relative Risk.
causalrr<-exp(beta_iptw[2])
lcl<-exp(beta_iptw[2]-1.96*SE[2])
ucl<-exp(beta_iptw[2]+1.96*SE[2])
results<-c(lcl,causalrr,ucl)
results

#Risk difference.
glm3.obj<-glm(died~treatment,
              weights=weight,
              family=binomial(link="identity"))
summary(glm3.obj)
beta_iptw<-coef(glm3.obj)
SE<-sqrt(diag(vcovHC(glm3.obj,type="HC0")))
summary(SE)

causalrd<-(beta_iptw[2])
lcl<-(beta_iptw[2]-1.96*SE[2])
ucl<-(beta_iptw[2]+1.96*SE[2])
results<-c(lcl,causalrd,ucl)
results
#ate=0.05154951.

############################################
#This was done in two steps.
#create a weight then do the glm.
#code below combines the steps.
############################################

#denominator = denominator of the weights.
#which is the propensity score model.
model<-ipwpoint(exposure=treatment,
                family="binomial",
                link="logit",
                denominator=~age+female+meanbp1+
                  ARF+CHF+Cirr+colcan+Coma+lungcan+MOSF+sepsis,
                data=mydata)
#summary of weights
summary(model$ipw.weights)
ipwplot(weights=model$ipw.weights,
        logscale=FALSE,
        main="weights",
        xlim=c(0,22))

#attach weight to mydata.
mydata$wt=model$ipw.weights

#fit msm using svy design.
#risk difference.
msm<-(svyglm(died~treatment,
             design=svydesign(~1,weights=~wt,
                              data=mydata)))
coef(msm)
confint(msm)

#matches:
#teffects ipw (died) (treatment ARF CHF Cirr colcan Coma 
#lungcan MOSF sepsis age female meanbp1, logit), ate

##########################
#truncate weights.
###########################

truncwt<-replace(model$ipw.weights,model$ipw.weights>10,10)
#use new weight in subsequent glm.

#truncate in ipw by specifying a percentile.
model2<-ipwpoint(exposure=treatment,
                 family="binomial",
                 link="logit",
                 denominator=~age+female+meanbp1+ARF+CHF+Cirr+colcan+
                   Coma+lungcan+MOSF+sepsis,
                 data=mydata,
                 trunc=0.01)
summary(model2$weights.trunc)
ipwplot(weights=model2$weights.trunc,
        logscale=FALSE,
        main="weights",xlim=c(0,22))

mydata$wt<-model2$weights.trunc
msm<-(svyglm(died~treatment,
             design=svydesign(~1,weights=~wt,
                              data=mydata)))
coef(msm)
confint(msm)

##########################
## IPW
## -l- is the confounder
#########################

set.seed(16)
n <- 1000
simdat <- data.frame(l = rnorm(n, 10, 5))
a.lin <- simdat$l - 10
pa <- exp(a.lin)/(1 + exp(a.lin))
simdat$a <- rbinom(n, 1, prob = pa)
simdat$y <- 10*simdat$a + 0.5*simdat$l + rnorm(n, -10, 5)
simdat[1:5,]

temp <- ipw::ipwpoint(exposure = a, 
                      family = "binomial", 
                      link = "logit",
                      numerator = ~ 1, 
                      denominator = ~ l, 
                      data = simdat)
summary(temp$ipw.weights)
summary(temp$num.mod)
summary(temp$den.mod)
simdat$sw <- temp$ipw.weights

msm <- (svyglm(y ~ a, 
               design = svydesign(~ 1, 
                                  weights = ~ sw,
                                  data = simdat)))
coef(msm)
confint(msm)


#############
#NN matching
#############

data34 <- tribble(
  ~id,~treat,~score,~income2,
  1,0,73,5,
  2,0,74,4,
  3,0,70,3,
  4,0,69,2,
  5,0,73,1,
  6,1,68,1,
  7,1,68,2,
  8,1,71,3,
  9,1,70,4,
  10,1,72,5
)
data34 = as.data.frame(data34)

#===============.
#using matchit.
#===============.

# Default: 1:1 NN PS matching w/o replacement
m.out1 <- matchit(treat ~ income2, data = data34)
m.out1
summary(m.out1)
md<-match.data(m.out1)
md
fit1 <- lm(score ~ treat, data = md, weights = weights)
fit1

#Linear model with covariates
fit3 <- lm(score ~ treat + income2, data = md,
           weights = weights)
fit3


#============================.
#5B. Nearest neighbour Matching.
#ties.
#==============================.

data36 <- tribble(
  ~id,~treat,~score,~income2,
  1,0,73,5,
  2,0,74,5,
  3,0,70,3,
  4,0,69,2,
  5,0,73,1,
  6,1,68,1,
  7,1,68,2,
  8,1,71,3,
  9,1,70,4,
  10,1,72,5
)
data36 = as.data.frame(data36)


m.out1 <- MatchIt::matchit(treat ~ income2,replace=TRUE,
                           method = "nearest",
                           estimand = "ATT",
                           ratio = 2, 
                           min.controls = 1, max.controls = 3,
                           data = data36)
m.out1
summary(m.out1)
md<-match.data(m.out1)

#get matches
#multiple rows per matched unit

g.matches1 <- get_matches(m.out1, data = data36,
                          distance = "propscore",id = "mid")
head(g.matches1, 10)
#export stata dataset.
#haven::write_dta(g.matches1,"C:/Temp/File.dta")

fit1 <- lm(score ~ treat, data = md,weights = weights)
fit1

#################
#exact matching
##################

#data22 <- read_dta("D:/Causal_Inference/Datasets/cattaneo2.dta")

m.out22 <- MatchIt::matchit(mbsmoke ~ 
                              mmarried + mage + fage
                            + medu + prenatal1,
                            exact ~ mmarried + prenatal1,
                            distance = "mahalanobis",
                            method = "nearest",
                            estimand = "ATT",
                            ratio = 2,
                            data = data22)
m.out22
summary(m.out22)
md<-match.data(m.out22)

fit1 <- lm(bweight ~ mbsmoke + mmarried + mage + fage
           + medu + prenatal1, data = md,weights = weights)
fit1

#################
#NN
##################

data42 <- tribble(
  ~id,~treat,~score,~income2,
  1, 0, 68, 4,
  2, 0, 68, 7,
  3, 0, 70, 2,
  4, 0, 69, 6,
  5, 0, 73, 5,
  6, 0, 69, 4,
  7, 0, 71, 7,
  8, 0, 72, 2,
  9, 0, 74, 6,
  10, 0, 73, 5,
  11, 1, 68, 7,
  12, 1, 68, 4,
  13, 1, 71, 5,
  14, 1, 70, 6,
  15, 1, 72, 2,
  16, 1, 71, 7,
  17, 1, 70, 4,
  18, 1, 69, 5,
  19, 1, 70, 6,
  20, 1, 72, 2,
  21, 0, 68, 4,
  22, 0, 68, 7,
  23, 0, 70, 2,
  24, 0, 69, 6,
  25, 0, 73, 5,
  26, 0, 69, 4,
  27, 0, 71, 7,
  28, 0, 72, 2,
  29, 0, 74, 6,
  30, 0, 73, 5,
  31, 1, 68, 7,
  32, 1, 68, 4,
  33, 1, 71, 5,
  34, 1, 70, 6,
  35, 1, 72, 2,
  36, 1, 71, 7,
  37, 1, 70, 4,
  38, 1, 69, 5,
  39, 1, 70, 6,
  40, 1, 72, 2
)
data42 = as.data.frame(data42)
m.out42 <- MatchIt::matchit(treat ~ income2,
                            exact ~ income2,
                            distance = "mahalanobis",
                            method = "nearest",
                            estimand = "ATT",
                            ratio = 1,
                            data = data42)
m.out42
summary(m.out42)
md<-match.data(m.out42)

fit1 <- lm(score ~ treat, data = md,weights = weights)
fit1

#=======================================.
#Example 1 (nearest neighbour matching).
#Outcome is died.
#=======================================.

rm(rhc)
rhc <- read_excel("D:/Causal_Inference/Datasets/rhc.xlsx")
head(rhc)

ARF<-as.numeric(rhc$cat1=='ARF')
CHF<-as.numeric(rhc$cat1=='CHF')
Cirr<-as.numeric(rhc$cat1=='Cirrhosis')
colcan<-as.numeric(rhc$cat1=='Colon Cancer')
Coma<-as.numeric(rhc$cat1=='Coma')
COPD<-as.numeric(rhc$cat1=='COPD')
lungcan<-as.numeric(rhc$cat1=='Lung Cancer')
MOSF<-as.numeric(rhc$cat1=='MOSF w/Malignancy')
sepsis<-as.numeric(rhc$cat1=='MOSF w/Sepsis')
female<-as.numeric(rhc$sex=='Female')
died<-as.numeric(rhc$death=='Yes')
age<-rhc$age
treatment<-as.numeric(rhc$swang1=='RHC')
meanbp1<-rhc$meanbp1
aps<-rhc$aps1
summary(meanbp1)

#new dataset.

mydata<-cbind(ARF,CHF,Cirr,colcan,Coma,lungcan,MOSF,sepsis,age,female,meanbp1,treatment,died)
mydata<-data.frame(mydata)

#export stata.
#haven::write_dta(mydata,"C:/Temp/mydata.dta")

#list of xvars.

xvars<-c("ARF","CHF","Cirr","colcan","Coma","lungcan",
         "MOSF","sepsis","age","female","meanbp1")

#smd (pre-matching): treatment vs control.

table1<-CreateTableOne(vars=xvars,strata="treatment",
                       data=mydata,test=FALSE)
print(table1,smd=TRUE)


#===============================.
#nearest-neighbour matching (1:1).
#2816 matched pairs.
#===============================

NNMATCH<-Match(Tr=treatment,M=1,X=mydata[xvars])
matched<-mydata[unlist(NNMATCH[c("index.treated",
                                 "index.control")]),]
View(matched)
#ssm on matched data.

matchedTable1<-CreateTableOne(vars=xvars,strata="treatment",
                              data=matched,test=FALSE)
print(matchedTable1,smd=TRUE)

#Paired t-test.
#POM.

y_treated<-matched$died[matched$treatment==1]
y_control<-matched$died[matched$treatment==0]

#difference for each pair (treatment - control)
DIFF_Y<-(y_treated) - (y_control)

#paired t-test.
#ate=0.0452882.

t.test(DIFF_Y)

#McNemar test.

table(y_treated,y_control)
mcnemar.test(matrix(c(994,493,394,305),2,2))

#close to Stata.
#teffects nnmatch (died ARF CHF Cirr colcan Coma lungcan MOSF sepsis 
#age female meanbp1) (treatment), ate 

############################################
#Nearest neighbour Matching (using matchit)
#NNM (using matchit).
#Steps:
#For those in the tx group find match in control group 
#with similar x values
#TE for control = (matched Y - observed Y)
#TE for treat   = (observed Y - matched Y)
#out <- matchit(Tx ~ xvars, 
#                  data = data,
#                  method = "nearest", 
#                  distance = "glm")
############################################

data34 <- tribble(
  ~id,~treat,~score,~income2,
  1,0,73,5,
  2,0,74,4,
  3,0,70,3,
  4,0,69,2,
  5,0,73,1,
  6,1,68,1,
  7,1,68,2,
  8,1,71,3,
  9,1,70,4,
  10,1,72,5
)
data34 = as.data.frame(data34)

## Covariates
vars <- c("income2")

## Construct a table
tabUnmatched <- CreateTableOne(vars = vars, 
                               strata = "treat", 
                               data = data34, 
                               test = FALSE)
## Show table with SMD
print(tabUnmatched, smd = TRUE)

# Default: 1:1 NN PS matching w/o replacement
m.out <- MatchIt::matchit(treat ~ income2, data = data34)
m.out
#method: 1:1 NNM without replacement
#distance: Propensity score
#estimated with logistic regression

summary(m.out)
#5 in control matched with 5 in Tx

md<-match.data(m.out)
md
#dataset with id for the matched pair (subclass).
#id=1 matched with id=9.

#estimation
fit1 <- lm(score ~ treat, data = md, weights = weights)
fit1
#ate=-2.

#Linear model with covariates
fit3 <- lm(score ~ treat + income2, data = md, weights = weights)
fit3
#ate=-2.

#########################################
# NNM (with ties)
#########################################

data36 <- tribble(
  ~id,~treat,~score,~income2,
  1,0,73,5,
  2,0,74,5,
  3,0,70,3,
  4,0,69,2,
  5,0,73,1,
  6,1,68,1,
  7,1,68,2,
  8,1,71,3,
  9,1,70,4,
  10,1,72,5
)
data36 = as.data.frame(data36)

#different matching (no longer 1:1).
#now the weights are very important.
#matching: ATT rather than ATE.

m.out1 <- MatchIt::matchit(treat ~ income2,replace=TRUE,
                           method = "nearest",
                           estimand = "ATT",
                           ratio = 2, 
                           min.controls = 1, max.controls = 3,
                           data = data36)
m.out1
#method: Variable ratio 2:1 NNM with replacement
#- distance: Propensity score
#- estimated with logistic regression

summary(m.out1)
md<-match.data(m.out1)
md

#Get matches:multiple rows per matched unit

g.matches1 <- get_matches(m.out1, data = data36,
                          distance = "propscore",id = "mid")
#sort.
g.matches1 <- g.matches1 %>%
  dplyr::arrange(g.matches1, subclass,.by_group=TRUE)
#View(g.matches1)
#export stata dataset.
#haven::write_dta(g.matches1,"C:/Temp/File.dta")

fit1 <- lm(score ~ treat, data = md,weights = weights)
fit1


###############################
# Nearest neighbour Matching.
###############################

data37 <- tribble(
  ~id,~treat,~score,~income2,
  1,0,68,4,
  2,0,68,4,
  3,0,70,2,
  4,0,69,6,
  5,0,73,5,
  6,1,68,6,
  7,1,68,6,
  8,1,71,5,
  9,1,70,4,
  10,1,72,7
)
data37 = as.data.frame(data37)

# Default: 1:1 NN PS matching w/o replacement
m.out1 <- MatchIt::matchit(treat ~ income2, data = data37)
m.out1
summary(m.out1)
md<-match.data(m.out1)
md
fit1 <- lm(score ~ treat, data = md, weights = weights)
fit1


#############################
# Nearest neighbour Matching
#############################

data42 <- tribble(
  ~id,~treat,~score,~income2,
  1,	0,	68,	4,
  2,	0,	68,	7,
  3,	0,	70,	2,
  4,	0,	69,	6,
  5,	0,	73,	5,
  6,	0,	69,	4,
  7,	0,	71,	7,
  8,	0,	72,	2,
  9,	0,	74,	6,
  10,	0,	73,	5,
  11,	1,	68,	7,
  12,	1,	68,	4,
  13,	1,	71,	5,
  14,	1,	70,	6,
  15,	1,	72,	2,
  16,	1,	71,	7,
  17,	1,	70,	4,
  18,	1,	69,	5,
  19,	1,	70,	6,
  20,	1,	72,	2,
  21,	0,	68,	4,
  22,	0,	68,	7,
  23,	0,	70,	2,
  24,	0,	69,	6,
  25,	0,	73,	5,
  26,	0,	69,	4,
  27,	0,	71,	7,
  28,	0,	72,	2,
  29,	0,	74,	6,
  30,	0,	73,	5,
  31,	1,	68,	7,
  32,	1,	68,	4,
  33,	1,	71,	5,
  34,	1,	70,	6,
  35,	1,	72,	2,
  36,	1,	71,	7,
  37,	1,	70,	4,
  38,	1,	69,	5,
  39,	1,	70,	6,
  40,	1,	72,	2
)
data42 = as.data.frame(data42)

m.out42 <- MatchIt::matchit(treat ~ income2,
                            exact ~ income2,
                            distance = "mahalanobis",
                            method = "nearest",
                            estimand = "ATT",
                            ratio = 1,
                            data = data42)
m.out42
summary(m.out42)
md<-match.data(m.out42)

fit1 <- lm(score ~ treat, data = md,weights = weights)
fit1


################
# PS Matching
################

data137 <- tribble(
  ~id,~treat,~score,~income2,
  1,0,73,4,
  2,0,74,4,
  3,0,70,2,
  4,0,69,6,
  5,0,73,5,
  6,1,68,6,
  7,1,68,6,
  8,1,71,5,
  9,1,70,4,
  10,1,72,7
)
data137 = as.data.frame(data137)
#Both give the same result.
#Matching on the PS.

m.out5a <- matchit(treat ~ income2, data = data137,
                   distance = "glm",
                   estimand = "ATT",
                   mahvars = ~ income2)
m.out5a
summary(m.out5a)
md.a<-match.data(m.out5a)
md.a
fit1.a <- lm(score ~ treat, data = md.a, weights = weights)
fit1.a

#Option B.
m.out5.b <- matchit(treat ~ income2, data = data137,
                    distance = "glm",
                    estimand = "ATT",
                    method = "nearest")
m.out5.b
summary(m.out5.b)
md.b<-match.data(m.out5.b)
md.b
fit1.b <- lm(score ~ treat, data = md.b, weights = weights)
fit1.b

############################
# PS Matching (One-to-one).
############################

rm(data22)
data231 <- haven::read_dta("D:/Causal_Inference/Datasets/cattaneo2.dta")
data231 = as.data.frame(data231)

#The match.data() output is preferred when pair membership is not 
#directly included in the analysis; 

#The get_matches() output is preferred when 
#pair membership is to be included.


#NN Mahalanobis distance matching w/ replacement.
m.out <- matchit(mbsmoke ~ mmarried + mage + medu + fbaby, 
                 data = data231,
                 distance = "mahalanobis", ties=TRUE,
                 replace = TRUE)
m.out
summary(m.out)

#1151 obs matched.
#864 Tx matched with 287 control (meaning 864 pairs).
#864*2 = 1728.
#EACH TX=1 has been matched with at least 1 control.

#match data.

md <- match.data(m.out, data = data231,
                 distance = "prop_score")
dim(md) #one row per matched unit [N=1151].
head(md, 10)

#get matches

gm <- get_matches(m.out, data = data231,
                  distance = "prop_score")
dim(gm) #multiple rows per matched unit
head(gm, 10)
#864 pairs (1728 rows)

#Number of control units in each match stratum
table(table(gm$subclass[gm$A == 0]))
table(table(gm$subclass[gm$A == 1]))

#match.data() output
fit1md <- lm(bweight ~ mbsmoke, data = md, weights = weights)
fit1md

# NN Mahalanobis distance matching w/ replacement.
m.out <- matchit(mbsmoke ~ mmarried + mage + medu + fbaby, 
                 data = data231,
                 distance = "mahalanobis", ties=TRUE,ratio=2,
                 replace = TRUE)
m.out
summary(m.out)

#1409 obs matched.
#864 Tx matched with 545 control (meaning 864 pairs).
#864*2 = 1728.
#EACH TX=1 has been matched with at least 1 control.

#match data.

md <- match.data(m.out, data = data231,
                 distance = "prop_score")
dim(md) #one row per matched unit [N=1409].
head(md, 10)

#get matches

gm <- get_matches(m.out, data = data231,
                  distance = "prop_score")
dim(gm) #multiple rows per matched unit
head(gm, 10)
#864 pairs (2592 rows)
#View(gm)
#(1 Tx + 1 control + 1 control) * 864.
#864*3 = 2592.

#Number of control units in each match stratum
table(table(gm$subclass[gm$mbsmoke == 0])) # 2 control for each Tx.

#match.data() output
fit1md <- lm(bweight ~ mbsmoke, data = md, weights = weights)
fit1md

#Nearest neighbour
m.out <- matchit(mbsmoke ~ mmarried + mage + medu + fbaby, 
                 data = data231,
                 method = "nearest", ties=TRUE,ratio=1,
                 replace = TRUE)
m.out
summary(m.out)

#1155 obs matched.
#864 Tx matched with 291 control (meaning 864 pairs).
#864*2 = 1728.
#EACH TX=1 has been matched with at least 1 control.

#match data.

md <- match.data(m.out, data = data231,
                 distance = "prop_score")
dim(md) #one row per matched unit [N=1409].
head(md, 10)

#get matches

gm <- get_matches(m.out, data = data231,
                  distance = "prop_score")
dim(gm) #multiple rows per matched unit
head(gm, 10)
#864 pairs (1728 rows)

#Number of control units in each match stratum
table(table(gm$subclass[gm$mbsmoke == 0])) # 1 control for each Tx.

#match.data() output
fit1md <- lm(bweight ~ mbsmoke, data = md, weights = weights)
fit1md

#try variable #matches.
m.out <- matchit(mbsmoke ~ mmarried + mage + medu + fbaby, 
                 data = data231, 
                 ties=TRUE,
                 replace = TRUE,
                 method = "nearest",
                 ratio = 2,
                 min.controls = 1, max.controls = 80)
m.out
summary(m.out)

#1462 obs matched.
#864 Tx matched with 598 control (meaning 864 pairs).
#864*2 = 1728.
#EACH TX=1 has been matched with at least 1 control.

#match data.

md <- match.data(m.out, data = data231,
                 distance = "prop_score")
dim(md) #one row per matched unit [N=1151].
head(md, 10)

#get matches

gm <- get_matches(m.out, data = data231,
                  distance = "prop_score")
dim(gm) #multiple rows per matched unit
head(gm, 10)
#864 pairs (1728 rows)

#Number of control units in each match stratum
table(table(gm$subclass[gm$mbsmoke == 0]))

#match.data() output
fit1md <- lm(bweight ~ mbsmoke, data = md, weights = weights)
fit1md

########
# PSM
#########

data138 <- tribble(
  ~id,~treat,~score,~income2,~wealth,
  1,	0,	68,	4,	18,
  2,	0,	68,	7,	14,
  3,	0,	70,	2,	19,
  4,	0,	69,	6,	18,
  5,	0,	73,	5,	16,
  6,	0,	69,	4,	18,
  7,	0,	71,	7,	20,
  8,	0,	72,	2,	20,
  9,	0,	74,	6,	20,
  10,	0,	73,	5,	17,
  11,	0,	68,	4,	17,
  12,	0,	68,	7,	20,
  13,	0,	70,	2,	15,
  14,	0,	69,	6,	20,
  15,	0,	73,	5, 19,
  16,	0,	69,	4,	18,
  17,	0,	71,	7,	18,
  18,	0, 72,	2,	15,
  19,	0,	74,	6,	20,
  20,	0,	73,	5,	17,
  21,	1,	68,	7,	15,
  22,	1,	68,	4,	14,
  23,	1,	71,	5,	19,
  24,	1,	70,	6,	20,
  25,	1,	72,	2,	22,
  26,	1,	71,	7,	13,
  27,	1,	70,	4,	17,
  28,	1,	69,	5,	21,
  29,	1,	70,	6,	18,
  30,	1,	72,	2,	14,
  31,	1,	68,	7,	13,
  32,	1,	68,	4,	23,
  33,	1,	71,	5,	14,
  34,	1,	70,	6,	22,
  35,	1,	72,	2,	19,
  36,	1,	71,	7,	16,
  37,	1,	70,	4,	20,
  38,	1,	69,	5,	21,
  39,	1,	70,	6,	18,
  40,	1,	72,	2,	19
)
data138 = as.data.frame(data138)

m.out <- matchit(treat ~ income2 + wealth, 
                 data = data138,
                 method = "nearest", 
                 ties=TRUE,ratio=1,
                 replace = TRUE)
m.out
summary(m.out)

#match data.
md <- match.data(m.out, data = data138,
                 distance = "prop_score")
dim(md) #one row per matched unit 
head(md, 10)

#get matches

gm<- get_matches(m.out, data = data138,id = "mid",
                 distance = "prop_score")
dim(gm) #multiple rows per matched unit
head(gm, 10)

#Number of control units in each match stratum
table(table(gm$subclass[gm$treat == 0])) # 1 control for each Tx.

#match.data() output
fit1md <- lm(score ~ treat, data = md, weights = weights)
fit1md

###############
# PS Matching
###############

rm(data231)
data231 <- haven::read_dta("D:/Causal_Inference/Datasets/cattaneo2.dta")
data2 = as.data.frame(data231)

#Use exact matching in combination with another matching method 

m.out5c <- matchit(mbsmoke ~ mmarried + mage + medu + fbaby, 
                   data = data2,
                   distance = "glm",
                   estimand = "ATT",
                   method = "nearest",
                   ratio = 2,
                   min.controls = 1, max.controls = 80)
m.out5c
summary(m.out5c)

#get_matches output
gm <- get_matches(m.out5c)
nrow(gm)

#Number of control units in each match stratum
table(table(gm$subclass[gm$A == 0]))
#matched data output.

md.c<-match.data(m.out5c)
fit1.c <- lm(bweight ~ mbsmoke, data = md.c, weights = weights)
fit1.c

#######################################################
#teffects psmatch (bweight) (mbsmoke mmarried mage medu fbaby), nneighbor(1)  atet 
#######################################################

#####################
#PSM Matching::MatchBalance
#####################

data6 <- tribble(
  ~id,~treat,~score,~income2,
  1,	0,	73,	4,
  2,	0,	74,	4,
  3,	0,	70,	2,
  4,	0,	69,	6,
  5,	0,	73,	5,
  6,	1,	68,	6,
  7,	1,	68,	6,
  8,	1,	71,	5,
  9,	1,	70,	4,
  10,	1,	72,	7
)
data6 = as.data.frame(data6)
data6

prop.model<-stats::glm(treat ~ income2, 
                       family = binomial(link=logit), 
                       data = data6)

#Summary of propensity score model
summary(prop.model)

#Propensity scores for each subject
pscore <- ifelse(data6$treat == 0, 
                 1 - predict(prop.model, type = "response"),
                 predict(prop.model, type = "response"))

data6<-cbind(data6,pscore)

#assess balance.
mb <- MatchBalance(treat ~ income2, 
                   data=data6,nboots=10)
mb

#match on the propensity score
prop.model2 <- glm(treat ~ income2, 
                   family = binomial(), 
                   data = data6)
X <- prop.model2$fitted
Y <- data6$score
Tr <- data6$treat

#ATET
rr <- Match(Y=Y,Tr=Tr,X=X,M=1,estimand="ATT")
summary(rr)

#ATE
rr2 <- Match(Y=Y,Tr=Tr,X=X,M=1,estimand="ATE")
summary(rr2)

#Unadjusted t-test
t.test(data6$score[as.factor(data6$treat)==0],
       data6$score[as.factor(data6$treat)==1],var.equal=T)

MatchBalance(treat ~ income2, data=data6,
             match.out=rr,nboots=10)

#Include pscore in the model.
prop.model.adj <- glm(score ~ treat + pscore, data = data6)
summary(prop.model.adj)
confint(prop.model.adj)

##############################
#PSM Matching: MatchBalance
##############################

data31 <- tribble(
  ~id,~treat,~sex,~age,~bp,
  1,	0,	0,	18,	132,
  2,	0,	0,	18,	130,
  3,	0,	0,	20,	131,
  4,	0,	0,	22,	131,
  5,	0,	1,	20,	140,
  6,	0,	1,	18,	131,
  7,	0,	1,	17,	140,
  8,	0,	1,	21,	140,
  9,	0,	1,	23,	132,
  10,	0,	1,	24,	133,
  11,	1,	0,	22,	136,
  12,	1,	0,	23,	132,
  13,	1,	0,	21,	140,
  14,	1,	0,	18,	130,
  15,	1,	0,	17,	139,
  16,	1,	0,	21,	133,
  17,	1,	1,	24,	139,
  18,	1,	1,	23,	140,
  19,	1,	1,	23,	139,
  20,	1,	1,	24,	132
)
data31 = as.data.frame(data31)
prop.model <- glm(treat ~ as.factor(sex) + age, 
                  family = binomial(), data = data31)

#Summary of propensity score model
summary(prop.model)

#Propensity scores for each subject
pscore <- ifelse(data31$treat == 0, 
                 1 - predict(prop.model, type = "response"),
                 predict(prop.model, type = "response"))

data31<-cbind(data31,pscore)

#assess balance.

mb <- MatchBalance(treat ~ as.factor(sex) + age, 
                   data=data31,nboots=10)
mb

#match on the propensity score

prop.model2 <- glm(treat ~ as.factor(sex) + age, family = binomial(), 
                   data = data31)
X <- prop.model2$fitted
Y <- data31$bp
Tr <- data31$treat
rr <- Match(Y=Y,Tr=Tr,X=X,M=1,estimand="ATE")
summary(rr)

#Include pscore in the model.

prop.model.adj <- glm(bp ~ treat + pscore, data = data31)
summary(prop.model.adj)
confint(prop.model.adj)

##########################################
#Full example using Match and Matchit
##########################################

rm(rhc)
rhc <- read_excel("D:/Causal_Inference/Datasets/rhc.xlsx")

#create a data set with just these variables, for simplicity
ARF<-as.numeric(rhc$cat1=='ARF')
CHF<-as.numeric(rhc$cat1=='CHF')
Cirr<-as.numeric(rhc$cat1=='Cirrhosis')
colcan<-as.numeric(rhc$cat1=='Colon Cancer')
Coma<-as.numeric(rhc$cat1=='Coma')
COPD<-as.numeric(rhc$cat1=='COPD')
lungcan<-as.numeric(rhc$cat1=='Lung Cancer')
MOSF<-as.numeric(rhc$cat1=='MOSF w/Malignancy')
sepsis<-as.numeric(rhc$cat1=='MOSF w/Sepsis')
female<-as.numeric(rhc$sex=='Female')
died<-as.numeric(rhc$death=='Yes')
age<-rhc$age
treatment<-as.numeric(rhc$swang1=='RHC')
meanbp1<-rhc$meanbp1

#new dataset
mydata<-cbind(ARF,CHF,Cirr,colcan,Coma,lungcan,MOSF,sepsis,
              age,female,meanbp1,treatment,died)
mydata<-data.frame(mydata)

#Export Stata dataset.
#library(foreign,lib="C:/Users/Shaun/Documents/R/win-library/4.0")
#write.dta(mydata, "C:/Temp/mydata.dta")

#covariates we will use (shorter list than you would use in practice)
xvars<-c("ARF","CHF","Cirr","colcan","Coma","lungcan","MOSF","sepsis",
         "age","female","meanbp1")

#treatment = treatment.
#outcome = died.

#look at a table 1
table1<- CreateTableOne(vars=xvars,strata="treatment", data=mydata, test=FALSE)
## include standardized mean difference (SMD)
print(table1,smd=TRUE)

############################################
#do greedy matching on Mahalanobis distance
#pair matching (M=1)
############################################

MyNNM<-Match(Tr=treatment,M=1,X=mydata[xvars],replace=FALSE)
matched<-mydata[unlist(MyNNM[c("index.treated","index.control")]), ]

#get table 1 for matched data with standardized differences
matchedTable1<-CreateTableOne(vars=xvars, strata ="treatment", 
                              data=matched, test = FALSE)
print(matchedTable1, smd = TRUE)


#outcome analysis (remember that is paired data: groups are same size).
y_treat<-matched$died[matched$treatment==1]
y_control<-matched$died[matched$treatment==0]

#pairwise difference
diff_y<-(y_treat - y_control)
summary(diff_y)

#paired t-test (causal risk difference).
#a standard t-test on the paired data (1 row for each pair)
t.test(diff_y)

#Interpretation: causal effect.
#Difference in mean (Tx versus control).
#Difference in probability of death if EVERYONE received RHC 
#versus if NO ONE received RHC.

#McNemar test
table(y_treat,y_control)
mcnemar.test(matrix(c(973,513,395,303),2,2))


##########################
#propensity score matching
#1. Estimate propensity score			
#2. Assess balance / overlap			
#3. Matching: possible caliper			
#4. Analysis: permutation tests			
#########################

#fit a propensity score model. logistic regression

psmodel<-glm(treatment~ARF+CHF+Cirr+colcan+Coma+lungcan+MOSF+sepsis+age+female+meanbp1,
             family=binomial(),
             data=mydata)

#show coefficients etc
summary(psmodel)

#create propensity score
pscore<-psmodel$fitted.values

#------------------------------------------------------------.
#do greedy matching on logit(PS) using Match with a caliper
#replace=FALSE: not allow people to be rematched.
#0.2 of the SD of the logit score.
#fewer matched pairs.
#-------------------------------------------------------------.

logit <- function(p) {log(p)-log(1-p)}
psmatch4<-Match(Tr=mydata$treatment,M=1,X=logit(pscore),replace=FALSE,caliper=.2)
matched<-mydata[unlist(psmatch4[c("index.treated","index.control")]), ]
xvars<-c("ARF","CHF","Cirr","colcan","Coma","lungcan","MOSF","sepsis",
         "age","female","meanbp1")

#get standardized differences
matchedTable1<-CreateTableOne(vars=xvars, strata ="treatment", 
                              data=matched, test = FALSE)
print(matchedTable1, smd = TRUE)

#outcome analysis
y_treat<-matched$died[matched$treatment==1]
y_control<-matched$died[matched$treatment==0]

#pairwise difference
diff_y<-(y_treat - y_control)

#paired t-test
t.test(diff_y)

#################################################.
#different code using MatchIt.
#do not have to fit the propensity model first.
#################################################.

m.out<-matchit(treatment~ARF+CHF+Cirr+colcan+Coma+
                 lungcan+MOSF+sepsis+age+female+meanbp1,
               data=mydata,
               method="nearest")
summary(m.out)

#plot(m.out,type="jitter")
#plot(m.out,type="hist")

#----------------------------------------.
#expit <- function(x) {1/(1+exp(-x)) }
#logit <- function(p) {log(p)-log(1-p)}
#---------------------------------------.

#-------------------------------.
#Fit marginal model using IPTW.
#-------------------------------.

#look at a table 1
table1<- CreateTableOne(vars=xvars,strata="treatment", 
                        data=mydata, test=FALSE)

## include standardized mean difference (SMD)
print(table1,smd=TRUE)

#propensity score model
psmodel <- glm(treatment ~ age + female + meanbp1+ARF+CHF+
                 Cirr+colcan+Coma+lungcan+MOSF+sepsis,
               family  = binomial(link ="logit"),
               data=mydata)
psmodel

## value of propensity score for each subject
ps <-predict(psmodel, type = "response")
head(ps)

#create weights based on ps.
#then check balance.
weight<-ifelse(mydata$treatment==1,1/(ps),1/(1-ps))

#apply weights to data
weighteddata<-svydesign(ids = ~ 1, data =mydata, weights = ~ weight)
#class(weighteddata)=survey design

#weighted table 1
weightedtable <-svyCreateTableOne(vars = xvars, strata = "treatment", 
                                  data = weighteddata, test = FALSE)

## Show table with SMD
print(weightedtable, smd = TRUE)

#to get a weighted mean for a single covariate directly:
#to match estimate in the table.
mean(weight[treatment==0]*age[treatment==0])/(mean(weight[treatment==0]))
mean(weight[treatment==1]*age[treatment==1])/(mean(weight[treatment==1]))

#--------------------------------------.
#1Causal risk difference (identity).
#MSM using the weight.
#---------------------------------------.

glm.obj<-glm(died~treatment,weights=weight,
             family=quasibinomial(link="identity"),
             data=mydata)
glm.obj

#summary(glm.obj)
betaiptw<-coef(glm.obj)
SE<-sqrt(diag(vcovHC(glm.obj, type="HC0")))

#RD = 0.0515.
causalrd<-(betaiptw[2])
lcl<-(betaiptw[2]-1.96*SE[2])
ucl<-(betaiptw[2]+1.96*SE[2])
c(lcl,causalrd,ucl)

#---------------------------------------.
#2. causal relative risk. Weighted GLM
#---------------------------------------.

glm.obj<-glm(died~treatment,weights=weight,
             family=quasibinomial(link=log),
             data=mydata)

#summary(glm.obj)
betaiptw<-coef(glm.obj)

#to properly account for weighting, use asymptotic (sandwich) variance
SE<-sqrt(diag(vcovHC(glm.obj, type="HC0")))

#get point estimate and CI for relative risk (need to exponentiate)
causalrr<-exp(betaiptw[2])
lcl<-exp(betaiptw[2]-1.96*SE[2])
ucl<-exp(betaiptw[2]+1.96*SE[2])
c(lcl,causalrr,ucl)
#RR = 1.0817.

#truncate weights at 10
truncweight<-replace(weight,weight>10,10)

#get causal risk difference
glm.obj<-glm(died~treatment,weights=truncweight,
             family=quasibinomial(link="identity"),
             data=mydata)

#summary(glm.obj)
betaiptw<-coef(glm.obj)
SE<-sqrt(diag(vcovHC(glm.obj, type="HC0")))

causalrd<-(betaiptw[2])
lcl<-(betaiptw[2]-1.96*SE[2])
ucl<-(betaiptw[2]+1.96*SE[2])
c(lcl,causalrd,ucl)


#############################
#alternative: use ipw package
#R calculates the weights for you as part of ps.
#Then fit the MSM.
#############################

#first fit propensity score model to get weights
#denominator = xvars in the ps model.

weightmodel<-ipwpoint(exposure= treatment, 
                      family = "binomial", 
                      link ="logit",
                      denominator= ~ age + female + 
                        meanbp1+ARF+CHF+Cirr+colcan+Coma+
                        lungcan+MOSF+sepsis, 
                      data=mydata)

#numeric summary of weights
summary(weightmodel$ipw.weights)
#plot of weights
ipwplot(weights = weightmodel$ipw.weights, logscale = FALSE,
        main = "weights", xlim = c(0, 22))
mydata$wt<-weightmodel$ipw.weights

#fit a marginal structural model (risk difference)
#svyglm gets the right SEs.
msm <- (svyglm(died ~ treatment, 
               design = svydesign(~ 1, 
                                  weights = ~wt,
                                  data =mydata)))
coef(msm)
confint(msm)

# fit propensity score model to get weights, but truncated
weightmodel<-ipwpoint(exposure= treatment, 
                      family = "binomial", link ="logit",
                      denominator= ~ age + female + meanbp1+ARF+CHF+Cirr+colcan+Coma+lungcan+MOSF+sepsis, 
                      data=mydata,trunc=.01)

#numeric summary of weights
summary(weightmodel$weights.trun)
#plot of weights
ipwplot(weights = weightmodel$weights.trun, logscale = FALSE,
        main = "weights", xlim = c(0, 22))
mydata$wt<-weightmodel$weights.trun
#fit a marginal structural model (risk difference)
msm <- (svyglm(died ~ treatment, design = svydesign(~ 1, 
                                                    weights = ~wt,
                                                    data =mydata)))
coef(msm)
confint(msm)

############################################
################# lalonde ##################
############## IPW #########################
#############################################

data(lalonde) 

lalonde$black<-ifelse(lalonde$race=="black",1,0)
lalonde$hisp<-ifelse(lalonde$race=="hispan",1,0)

# Logit model
psreg <- glm(treat ~ age + educ + black + hisp + 
               married  + re74 + re75, data = lalonde, 
             family = binomial(link = 'logit'))

lalonde <- lalonde %>%
  mutate(prob = predict(psreg, type = 'response')) %>%
  mutate(invwt = treat/prob + (1-treat)/(1-prob)) 

# Weighted least squares estimation 
ipwreg <- lm(re78 ~ treat + age + educ + 
               black + hisp + married  + re74 + re75, data = lalonde, 
             weights = invwt)
summary(ipwreg) 

###########################
#PSM matching.
###########################
#https://scidesign.github.io/designbook/design-of-observational-studies.html#example---epidemiologic-follow-up-study

data(lalonde)

# Estimate the propensity model

lalonde$black<-ifelse(lalonde$race=="black",1,0)
lalonde$hisp<-ifelse(lalonde$race=="hispan",1,0)

glm1  <- glm(treat~age + I(age^2) + educ + I(educ^2) + black +
               hisp + married + re74  + I(re74^2) + re75 + I(re75^2), 
             family=binomial, 
             data=lalonde)

#save data objects

X  <- glm1$fitted
Y  <- lalonde$re78
Tr  <- lalonde$treat

#one-to-one matching with replacement (the "M=1" option).
#Estimating the treatment effect on the treated (the "estimand" option defaults to ATT).

rr  <- Match(Y=Y, Tr=Tr, X=X, M=1);
summary(rr)

# Let's check the covariate balance
# 'nboots' is set to small values in the interest of speed.
# Please increase to at least 500 each for publication quality p-values.  

mb  <- MatchBalance(treat~age + I(age^2) + educ + I(educ^2) 
                    + black + hisp + married + re74  + I(re74^2) + re75 
                    + I(re75^2), 
                    data=lalonde, 
                    match.out=rr, nboots=10)


#########################
#NN matching using match.
#########################

data34 <- tribble(
  ~id,~treat,~score,~income2,
  1,0,73,5,
  2,0,74,4,
  3,0,70,3,
  4,0,69,2,
  5,0,73,1,
  6,1,68,1,
  7,1,68,2,
  8,1,71,3,
  9,1,70,4,
  10,1,72,5
)
data34 = as.data.frame(data34)

#===========.
#using match.
#===========.

xvars<-c("income2")

#smd (pre-matching): treatment vs control.

table1<-CreateTableOne(vars=xvars,strata="treat",
                       data=data34,test=FALSE)
print(table1,smd=TRUE)

#nearest-neighbour matching.

NNMATCH<-Matching::Match(Tr=data34$treat,M=1,X=data34[xvars])
matched<-data34[unlist(NNMATCH[c("index.treated", "index.control")]),]

#ssm on matched data.
matchedTable1<-CreateTableOne(vars=xvars,strata="treat",
                              data=matched,test=FALSE)
print(matchedTable1,smd=TRUE)

#Paired t-test.
#Potential outcomes (means).

y_treated<-matched$score[matched$treat==1]
y_control<-matched$score[matched$treat==0]

#difference for each pair (treatment - control)

ate<-(y_treated) - (y_control)
summary(ate)

#paired t-test.
t.test(ate)

#==========================.
#different way using match.
#==========================.

Y<-data34$score
X<-data34$income2
Tr<-data34$treat

match.ate<-Matching::Match(Y=Y, Tr=Tr, X=X,exact=T,M=1,estimand="ATE")
round(match.ate$est,1)
round(match.ate$est,2)


