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


#####################################################################
#Example using PSW
#psw.wt is used to estimate the weighted treatment effect estimator 
#(without double robustness).
######################################################################

data(test_data)
#Propensity score model
ps <- "Z ~ X1 + X2 + X3 + X4"
tmp <- PSW::psw.wt(data = test_data, weight = "ATE", form.ps = ps,
                   out.var = "Y", family = "gaussian")
tmp

##########################
## PSM: cattaneo2
##########################

data2 <- haven::read_dta("D:/Causal_Inference/Datasets/cattaneo2.dta")
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
fit1

####################
#data
####################

data <- tribble(
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
data = as.data.frame(data)
#model for Tx
prop.model <- glm(treat ~ as.factor(sex) + age, 
                  family = binomial(), data = data)
summary(prop.model)

#Propensity scores for each subject
pscore <- ifelse(data$treat == 0, 
                 1 - predict(prop.model, type = "response"),
                 predict(prop.model, type = "response"))
data<-cbind(data,pscore)
head(data)

#using matching without a pscore.
mb <- Matching::MatchBalance(treat ~ as.factor(sex) + age, data=data,nboots=2)
mb

#using Match (1:1).
a <- glm(treat ~ as.factor(sex) + age, family = binomial(),data = data)
X <- a$fitted
Y <- data$bp
Tr <- data$treat
rr <- Matching::Match(Y=Y,Tr=Tr,X=X,M=1)   # use in MatchBalance #
summary(rr)

#Unadjusted t-test
t.test(data$bp[as.factor(data$treat)==0],
       data$bp[as.factor(data$treat)==1],var.equal=T)

MatchBalance(treat ~ as.factor(sex) + age, data=data,match.out=rr,nboots=2)

#Use PSW library
#Propensity score model
#ps <- "treat ~ as.factor(sex) + age"
#Estimate average treatment effect with "ATE".
#Estimates the weights.
tmp <- PSW::psw.wt(data = data, weight = "ATE", form.ps = treat ~ sex + age,
                   out.var = "bp", family = "gaussian")
summary(tmp)
print(tmp$est.wt)

#=============================.
#Matching via stratification.
#=============================.

prop.model <- glm(treat ~ as.factor(sex) + age, family = binomial(), data = data)
pscore <- predict(prop.model, type = "response")
strat <- quantile(pscore,probs = c(.2,.4,.6,.8))
data$strata<-quantile(pscore,probs = c(.2,.4,.6,.8))
head(data)

strat1 <- pscore<=strat[1]
p.model1 <- glm(bp[strat1]~treat[strat1],data=data)
summary(p.model1)

strat2 <- pscore > strat[1] & pscore <= strat[2] 
p.model2 <- glm(bp[strat2]~treat[strat2],data=data)
summary(p.model2)

strat3 <- pscore > strat[2] & pscore <= strat[3] 
p.model3 <- glm(bp[strat3]~treat[strat3],data=data)
summary(p.model3)

strat4 <- pscore > strat[3] & pscore <= strat[4] 
p.model4 <- glm(bp[strat4]~treat[strat4],data=data)
summary(p.model4)

strat5 <- pscore > strat[4] 
p.model5 <- glm(bp[strat5]~treat[strat5],data=data)
summary(p.model5)

#take wtd average.

#1 [230]: -1.14.
#2 [181]: -2.32
#3 [220]: -0.63.
#4 [202]:  1.21
#5 [167]: 0.045

stratvar <- numeric(length(data$treat))

for (i in 1:length(data$treat)) 
{
  if (strat1[i]==T) {stratvar[i] <- 1}
  else 
    if (strat2[i]==T) {stratvar[i] <- 2}
  else 
    if (strat3[i]==T) {stratvar[i] <- 3}
  else 
    if (strat4[i]==T) {stratvar[i] <- 4}
  else stratvar[i] <- 5
}

stratmodel <- glm(bp~treat + as.factor(stratvar),data=data)
summary(stratmodel)
confint(stratmodel)[2,]

prop.model.adj <- glm(bp ~ treat + pscore, data = data)
summary(prop.model.adj)
confint(prop.model.adj)


#way of creating and adding Pr(Tx).
data2 <- data %>%
  mutate(
    pTx = glm(treat ~ as.factor(sex) + age, 
              data = data, family = "binomial") %>%
      predict(type = "response")
  )


##########################
## PSM: lalonde
##########################

#exact matching.
m.out <- matchit(treat ~ age + race + married + educ, 
                 data = lalonde,
                 method = "exact")
m.out
summary(m.out)  # 54 treated matched with 59 control #

m.out <- matchit(treat ~ age + educ + race + nodegree +
                   married + re74 + re75, data = lalonde,
                 method = "nearest", ratio = 2,
                 min.controls = 1, max.controls = 12)
m.out
summary(m.out) # 185 treated matched with 370 control

# Mahalanobis distance matching - no PS estimated.
m.out <- matchit(treat ~ age + educ + race + married +
                   nodegree + re74 + re75, data = lalonde,
                 distance = "mahalanobis")
summary(m.out) # 185 treated matched with 185 control

#Mahalanobis distance matching with PS estimated
#for use in a caliper; matching done on mahvars
m.out <- matchit(treat ~ age + educ + race + married +
                   nodegree + re74 + re75, data = lalonde,
                 distance = "glm", caliper = .1,
                 mahvars = ~ age + educ + race + married +
                   nodegree + re74 + re75)
summary(m.out) # 111 treated matched with 111 control

# User-supplied propensity scores
pscore <- fitted(glm(treat ~ age + educ + race + married +
                       nodegree + re74 + re75, data = lalonde,
                     family = binomial))
m.out <- matchit(treat ~ age + educ + race + married +
                   nodegree + re74 + re75, data = lalonde,
                 distance = pscore)
summary(m.out) # 185 treated matched with 185 control


#Construct a matched dataset from a matchit object.
#vignette("estimating-effects") for uses of #match.data() and get matches() 
#in estimating treatment effects.

# 2:1 matching w/replacement.
m.out <- matchit(treat ~ age + educ + married +
                   race + nodegree + re74 + re75,
                 data = lalonde, replace = TRUE,
                 caliper = .05, ratio = 4)
summary(m.out) # 180 treated matched with 167 control

#match data.
md <- match.data(m.out, data = lalonde,distance = "prop.score")
dim(md) #one row per matched unit (180 + 167 = 347 rows)
head(md, 10)

#get matches
gm <- get_matches(m.out, data = lalonde,distance = "prop.score")
dim(gm) #multiple rows per matched unit
head(gm, 10)

#vignette("MatchIt") for an introduction to matching with MatchIt; vignette("matching-methods")
#for descriptions of the variety of matching methods and options available; vignette("assessing-balance")
#for information on assessing the quality of a matching specication; vignette("estimating-effects")
#for instructions on how to estimate treatment eects after matching; and vignette("sampling-weights")
#for a guide to using MatchIt with sampling weights.

# Default: 1:1 NN PS matching w/o replacement
m.out <- matchit(treat ~ age + educ + race + nodegree +
                   married + re74 + re75, data = lalonde)
m.out
summary(m.out) # 185 treated matched with 185 control

# 1:1 NN Mahalanobis distance matching w/ replacement and
# exact matching on married and race
m.out <- matchit(treat ~ age + educ + race + nodegree +
                   married + re74 + re75, data = lalonde,
                 distance = "mahalanobis", replace = TRUE,
                 exact = ~ married + race)
m.out
summary(m.out) # 185 treated matched with 78 control


# 2:1 NN Mahalanobis distance matching within caliper defined
# by a logit pregression PS
m.out <- matchit(treat ~ age + educ + race + nodegree +
                   married + re74 + re75, data = lalonde,
                 distance = "glm", link = "logit",
                 mahvars = ~ age + educ + re74 + re75,
                 caliper = .1, ratio = 2)
m.out
summary(m.out) # 111 treated matched with 146 control

# Optimal full PS matching for the ATE within calipers on
# PS, age, and educ
m.out <- matchit(treat ~ age + educ + race + nodegree +
                   married + re74 + re75, data = lalonde,
                 method = "full", estimand = "ATE",
                 caliper = c(.1, age = 2, educ = 1),
                 std.caliper = c(TRUE, FALSE, FALSE))
m.out
summary(m.out)



# Subclassification on a logistic PS with 10 subclasses after
# discarding controls outside common support of PS
mout <- matchit(treat ~ age + educ + race + nodegree +
                  married + re74 + re75, data = lalonde,
                method = "subclass", distance = "glm",
                discard = "control", subclass = 10)
mout
summary(mout) # 185 treated matched with 372 control

#################################
#example
#################################

data <- tribble(
  ~id,~treat,~score,~income2,
  1,	0,	73,	5,
  2,	0,	74,	4,
  3,	0,	70,	3,
  4,	0,	69,	2,
  5,	0,	73,	1,
  6,	1,	68,	1,
  7,	1,	68,	2,
  8,	1,	71,	3,
  9,	1,	70,	4,
  10,	1,	72,	5
)
data
data = as.data.frame(data)
Y<-data$score
X<-data$income2
Tr<-data$treat
match.ate<-Matching::Match(Y=Y, 
                           Tr=Tr, 
                           X=X,
                           exact=T,
                           estimand="ATE")
round(match.ate$est,2)

#Matching on the propensity score.
ps.model<-glm(treat ~ income2, 
              family=binomial,
              data=data,
              na.action=na.exclude)
data$pX=predict(ps.model)

match.ate<-Matching::Match(
  Y=data$score, 
  Tr=data$treat, 
  X=data$pX,
  exact=T,
  estimand="ATE")
print(match.ate)
round(match.ate$est,2)


####################
#### rhc data ######
####################

rhc <- read_excel("D:/Causal_Inference/Datasets/rhc.xlsx")
head(rhc)

## Covariates
vars <- c("age","sex","race","edu","income","ninsclas","cat1","das2d3pc","dnr1",
          "ca","surv2md1","aps1","scoma1","wtkilo1","temp1","meanbp1","resp1",
          "hrt1","pafi1","paco21","ph1","wblc1","hema1","sod1","pot1","crea1",
          "bili1","alb1","resp","card","neuro","gastr","renal","meta","hema",
          "seps","trauma","ortho","cardiohx","chfhx","dementhx","psychhx",
          "chrpulhx","renalhx","liverhx","gibledhx","malighx","immunhx",
          "transhx","amihx")

## Construct a table
tabUnmatched <- CreateTableOne(vars = vars, 
                               strata = "swang1", 
                               data = rhc, 
                               test = FALSE)
## Show table with SMD
print(tabUnmatched, smd = TRUE)

## Count covariates with important imbalance
addmargins(table(ExtractSmd(tabUnmatched) > 0.1))

## Fit model (Logit: Tx ~ Xvars)
rhc$swang1 <- factor(rhc$swang1, levels = c("No RHC", "RHC"))
psModel <- glm(formula = swang1 ~ age + sex + race + edu + income + ninsclas +
                 cat1 + das2d3pc + dnr1 + ca + surv2md1 + aps1 + scoma1 +
                 wtkilo1 + temp1 + meanbp1 + resp1 + hrt1 + pafi1 +
                 paco21 + ph1 + wblc1 + hema1 + sod1 + pot1 + crea1 +
                 bili1 + alb1 + resp + card + neuro + gastr + renal +
                 meta + hema + seps + trauma + ortho + cardiohx + chfhx +
                 dementhx + psychhx + chrpulhx + renalhx + liverhx + gibledhx +
                 malighx + immunhx + transhx + amihx,
               family  = binomial(link = "logit"),
               data=rhc)

## Predicted probability of being assigned to RHC
rhc$pRhc<-predict(psModel, type = "response")
## Predicted probability of being assigned to no RHC
rhc$pNoRhc<-(1 - rhc$pRhc)

## Predicted probability of being assigned to the
## treatment actually assigned (either RHC or no RHC)
rhc$pAssign <- NA
rhc$pAssign[rhc$swang1 == "RHC"]    <- rhc$pRhc[rhc$swang1   == "RHC"]
rhc$pAssign[rhc$swang1 == "No RHC"] <- rhc$pNoRhc[rhc$swang1 == "No RHC"]

## Smaller of pRhc vs pNoRhc for matching weight
rhc$pMin <- pmin(rhc$pRhc, rhc$pNoRhc)

#=========================================
#Propensity score matching (using Match)
#=========================================
listMatch <- Match(Tr       = (rhc$swang1 == "RHC"),      # Need to be in 0,1
                   ## logit of PS,i.e., log(PS/(1-PS)) as matching scale
                   X        = log(rhc$pRhc / rhc$pNoRhc),
                   ## 1:1 matching
                   M        = 1,
                   ## caliper = 0.2 * SD(logit(PS))
                   caliper  = 0.2,
                   replace  = FALSE,
                   ties     = TRUE,
                   version  = "fast")
## Extract matched data
rhcMatched <- rhc[unlist(listMatch[c("index.treated","index.control")]), ]

## Construct a table
tabMatched <- CreateTableOne(vars = vars, strata = "swang1", data = rhcMatched, test = FALSE)
## Show table with SMD
print(tabMatched, smd = TRUE)

## Count covariates with important imbalance
addmargins(table(ExtractSmd(tabMatched) > 0.1))

#================================
#Propensity score matching weight
#=================================

## Matching weight
rhc$mw <- rhc$pMin / rhc$pAssign
## Weighted data
rhcSvy <- svydesign(ids = ~ 1, data = rhc, weights = ~ mw)

## Construct a table (This is a bit slow.)
tabWeighted <- svyCreateTableOne(vars = vars, strata = "swang1", data = rhcSvy, test = FALSE)
## Show table with SMD
print(tabWeighted, smd = TRUE)

# Count covariates with important imbalance
addmargins(table(ExtractSmd(tabWeighted) > 0.1))

#==============================
#Propensity score overlap weight
#================================

# Overlap weight
rhc$ow <- (rhc$pAssign * (1 - rhc$pAssign)) / rhc$pAssign
## Weighted data
rhcSvyOw <- svydesign(ids = ~ 1, data = rhc, weights = ~ ow)

## Construct a table (This is a bit slow.)
tabWeightedOw <- svyCreateTableOne(vars = vars, strata = "swang1", data = rhcSvyOw, test = FALSE)
## Show table with SMD
print(tabWeightedOw, smd = TRUE)

## Count covariates with important imbalance
addmargins(table(ExtractSmd(tabWeightedOw) > 0.1))

#=====================================================
#Assessing balance before and after matching/weighting
#=====================================================

## Construct a data frame containing variable name and SMD from all methods
dataPlot <- data.frame(variable  = rownames(ExtractSmd(tabUnmatched)),
                       Unmatched = as.numeric(ExtractSmd(tabUnmatched)),
                       Matched   = as.numeric(ExtractSmd(tabMatched)),
                       Weighted  = as.numeric(ExtractSmd(tabWeighted)),
                       WeightedOw = as.numeric(ExtractSmd(tabWeightedOw)))

## Create long-format data for ggplot2
dataPlotMelt <- reshape2::melt(data          = dataPlot,
                               id.vars       = c("variable"),
                               variable.name = "Method",
                               value.name    = "SMD")



## Order variable names by magnitude of SMD
varNames <- as.character(dataPlot$variable)[order(dataPlot$Unmatched)]

## Order factor levels in the same order
dataPlotMelt$variable <- factor(dataPlotMelt$variable,
                                levels = varNames)

## Plot using ggplot2
ggplot(data = dataPlotMelt,
       mapping = aes(x = variable, y = SMD, group = Method, color = Method)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() +
  theme_bw() + theme(legend.key = element_blank())

## Column bind tables
resCombo <- cbind(print(tabUnmatched, printToggle = FALSE),
                  print(tabMatched,   printToggle = FALSE),
                  print(tabWeighted,  printToggle = FALSE),
                  print(tabWeightedOw,  printToggle = FALSE))

## Add group name row, and rewrite column names
resCombo <- rbind(Group = rep(c("No RHC","RHC"), 3), resCombo)
colnames(resCombo) <- c("Unmatched","","Matched","","MW","","OW","")
print(resCombo, quote = FALSE)

#===============
#Outcome analysis
#===============

## Unmatched model (unadjusted)
glmUnmatched <- glm(formula = (death == "Yes") ~ swang1,
                    family  = binomial(link = "logit"),
                    data    = rhc)

## Matched model
glmMatched <- glm(formula = (death == "Yes") ~ swang1,
                  family  = binomial(link = "logit"),
                  data    = rhcMatched)

## Weighted model
glmWeighted <- svyglm(formula = (death == "Yes") ~ swang1,
                      family  = binomial(link = "logit"),
                      design    = rhcSvy)

## Show results together
resTogether <- list(Unmatched = ShowRegTable(glmUnmatched, printToggle = FALSE),
                    Matched   = ShowRegTable(glmMatched, printToggle = FALSE),
                    Weighted  = ShowRegTable(glmWeighted, printToggle = FALSE))
print(resTogether, quote = FALSE)

#====================================================.
#MatchIt.
#https://rdrr.io/cran/MatchIt/f/vignettes/MatchIt.Rmd
#====================================================.

#Introduction
#MatchIt implements the suggestions of Ho, Imai, King, and Stuart 
#for improving parametric statistical models for 
#estimating treatment effects in observational studies 
#and reducing model dependence by preprocessing data with 
#semi-parametric and non-parametric matching methods. 
#After appropriately preprocessing with MatchIt, 
#researchers can use whatever parametric model they would have 
#used without MatchIt and produce inferences that are more 
#robust and less sensitive to modeling assumptions. 
#MatchIt reduces the dependence of causal inferences on 
#commonly made, but hard-to-justify, statistical 
#modeling assumptions using a large range of sophisticated matching 
#methods. The package includes several popular 
#approaches to matching and provides access to methods 
#implemented in other packages through its single, 
#unified, and easy-to-use interface.

#Matching is used in the context of estimating the causal effect of a 
#binary treatment or exposure on an outcome while controlling 
#for measured pre-treatment variables, 
#typically confounding variables or variables prognostic of the outcome. 
#Here and throughout the MatchIt documentation we use the word 
#"treatment" to refer to the focal causal variable of interest, 
#with "treated" and "control" reflecting the names of the treatment groups. 

#The goal of matching is to produce covariate balance, that is, 
#for the distributions of covariates in the two groups to be 
#approximately equal to each other, as they would be in a successful 
#randomized experiment. 
#The importance of covariate balance is that it allows 
#for increased robustness to the choice of model 
#used to estimate the treatment effect; 
#in perfectly balanced samples, a simple difference in means 
#can be a valid treatment effect estimate. 
#Here we do not aim to provide a full introduction 
#to matching or causal inference theory, 
#but simply to explain how to use MatchIt to perform 
#nonparametric preprocessing. 

#A matching analysis involves four primary steps: 
#1) planning, 
#2) matching, 
#3) assessing the quality of matches, 
#4) estimating the treatment effect and its uncertainty. 

#Here we briefly discuss these steps and how they can be 
#implemented with MatchIt; 
#in the other included vignettes, these steps are discussed in more detail.
#We will use Lalonde's data on the evaluation of the National 
#Supported Work program to demonstrate MatchIt's capabilities. 

data("lalonde")
head(lalonde)

#The statistical quantity of interest is the causal effect of the 
#treatment (treat) on 1978 earnings (re78). 
#The other variables are pre-treatment covariates. 
#In particular, the analysis is concerned with the 
#marginal, total effect of the treatment for those 
#who actually received the treatment.

#In what follows, we briefly the four steps of a matching 
#analysis and how to implement them in MatchIt. 
#For more details, we recommend reading the other vignettes, 
#vignette("matching-methods"), 
#vignette("assessing-balance"), 
#vignette("estimating-effects"), 
#For the use of MatchIt with sampling weights, 
#also see vignette("sampling-weights"). 

#It is important to recognize that the ease of using MatchIt 
#does not imply the simplicity of matching methods; 
#advanced statistical methods like matching that require 
#many decisions to be made and caution in their use should 
#only be performed by those with statistical training.

#Planning
#The planning phase of a matching analysis involves 
#selecting the type of effect to be estimated, 
#selecting the target population to which the treatment effect is to generalize, 
#and selecting the covariates for which balance is required for 
#an unbiased estimate of the treatment effect. 
#Each of these are theoretical steps that do not involve performing 
#analyses on the data. Ideally, they should be considered prior 
#to data collection in the planning stage of a study. 
#Thinking about them early can aid in performing a complete and 
#cost-effective analysis.

#Selecting the type of effect to be estimated. 
#There are a few different types of effects to be estimated. 
#In the presence of mediating variables, one might be interested in the 
#direct effect of the treatment that does not pass through the 
#mediating variables or the total effect of the treatment across 
#all causal pathways. 

#Matching is well suited for estimating total effects, and 
#specific mediation methods may be better suited for other 
#mediation-related quantities. 

#One may be interested in a conditional effect or a marginal effect. 
#A conditional effect is the effect of a treatment within some strata of 
#other prognostic variables (e.g., at the patient level), 

#A marginal effect is the average effect of a treatment in a population 
#(e.g., for implementing a broad policy change). 

#Different types of matching are well suited for each of these, 
#but the most common forms are best used for estimating 
#marginal treatment effects; 

#For conditional treatment effects, typically modeling assumptions 
#are required or matching must be done within strata of 
#the conditioning variables. Matching can reduce the reliance on 
#correct model specification for conditional effects.

#Selecting a target population. 
#The target population is the population to which the 
#effect estimate is to generalize. 
#Typically, an effect estimated in a sample generalizes to the 
#population from which the sample is a probability sample. 
#If the sample is not a probability sample from any population 
#(e.g., it is a convenience sample or involves patients from an 
#arbitrary hospital), the target population can be unclear. 
#Often, the target population is a group of units who are 
#eligible for the treatment (or a subset thereof). 
#Causal estimands are defined by the target population to which they 
#generalize.

#The average treatment effect in the population (ATE) 
#is the average effect of the treatment for all units in the 
#target population. 

#The average treatment effect in the treated (ATT) is the average 
#effect of the treatment for units like those who actually were treated. 

#The most common forms of matching are best suited for estimating the ATT, 
#though some are also available for estimating the ATE. 

#Some matching methods distort the sample in such a way that the 
#estimated treatment effect corresponds neither to the ATE nor to the ATT, 
#but rather to the effect in an unspecified population 
#(sometimes called the ATM, or average treatment effect in the remaining 
#matched sample). 

#When the target population is not so important 
#(e.g., in the case of treatment effect discovery), 
#such methods may be attractive; otherwise, care should be taken in 
#ensuring the effect generalizes to the target population of interest. 

#Different matching methods allow for different target populations, 
#so it is important to choose a matching method that allows one to 
#estimate the desired effect.

#Selecting covariates to balance. 
#Selecting covariates carefully is critical for ensuring the 
#resulting treatment effect estimate is free of confounding 
#and can be validly interpreted as a causal effect. 

#To estimate total causal effects, all covariates must be measured 
#prior to treatment (or otherwise not be affected by the treatment). 
#Covariates should be those that cause variation in the outcome and 
#selection into treatment group; these are known as confounding variables. 
#Ideally these covariates are measured without error 
#and are free of missingness.

#Check Initial Imbalance
#After planning and prior to matching, it can be a good 
#idea to view the initial imbalance in one's data that 
#matching is attempting to eliminate. 

# No matching; constructing a pre-match matchit object
m.out0 <- matchit(treat ~ age + educ + race + married + nodegree 
                  + re74 + re75, 
                  data = lalonde,
                  method = NULL, distance = "glm")

#The first argument is a formula relating the treatment to the covariates 
#used in estimating the propensity score and for which balance 
#is to be assessed. 
#Typically, the method argument specifies the method of matching 
#to be performed; here, we set it to NULL so we can assess 
#balance prior to matching. 
#The distance argument specifies the method for estimating the 
#propensity score:
#this is a one-dimensional summary of all the included covariates, 
#computed as the predicted probability of being the treated group 
#given the covariates; 
#here, we set it to "glm" for generalized linear model, 
#which implements logistic regression by default

#Note that the default for method is "nearest" 
#to perform nearest neighbor matching. 
#To prevent any matching from taking place in order to assess 
#pre-matching imbalance, method must be set to NULL.

#Note that setting distance = "logit", which was the default in 
#MatchIt version prior to 4.0.0, 
#will also estimate logistic regression propensity scores. 
#Because it is the default, the distance argument can 
#actually be omitted if logistic regression propensity scores are desired.

#Below we assess balance on the unmatched data using summary():

# Checking balance prior to matching
summary(m.out0)

#We can see severe imbalances as measured by the 
#standardized mean differences (Std. Mean Diff.), 
#variance ratios (Var. Ratio), and 
#empirical cumulative density function (eCDF) statistics. 
#Values of smd and eCDF statistics close to zero and 
#values of variance ratios close to one indicate good balance, 
#and here many of them are far from their ideal values.

#Matching
#Now, matching can be performed. 
#There are several different classes and methods of matching, 
#described in vignette("matching-methods"). 
#Here, we begin by briefly demonstrating 1:1 NNM on the propensity score, 
#which is appropriate for estimating the ATT. 
#One by one, each treated unit is paired with an available control unit 
#that has the closest propensity score to it. 
#Any remaining control units are left unmatched and excluded 
#from further analysis. 
#Due to the theoretical balancing properties of the propensity score 
#described by Rosenbaum and Rubin, PSM can be an 
#effective way to achieve covariate balance in the treatment groups. 
#Below we demonstrate the use of matchit() to perform 
#nearest neighbor propensity score matching.

# 1:1 NN PS matching w/o replacement
m.out1 <- matchit(treat ~ age + educ + race + married + 
                    nodegree + re74 + re75, 
                  data = lalonde,
                  method = "nearest", 
                  distance = "glm")

#We use the same syntax as before, but this time specify 
#method = "nearest" to implement nearest neighbor matching, 
#again using a logistic regression propensity score. 
#Many other arguments are available for tuning the matching method 
#and method of propensity score estimation.

#The matching outputs are contained in the m.out1 object. 
#Printing this object gives a description of the type of matching performed:

m.out1

#The key components of the m.out1 object are:
#weights (the computed matching weights), 
#subclass (matching pair membership), 
#distance (the estimated propensity score), and 
#match.matrix (which control units are matched to each treated unit). 

#How these can be used for estimating the effect of the treatment 
#after matching is detailed in vignette("estimating-effects").

#Assessing the Quality of Matches
#Although matching on the propensity score is often effective 
#at eliminating differences between the treatment groups 
#to achieve covariate balance, 
#its performance in this regard must be assessed. 
#If covariates remain imbalanced after matching, the matching is 
#considered unsuccessful, and a different matching specification 
#should be tried. MatchIt offers a few tools for the assessment 
#of covariate balance after matching. 
#These include graphical and statistical methods. 
#More detail on the interpretation of the included plots and statistics 
#can be found in vignette("assessing-balance").

#In addition to covariate balance, the quality of the match 
#is determined by how many units remain after matching. 
#Matching often involves discarding units that are not 
#paired with other units, and some matching options, 
#such as setting restrictions for common support or calipers, 
#can further decrease the number of remaining units. 
#If, after matching, the remaining sample size is small, 
#the resulting effect estimate may be imprecise. In many cases, 
#there will be a trade-off between balance and remaining sample size. 
#How to optimally choose among them is an instance of the 
#fundamental bias-variance trade-off problem that cannot be 
#resolved without substantive knowledge of the phenomena under study. 
#Prospective power analyses can be used to determine how 
#small a sample can be before necessary precision is sacrificed.

#To assess the quality of the resulting matches numerically, 
#we can use the summary() function on m.out1 as before. 
#Here we set un = FALSE to suppress display of the balance 
#before matching for brevity and because we already saw it. 
#(Leaving it as TRUE, its default, would display balance 
#both before and after matching.)

# Checking balance after NN matching
summary(m.out1, un = FALSE)

#At the top is a summary of covariate balance after matching. 
#Although balance has improved for some covariates, 
#in general balance is still quite poor, 
#indicating that nearest neighbor propensity score matching is 
#not sufficient for removing confounding in this dataset. 
#The final column, Std. Pair Diff, displays the average absolute 
#within-pair difference of each covariate. 
#When these values are small, better balance is typically achieved 
#and estimated effects are more robust to misspecification of the 
#outcome model

#Next is a table of the sample sizes before and after matching. 
#The matching procedure left 244 control units unmatched. 
#Ideally, unmatched units would be those far from the treated units 
#and would require greater extrapolation were they to have been retained. 
#We can visualize the distribution of propensity scores of those who 
#were matched using plot() with type = "jitter":

plot(m.out1, type = "jitter", interactive = FALSE)

#We can visually examine balance on the covariates using 
#plot() with type = "qq":

plot(m.out1, type = "qq", 
     interactive = FALSE,
     which.xs = c("age", "married", "re75"))


#Points far from the solid diagonal line are the areas of the 
#covariate distributions that differ between the treatment groups. 
#Although married and re75 appear to have improved balance after matching, 
#the case is mixed for age.

#Trying a Different Matching Specification
#Given the poor performance of NNM in this example, 
#we can try a different matching method 
#or make other changes to the matching algorithm or distance specification. 
#Below, we'll try full matching, which matches every treated unit 
#to at least one control and every control to at least one treated unit 
#We'll also try a different link (probit) for the propensity score model.

# Full matching on a probit PS
m.out2 <- matchit(treat ~ age + educ + race + married + 
                    nodegree + re74 + re75, data = lalonde,
                  method = "full", 
                  distance = "glm", 
                  link = "probit")
m.out2

#We can examine balance on this new matching specification.
# Checking balance after full matching
summary(m.out2, un = FALSE)

#Balance is far better, as determined by the lower smd and eCDF statistics. 
#The balance should be reported when publishing the results 
#of a matching analysis. 
#This can be done either in a table, using the values resulting 
#from summary(), or in a plot, such as a Love plot, 
#which we can make by calling plot() on the summary() output:

plot(summary(m.out2))

#Love plots are a simple and straightforward way to summarize 
#balance visually. 
#See vignette("assessing-balance") for more information on 
#how to customize MatchIt's Love plot and how to use cobalt, 
#a package designed specifically for balance assessment and 
#reporting that is compatible with MatchIt.

#Estimating the Treatment Effect
#How treatment effects are estimated depends on what form of 
#matching was performed. 
#See vignette("estimating-effects") for information on the variety 
#of ways to estimate effects and standard errors after each type of 
#matching and for several outcome types. 

#After 1:1 matching without replacement, we can run a 
#simple regression of the outcome on the treatment in the 
#matched sample (i.e., including the matching weights). 
#With continuous outcomes, it is often a good idea to also 
#include the covariates used in the matching in the effect estimation, 
#as doing so can provide additional robustness to slight 
#imbalances remaining after the matching and can improve precision.

#Even though the 1:1 matching was not successful, 
#we'll demonstrate here how to estimate a treatment effect 
#after performing such an analysis. 
#First, we'll extract the matched dataset from the matchit object 
#using match.data(). This dataset only contains the matched units 
#and adds columns for distance, weights, and subclass.

m.data1 <- match.data(m.out1)
head(m.data1)

#We can then estimate a treatment effect in this dataset 
#using the standard regression functions in R, 
#like lm() or glm(), being sure to include the matching weights 
#(stored in the weights variable of the match.data() output) 
#in the estimation. We recommend using cluster-robust 
#standard errors for most analyses, with pair membership 
#as the clustering variable; the lmtest and sandwich packages 
#together make this straightforward.

#With 1:1 nearest neighbor matching without replacement, 
#excluding the matching weights does not change the estimates. 
#For all other forms of matching, they are required, 
#so we recommend always including them for consistency.

library("lmtest") #coeftest
library("sandwich") #vcovCL

fit1 <- lm(re78 ~ treat + age + educ + race + married + nodegree + 
             re74 + re75, 
           data = m.data1, 
           weights = weights)
lmtest::coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)

#The coefficient on treat is the estimated ATT. 
#The other coefficients and tests should not be interpreted or reported. 
#Estimating standard errors with matched data is an area of 
#ongoing development. Generally, the approach demonstrated above 
#works well for continuous outcomes. See vignette("estimating-effects") 
#for more information on how to estimate standard errors 
#with each type of matching and with different outcome types.

#A benefit of matching is that the outcome model used to estimate the 
#treatment effect is robust to misspecification when 
#balance has been achieved. 

#With 1:1 NNM, we failed to achieve balance, so one should be cautious 
#about trusting the estimated effect. 
#With full matching, we were able to achieve balance, 
#so the effect estimate should depend less on the form of the outcome model 
#used. Below we estimate the effect and standard error of the treatment 
#effect after full matching. As before, we'll use functions from the 
#lmtest and sandwich packages here because they provide a fairly 
#general interface to estimating coefficients and standard errors.

m.data2 <- match.data(m.out2)

fit2 <- lm(re78 ~ treat + age + educ + race + married + nodegree + 
             re74 + re75, 
           data = m.data2, 
           weights = weights)
coeftest(fit2, vcov. = vcovCL, cluster = ~subclass)

#Given the results of these two estimates, we would be inclined 
#to trust the one resulting from the second analysis, 
#i.e., using full matching, because better balance was achieved on 
#all the variables, making the effect estimate less sensitive to 
#the form of the outcome model we used.

#Effect estimation with nonlinear models (e.g., for binary or 
#time-to-event event outcomes) is more complicated due to 
#noncollapsibility of the estimated effects; including additional 
#covariates in the model can change the meaning of the estimated effect. 
#See vignette("estimating-effects") for more details. 
#Note that for some models, effect and standard error estimation 
#is still being researched.

#Reporting Results
#To report matching results in a manuscript or research report, 
#a few key pieces of information are required. 
#One should be as detailed as possible about the matching procedure 
#and the decisions made to ensure the analysis is replicable and 
#can be adequately assessed for soundness by the audience. 
#Key pieces of information to include are 
#1) the matching specification used (including the method and any 
#additional options, like calipers or common support restrictions), 
#2) the distance measure used (including how it was estimated 
#e.g., using logistic regression for propensity scores), 
#3) which other matching methods were tried prior to settling on a 
#final specification and how the choices were made, 
#4) the balance of the final matching specification (including 
#standardized mean differences and other balance statistics for the 
#variables, their powers, and their interactions; 
#some of these can be reported as summaries rather than in full detail), 
#5) the number of matched, unmatched, and discarded units included in the 
#effect estimation, and 
#6) the method of estimating the treatment effect and standard error or 
#confidence interval (including the specific model used and the 
#specific type of standard error). 

#We used propensity score matching to estimate the average 
#marginal effect of the treatment on 1978 earnings on those who received 
#it accounting for confounding by the included covariates. 
#We first attempted 1:1 nearest neighbor propensity score matching 
#without replacement with a propensity score estimated using 
#logistic regression of the treatment on the covariates. 
#This matching yielded poor balance, so we instead tried 
#full matching on the propensity score, which yielded adequate balance, 
#as indicated in Table 1 and Figure 1. The propensity score was 
#estimated using a probit regression of the treatment on the covariates, 
#which yielded better balance than did a logistic regression. 
#After matching, all standardized mean differences for the covariates 
#were below 0.1 and all standardized mean differences for squares 
#and two-way interactions between covariates were below .15, 
#indicating adequate balance. Full matching uses all treated and 
#all control units, so no units were discarded by the matching.
#To estimate the treatment effect and its standard error, 
#we fit a linear regression model with 1978 earnings as the outcome 
#and the treatment and the covariates as additive predictors and 
#included the full matching weights in the estimation. 
#The coefficient on the treatment was taken to be the estimate of 
#the treatment effect. 
#The lm() function was used to estimate the effect, 
#and a cluster-robust variance as implemented in the vcovCL() function 
#in the sandwich package was used to estimate its standard error 
#with matching stratum membership as the clustering variable.

#The estimated effect was \$1980 (SE = 756.1, p = .009), 
#indicating that the average effect of the treatment for 
#those who received it is to increase earnings.

#Conclusion
#Although we have covered the basics of performing a matching analysis 
#here, to use matching to its full potential, 
#the more advanced methods available in MatchIt should be considered. 
#We recommend reading the other vignettes included 
#here to gain a better understand of all the MatchIt has to offer 
#and how to use it responsibly and effectively. 
#As previously stated, the ease of using MatchIt does not imply that 
#matching or causal inference in general are simple matters; 
#matching is an advanced statistical technique that should be 
#used with care and caution. 
#We hope the capabilities of MatchIt ease and encourage the use of 
#nonparametric preprocessing for estimating causal effects in a robust 
#and well-justified way.
