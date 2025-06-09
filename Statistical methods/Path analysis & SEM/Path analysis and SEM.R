##########################.
###Path analysis and SEM
##########################.

getwd() 
.libPaths()
print(R.version.string)

Macdrive<-
  "/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/"
UCLdrive<-
  "C:/Users/rmjdshc/OneDrive - University College London/Learning/"
folder<-"Path Analysis and SEM/"  
#setwd(paste0(UCLdrive, folder))
setwd(paste0(Macdrive, folder))

library(tidyverse)
library(tibble)
library(dplyr)
library(lavaan)
library(haven)
library(mediation)
library(psych)
library(semTools)
library(bda)

#Introduction

#Factor analysis is used mostly for data reduction purposes. 
#To obtain a smaller set of variables (preferably uncorrelated) from a 
#larger set of variables (most of which are correlated to each other). 
#To create indexes with variables that measure 
#similar things (conceptually) (e.g. wealth index).

#Exploratory FA ~ When you do not have a pre-defined idea of the structure 
#or of how many dimensions are in a set of variables.

#Confirmatory FA ~ When you want to test specific hypotheses about the 
#structure or test the number of dimensions underlying a set of variables.

#Create an index: allow each observed item to have its own unique variance 
#and develop a scale that represents the shared meaning of the set of 
#items on a single dimension (i.e. CFA). 
#The alpha coefficient is a measure of internal consistency. 
#A good alpha does not ensure that a single dimension is being tapped. 
#Alpha is insufficient: investigate the number of dimensions 
#via Factor Analysis.

#Principal components.
#PCFA analyses the correlation matrix (each item standardized to 
#have a variance of 1).
data2<-haven::read_dta("data/nlsy97cfa2.dta")
data2$conserv=rowMeans(data2[,c("x1", "x2", "x3", "x4", "x5", 
                                "x6", "x7","x8","x9")], na.rm=TRUE)
pca <- psych::principal(data2, nfactors=1, rotate="varimax",
                 center = TRUE,scale. = TRUE,scores=TRUE)
pca
head(pca$scores)

# Single-factor CFA using lavaan.
data<-haven::read_dta("data/nlsy97cfa.dta")
M1<- 'Cons =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9'
fit<- lavaan::cfa(M1, data=data)
lavaan::summary(fit, standardized = TRUE)
semTools::reliability(fit)
summary(fit, fit.measures=TRUE)
parameterEstimates(fit,ci=FALSE,standardized=TRUE)
inspect(fit,what="std")$lambda
inspect(fit,what="est")$lambda
semTools::compRelSEM(fit)

### Extract factor scores
data2<-haven::read_dta("data/nlsy97cfa2.dta")
M1<- 'Cons =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9'
fit<- lavaan::cfa(M1, data=data2)
data2$Fscore<-lavPredict(fit)
head(data2$Fscore)

#Single-factor CFA (residual correlation)
M2 <- '
#Measurement Model
Cons =~ x1 + x3 + x4 + x5 + x6 + x7 + x9
#Residual correlations
x3 ~~ x4'
fit <- lavaan::cfa(M2, data=data)
summary(fit, standardized = TRUE)  # std.all column
##works for factors with exclusively continuous OR categorical indicators
semTools::reliability(fit)
semTools::compRelSEM(fit)

# Two-factor CFA.
M3 <- '
#Measurement Model
Cons =~ x1 + x3 + x4 + x5 + x6 + x7 + x9
Depress =~ x11 + x12 + x13 
#Residual correlations
x3 ~~ x4'
fit <- lavaan::cfa(M3, data=data)
summary(fit, standardized = TRUE)  # std.all column
lavaan::fitMeasures(fit, c("cfi","rmsea"))
b<-lavaan::modindices(fit, sort = TRUE, maximum.number = 5)
b

### Simple measurement model (1-factor CFA)
data<-haven::read_dta("data/sem_1fmm.dta")
M1<- 'X =~ 1*x1 + x2 + x3 + x4'
fit<- lavaan::cfa(M1, data=data)
inspect(fit,what="est")$lambda
M2<- 'X =~ 2*x1 + x2 + x3 + x4'
fit<- lavaan::cfa(M2, data=data)
inspect(fit,what="est")$lambda
lavaan::fitMeasures(fit, c("chisq","baseline.chisq","cfi","rmsea"))

M2<- 'X =~ 2*x1 + x2 + x3 + x4
#Residual correlations
x2 ~~ x3'
fit<- lavaan::cfa(M2, data=data)
inspect(fit,what="est")$lambda
lavaan::fitMeasures(fit, c("chisq","baseline.chisq","cfi","rmsea"))

#estimate but constrain loadings.
M3<- 'X =~ 1*x1 + t*x2 + t*x3 + x4
#Residual correlations
x2 ~~ x3'
fit<- lavaan::cfa(M3, data=data)
inspect(fit,what="est")$lambda
lavaan::fitMeasures(fit, c("chisq","baseline.chisq","cfi","rmsea"))

## Two-factor CFA
lower <- '
2038.1 
1631.1 1932.1  
1393.1 1336.1 1313.1 
1657.1 1647.1 1273.1 2034.1  
1721.1 1688.1 1498.1 1677.1 2061.1 
659.1 702.1 585.1 656.1 775.1 630.1  
779.1 790.1 653.1 764.1 871.1 500.1 741.1  
899.1 879.1 750.1 897.1 1008.1 648.1 679.1 1087.1  
775.1 739.1 659.1 751.1 895.1 550.1 603.1 677.1 855.1 
821.1 785.1 669.1 802.1 877.1 491.1 574.1 686.1 622.1 728.1 '
M.cov <-
  getCov(lower, names = c("a1", "a2", "a3", "a4", "a5", 
                          "c1", "c2","c3", "c4","c5"))
M.cov

M <- '
# latent variables
Affective =~ a1 + a2 + a3 + a4 + a5
Cognitive =~ c1 + c2 + c3 + c4 + c5'
fit <- sem(M,
           sample.cov = M.cov,
           sample.nobs = 216)
summary(fit, standardized = FALSE)
mi<-lavaan::modindices(fit, sort = TRUE, maximum.number = 5)
mi

M1 <- '
# latent variables
Affective =~ a1 + a2 + a3 + a4 + a5
Cognitive =~ c1 + c2 + c3 + c4 + c5
Affective ~~ 0*Cognitive '
fit <- sem(M1,
           sample.cov = M.cov,
           sample.nobs = 216)
summary(fit, standardized = FALSE)

#constraints.
M1 <- '
# latent variables
Affective =~ a1 + b*a2 + a3 + a4 + a5
Cognitive =~ c1 + b*c2 + c3 + c4 + c5'
fit <- sem(M1,
           sample.cov = M.cov,
           sample.nobs = 216)
summary(fit, standardized = FALSE)


#OLS regression via sem.
data<-haven::read_dta("data/moderation_in_sem.dta")
fit <- lm(outcome ~ income + sex + (income*sex), data = data)
data$sex<-ifelse(data$sex == 2, 1, 0)
data$inter<-data$income*data$sex
fit <- lm(outcome ~ income + sex + inter, data = data)
M <- ' outcome ~ 1 + income + sex + inter'
fit <- lavaan::sem(M, data = data)
coef(fit)

#OLS regression via sem.
set.seed(1234)
Data <- data.frame(y = rnorm(100),
                   x1 = rnorm(100),
                   x2 = rnorm(100),
                   x3 = rnorm(100))
M <- ' y ~ 1 + b1*x1 + b2*x2 + b3*x3 '
fit <- lavaan::sem(M, data = Data)
coef(fit)

## Path analysis 
#Path analysis is used to estimate a system of equations in which all of the 
#variables are observed. Unlike models that include latent variables, 
#path models assume perfect measurement of the observed variables: only the 
#structural relationships between the observed variables are modelled. 
#This type of model is often used when one or more variables is thought to 
#mediate the relationship between two others (mediation models). 
#Similar model setups can be used to estimate models where the relationship 
#between variables is thought to vary across groups (multiple group models).

#Mediation via OLS regression: If variable Z is on the causal 
#pathway between X and Y:
#(1) Adjust for Z: Only gives you the direct effect of X on Y 
#(not mediated by Z)
#(2) Do Not Adjust for Z: Estimate the total effect of X on Y:
#Total effect = Direct effect (not mediated by intervening variable) plus the 
#indirect effect (mediated by intervening variable).

### Mediation using path.dta
data<-haven::read_dta("data/path.dta")
#exposure = attention4; mediation through math7 and read7
#N=338 (complete cases)
M1 <- ' 
# direct effect
math21 ~ a*attention4 + b*math7 + c*read7
# mediators (math7 and read7) 
math7 ~ d*attention4
read7 ~ e*attention4
# indirect effect (a*b)
indirect := d*b + e*c
# total effect
total := a + ((d*b) + (e*c))
'
fit <- lavaan::sem(M1, data = data,estimator = "ML")
summary(fit)
summary(fit,standardized = TRUE)

#Allow for correlation between errors: parallel mediators.
M2 <- ' 
# direct effect
math21 ~ a*attention4 + b*math7 + c*read7
# mediator
math7 ~ d*attention4
read7 ~ e*attention4
#Residual correlations
math7 ~~ read7
# indirect effect (a*b)
indirect := d*b + e*c
# total effect
total := a + ((d*b) + (e*c))
'
fit <- lavaan::sem(M2, data = data)
summary(fit,standardized = TRUE)

#Add variables: don't need to specify coefficients for those.
#complete cases.
M3 <- ' 
# direct effect
math21 ~ a*attention4 + b*math7 + c*read7 + vocab4 + adopted + male + momed
# mediator
math7 ~ d*attention4 + vocab4 + adopted + male + momed
read7 ~ e*attention4 + vocab4 + adopted + male + momed
#Residual correlations
math7 ~~ read7
# indirect effect of attention 4 (a*b)
indirect := d*b + e*c
# total effect of attention 4
total := a + ((d*b) + (e*c))
'
fit <- lavaan::sem(M3, data = data)
summary(fit,standardized = TRUE)


#SEM with missing data
#N=430 method(mlmv) in Stata
fit2 <- lavaan::sem(M1, data = data, missing="ml")
summary(fit2,standardized = TRUE)
fit2 <- lavaan::sem(M1, data = data, estimator="MLMV")
summary(fit2,standardized = TRUE)

###########################
#cross-lagged panel
###########################

#A cross-lagged panel model (CLPM) is a type of SEM used to analyze the 
#reciprocal relationships between two or more variables over time.
#Key Concepts: CLPM requires at least two time points (T1, T2) and usually more. 
#It's used when you collect repeated measurements from the same subjects.
#Variables of Interest: Typically, two or more variables are measured at each 
#time point (e.g., X and Y at T1 and T2).
#Main Goals:

#Stability (autoregressive) paths: 
#To assess how a variable predicts itself over time (e.g., X at T1 → X at T2).

#Cross-lagged paths: To assess how one variable predicts change in another 
#variable over time (e.g., X at T1 → Y at T2, and Y at T1 → X at T2).
#Controlling for prior levels: The cross-lagged effects help infer potential 
#causal directionality while accounting for earlier levels of both variables.

#This example uses simulated data to model the cross-lagged relationship 
#between two variables over two time points.

set.seed(123)
n <- 300  
# Simulate T1 variables
x1 <- rnorm(n, mean = 50, sd = 10)
y1 <- 0.5 * x1 + rnorm(n, mean = 0, sd = 5)
# Simulate T2 variables with autoregressive and cross-lagged effects
x2 <- 0.6 * x1 + 0.3 * y1 + rnorm(n, mean = 0, sd = 5)
y2 <- 0.4 * y1 + 0.2 * x1 + rnorm(n, mean = 0, sd = 5)
# Combine into a data frame
df_clpm <- data.frame(x1, y1, x2, y2)

model <- '
  # Autoregressive paths
  x2 ~ a1 * x1
  y2 ~ b1 * y1

  # Cross-lagged paths
  x2 ~ c1 * y1
  y2 ~ d1 * x1

  # Correlations at T1 and T2
  x1 ~~ y1
  x2 ~~ y2
'

fit <- lavaan::sem(model, data = df_clpm)
summary(fit, fit.measures = TRUE, standardized = TRUE)

#Look specifically at:
#x2 ~ y1 (cross-lagged effect of y1 on x2)
#y2 ~ x1 (cross-lagged effect of x1 on y2)

###########################
#cross-lagged panel (N=336)
###########################

M <- ' 
math21 ~ math7 + read7
read21 ~ math7 + read7
#Residual correlations
math21 ~~ read21
math7 ~~ read7 
'
fit <- lavaan::sem(M, data = data)
summary(fit,standardized = TRUE)

###########################
# Path analysis using HSE.
###########################

data <- haven::read_dta("data/practical08v2.dta")
data = as.data.frame(data)
#Brackets not allowed.
#BMI as outcome.
s1<-'
genhelf ~ a*hrs10spt + f*ghq12scr
ghq12scr ~ d*hrs10spt
bmival ~ c*hrs10spt + e*ghq12scr + b*genhelf
direct_hrs10 :=c
direct_ghq :=e
direct_genhelf :=b
indirect_hrs10 :=(a*b) + (d*e) + (d*f*b)
total_hrs10 :=c + (a*b) + (d*e) + (d*f*b)
'
M1<-lavaan::sem(s1, data=data)
summary(M1)
standardizedSolution(M1, type="std.all")

#Measurement Model (=~) and path analysis (SEM).
s2<-'
Distress =~ ghqconc + ghqconfi + ghqdecis + ghqenjoy + ghqface + ghqhappy + 
ghqover + ghqsleep + ghqstrai + ghqunhap + ghquse + ghqworth
genhelf ~ a*hrs10spt + f*Distress
Distress ~ d*hrs10spt 
bmival ~ c*hrs10spt + e*Distress + b*genhelf
direct_Distress :=e
indirect_Distress :=(b*f)
total_Distress :=e + (b*f)
'
M2<-lavaan::sem(s2, data=data)
summary(M2)
standardizedSolution(M2, type="std.all")

### Path analysis and mediation

data2<-haven::read_dta("data/data2.dta")

#Example 1 (just identified model).
#hs = exposure.
#gre = mediator.
#grad = outcome
M1<-'
grad ~ y1*gre + y2*hs + y3*col
gre ~ b1*hs + b2*col
direct.hs :=y2
indirect.hs :=(b1*y1)
total.hs :=y2 + (b1*y1)
'
M1.res<-lavaan::sem(M1, data=data2)
summary(M1.res)

#Example 2.
#Just-identified model.
M2<-'
grad ~ y1*gre + y2*hs + y3*col
gre ~ b1*hs + b2*col
col ~ c1*hs 
direct.hs :=y2
indirect.hs :=(b1*y1)+(c1*y3)+(c1*b2*y1)
total.hs :=y2 + (b1*y1)+(c1*y3)+(c1*b2*y1)
'
M2.res<-lavaan::sem(M2, data=data2)
summary(M2.res)

#Example 3.
#Over-identified model.
#An over-identified model, that is a model with positive df 
#(as opposed to the previous models which can be described as 
#saturated or just identified). 
#Having positive df allows us to examine the fit of the model 
#using the chi-squared test of model fit, 
#along with fit indices, for example, CFI and RMSEA.

#Two mediators.
M3<-'
grad ~ y1*gre + y2*col
gre ~ b1*col
col ~ c1*hs 
indirect.hs :=(c1*y2)+(c1*b1*y1)
total.hs :=(c1*y2)+(c1*b1*y1)
'
M3.res<-lavaan::sem(M3, data=data2)
summary(M3.res)

#The chi-squared value compares the current model to a saturated model. 
#Since our model is not saturated (i.e., our model has positive df), 
#the chi-squared value is no longer zero and 
#may be used to evaluate model fit. 
#Similarly, the CFI and TLI which were equal to one in the 
#just identified model now take on informative values. 
#Further down, the RMSEA and SRMR now take on informative values 
#(in a just identified model, they are displayed as zero). 
#Having positive df, and hence, informative values 
#of the fit indices allows us to better evaluate how well our model 
#fits the data. 

### Mediation.

#In the classic paper on mediation analysis, Baron and Kenny (1986, p.1176) 
#defined a mediator as "In general, a given variable may be said to function 
#as a mediator to the extent that it accounts for the relation between 
#the predictor and the criterion." Therefore, mediation analysis answers the 
#question why X can predict Y. 
#Note that a mediation model is a directional model. For example, the mediator 
#is presumed to cause the outcome and not vice versa. 
#Mediation is not defined statistically; rather statistics can be used to 
#evaluate a presumed mediation model. 
#Baron and Kenny (1989) outlined a 4-step procedure to determine whether 
#there is a mediation effect:
  
#  (1) Show that X is correlated with Y. Regress Y on X to estimate and 
#test the path c. This step establishes that there is an effect that may 
#be mediated.

#(2) Show that X is correlated with M. Regress M on X to estimate and test path 
##a. This step essentially involves treating the mediator as if it were an 
#outcome variable.

#(3) Show that M affects Y. Regress Y on both X and M to estimate and test path 
#b. Note that it is not sufficient just to correlate the mediator with the 
#outcome; the mediator and the outcome may be correlated because 
#they are both caused by the input variable X. 
#Thus, the input variable X must be controlled in establishing 
#the effect of the mediator on the outcome.

#(4) To establish that M completely mediates the X-Y relationship, 
#the effect of X on Y controlling for M (path c) should be zero. 

#The effects in both Steps 3 and 4 are estimated in the same equation.
#If all four of these steps are met, then the data are consistent 
#with the hypothesis that variable M completely mediates the X-Y relationship, 
#and if the first three steps are met but the Step 4 is not, 
#then partial mediation is indicated.

#hs = exposure.
#gre = mediator.
#grad = outcome
M4<-'
grad ~ y1*gre + y2*hs 
gre ~ b1*hs 
direct.hs :=y2
indirect.hs :=(b1*y1)
total.hs :=y2 + (b1*y1)
'
M4.res<-lavaan::sem(M4, data=data2)
summary(M4.res)

#Baron and Kenny.
#Step 1 (Y~X)
summary(lm(data2$grad~data2$hs))
# Step 2 (M~X)
summary(lm(data2$gre~data2$hs))
#Steps 3 and 4 (Y~X+M)
summary(lm(data2$grad~data2$hs+data2$gre))

#Summary:
  
#Step 1 (Y ~ X): X is significantly related to Y (0.6508) (c).
#Step 2: (M ~ X): X is significantly related to M (0.552) (a).
#Step 3: (Y ~ X+M):  X is significantly related to Y (b) 
#after adjustment for M: (0.4237).
#Both b and a are significant: M significantly mediates the relationship 
#between X and Y.
#Mediation effect = indirect effect = (0.552*0.411) = 0.227. 
#Direct effect is also significant: have partial mediation.

#Sobel test: Many researchers believe that the essential steps in establishing 
#mediation are Steps 2 and 3. 
#Step 4 does not have to be met unless the expectation is for complete 
#mediation. 
#In the opinion of most researchers, though not all, Step 1 is not required. 
#However, note that a path from the input variable to the outcome is implied 
#if Steps 2 and 3 are met. 

#If c' (direct) were in the opposite sign to that of a*b (indirect), 
##then it could be the case that Step 1 would not be met, but there is 
#still mediation. 
#In this case the mediator acts like a suppressor variable. 
#MacKinnon, Fairchild, and Fritz (2007) called it inconsistent mediation. 
#The total effect of X on Y may be very small because the direct and indirect 
#effects will tend to cancel each other out. 
#Note that with inconsistent mediation, the direct effect is typically larger 
#than the total effect.
#It is therefore much more common and highly recommended to perform a single 
#test of a*b, than the two separate tests of a and b.

model.M <- lm(gre ~ hs, data2)
model.Y <- lm(grad ~ hs + gre,data2)
results <- mediation::mediate(model.M, model.Y, treat='hs', mediator='gre',
                   boot=TRUE, sims=10)
summary(results)

##########################################
#Indirect effects and mediation analysis##
##########################################

set.seed(1234)
x <- rnorm(100)
m <- 0.5*x + rnorm(100)
y <- 0.7*m + rnorm(100)
data <- data.frame(x = x, y = y, m = m)
M <- ' 
# direct effect
y ~ c*x
# mediator
m ~ a*x
y ~ b*m
# indirect effect (a*b)
indirect := a*b
# total effect
total := c + (a*b)
'
fit <- lavaan::sem(M, data = data)
summary(fit)

### CFA and SEM using PoliticalDemocracy
M1 <- '
# measurement model
Ind60 =~ x1 + x2 + x3
Dem60 =~ y1 + y2 + y3 + y4
Dem65 =~ y5 + y6 + y7 + y8
# regressions
Dem60 ~ Ind60
Dem65 ~ Ind60 + Dem60
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'
fit <- lavaan::sem(M1, data = PoliticalDemocracy)
summary(fit, standardized = TRUE)

# Fixing parameters
M2 <- '
# measurement model
# loading on x2 = 1 (note no brackets)
Ind60 =~ 1*x1 + x2 + x3   
Dem60 =~ 1*y1 + 1*y2 + y3 + y4
Dem65 =~ 1*y5 + y6 + y7 + y8
# regressions
Dem60 ~ Ind60
Dem65 ~ Ind60 + Dem60
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'
fit <- lavaan::sem(M2, data = PoliticalDemocracy)
summary(fit, standardized = FALSE)


### CFA and SEM using HolzingerSwineford1939

## 1-factor CFA
HS.model <- ' visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 '
fit <- cfa(HS.model, data = HolzingerSwineford1939)
head(lavPredict(fit))
head(lavPredict(fit, type = "ov"))

#3-factor CFA
M1 <- ' Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9 '
fit <- lavaan::cfa(M1, data=HolzingerSwineford1939)
summary(fit, fit.measures=TRUE)

#Constrain covariances of the 3 LVs 
#to be orthogonal (uncorrelated)
M2 <- ' Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9 '
fit <- lavaan::cfa(M2,data = HolzingerSwineford1939,
                   orthogonal = TRUE)
summary(fit)

# Constrain all LV error to 1.
M3 <- ' Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9 '
fit <- lavaan::cfa(M3,data = HolzingerSwineford1939,
                   std.lv = TRUE)
summary(fit)

# Constraints
M4 <- '
# three-factor model
Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ NA*x7 + x8 + x9
# orthogonal factors
Visual ~~ 0*Speed
Textual ~~ 0*Speed
Textual ~~ Visual
# fix variance of speed factor
Speed ~~ 1*Speed
'
fit <- lavaan::sem(M4, data = HolzingerSwineford1939)
summary(fit)

# Starting values.
#M5 <- '
#Visual =~ x1 + start(0.8)*x2 + start(1.2)*x3
#Textual =~ x4 + start(0.5)*x5 + start(1.0)*x6
#Speed =~ x7 + start(0.7)*x8 + start(1.8)*x9 '
#fit <- lavaan::cfa(M5, data=HolzingerSwineford1939)
#summary(fit, fit.measures=TRUE)

#Using labels (here for loading)
#M6 <- '
#Visual =~ x1 + start(0.8)*x2 + start(1.2)*x3 + MyLabel*x3
#Textual =~ x4 + start(0.5)*x5 + start(1.0)*x6
#Speed =~ x7 + start(0.7)*x8 + start(1.8)*x9 '
#fit <- lavaan::cfa(M6, data=HolzingerSwineford1939)
#summary(fit, fit.measures=TRUE)

#Equality constraints (estimate but constrain a)
M7 <- '
Visual =~ x1 + a*x2 + x3 
Textual =~ x4 + a*x5 + x6
Speed =~ x7 + x8 + x9 '
fit <- lavaan::cfa(M7, data=HolzingerSwineford1939)
summary(fit, fit.measures=TRUE)

#Bringing in the means (long version)
M8 <- '
# three-factor model
Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9
# estimate the Intercepts
x1 ~ 1
x2 ~ 1
x3 ~ 1
x4 ~ 1
x5 ~ 1
x6 ~ 1
x7 ~ 1
x8 ~ 1
x9 ~ 1'
fit <- lavaan::cfa(M8, data=HolzingerSwineford1939)
summary(fit)

#Bringing in the means (short version)
#cons in the measurement model in Stata.
M8 <- '
# three-factor model
Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9'
fit <- lavaan::cfa(M8, data=HolzingerSwineford1939,
                   meanstructure = TRUE)
summary(fit)

#Constrain the intercepts
M9 <- '
# three-factor model
Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9
# intercepts
x1 ~ 1
x2 ~ 1
x3 ~ 0.5*1
x4 ~ 0.5*1
x5 ~ 1
x6 ~ 1
x7 ~ 1
x8 ~ 1
x9 ~ 1'
fit <- lavaan::cfa(M9, data=HolzingerSwineford1939)
summary(fit)

###########################################
# Multi-group using HolzingerSwineford1939
###########################################


#Default: same CFA model in each group (all parameters vary).
#LV means 0 in each group.
M1 <- 'Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9 '
fit <- lavaan::cfa(M1,data = HolzingerSwineford1939,group = "school")
summary(fit)

# Fix parameters (parameter in 1st, parameter in 2nd)
M2 <- ' Visual =~ x1 + c(0.5,0.5)*x2 + c(0.6, 0.9)*x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9 '
fit <- lavaan::cfa(M2,data = HolzingerSwineford1939,group = "school")
summary(fit)

#Constrain a single parameter to be equal across groups 
#here the loading for Visual -> x3
M3 <- ' Visual =~ x1 + x2 + c(a,a)*x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9 '
fit <- lavaan::cfa(M3,data = HolzingerSwineford1939,group = "school")
summary(fit)

#Constraint x3 in 1st group only
M4 <- ' Visual =~ x1 + x2 + c(1,NA)*x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9 '
fit <- lavaan::cfa(M4,data = HolzingerSwineford1939,group = "school")
summary(fit)


#Constraining groups of parameters to be equal across groups

#+ loadings: factor loadings
#+ intercepts: the intercepts of the observed variables
#+ means: the intercepts/means of the latent variables
#+ residuals: the residual variances of the observed variables
#+ residual.covariances: the residual covariances of the observed variables
#+ lv.variances: the (residual) variances of the latent variables
#+ lv.covariances: the (residual) covariances of the latent variables
#+ regressions: all regression coefficients in the model


#Constrain the loadings to be the same.
M5 <- ' Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9 '
fit <- lavaan::cfa(M5,data = HolzingerSwineford1939,group = "school",
                   group.equal = c("loadings"))
summary(fit)

#loadings and intercepts.
#intercepts for LVs no longer 0 in both groups.
M6 <- ' Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9 '
fit <- lavaan::cfa(M6,data = HolzingerSwineford1939,group = "school",
                   group.equal = c("loadings","intercepts"))
summary(fit)

#Most equal but specify some parameters that are free to vary across groups.
#loading x2 now varies across groups.
#intercept for x8 now varies across groups.
#All other loadings/intercepts are the same.
M7 <- ' Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9 '
fit <- lavaan::cfa(M7,data = HolzingerSwineford1939,group = "school",
                   group.equal = c("loadings","intercepts"),
                   group.partial = c("Visual=~x2", "x8~1"))
summary(fit)

# Measurement invariance using HolzingerSwineford1939

#Before we compare, say, the values of latent means across multiple
#groups, we first need to establish measurement invariance. 
#When data is continuous, testing for measurement
#invariance involves a fixed sequence of model comparison tests. 
#A typical sequence involves three models:
  
#Model 1: configural invariance. The same factor structure is imposed on 
#all groups.
#Model 2: weak invariance. The factor loadings are constrained to be equal 
#across groups.
#Model 3: strong invariance. The factor loadings and intercepts are 
#constrained to be equal across groups.


M <- ' Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9 '

# configural invariance
fit1 <- lavaan::cfa(M, data = HolzingerSwineford1939, group = "school")
# weak invariance
fit2 <- lavaan::cfa(M, data = HolzingerSwineford1939, group = "school",
                    group.equal = "loadings")
# strong invariance
fit3 <- lavaan::cfa(M, data = HolzingerSwineford1939, group = "school",
                    group.equal = c("intercepts", "loadings"))
# model comparison tests
lavTestLRT(fit1, fit2, fit3)


#The lavTestLRT() function can be used for model comparison tests. 
#Because we provided three model fits, it will produce two tests: 
#(i) the first test compares the first model versus the second model, while 
#(ii) the second test compares the second model versus the third model. 

#Because the first p-value is non-significant, we may
#conclude that weak invariance (equal factor loadings) is supported in this 
#dataset. 

#However, because the second p-value is significant, strong invariance is not. 
##Therefore, it is unwise to directly compare the values of
#the latent means across the two groups. M2 is the best fit: cannot rely on M3.

# Modification indices

M <- ' Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9 '
fit <- lavaan::cfa(M,data = HolzingerSwineford1939)
mi<-lavaan::modindices(fit, sort = TRUE, maximum.number = 5)
mi
mi[mi$op == "=~"]  # just see the loadings

# Extracting information from a fitted model
M <- ' Visual =~ x1 + x2 + x3
Textual =~ x4 + x5 + x6
Speed =~ x7 + x8 + x9 '
fit <- cfa(M,data = HolzingerSwineford1939)
lavaan::parameterEstimates(fit)
lavaan::parameterEstimates(fit,standardized = TRUE)
lavaan::fitted(fit)
lavaan::resid(fit)
lavaan::vcov(fit)
lavaan::fitMeasures(fit)
lavaan::fitMeasures(fit, c("cfi","rmsea","srmr"))
lavaan::lavInspect(fit)

############################
# SEM for longitudinal data
############################

#Wheaton 
#Using covariance matrix as input
lower <- '
11.834
6.947 9.364
6.819 5.091 12.532
4.783 5.028 7.495 9.986
-3.839 -3.889 -3.841 -3.625 9.610
-21.899 -18.831 -21.748 -18.775 35.522 450.288 '
M.cov <-
  getCov(lower, names = c("anomia67", "pwless67",
                          "anomia71", "pwless71",
                          "education", "wealth"))
M.cov

#classic wheaton et al. model
M <- '
# latent variables
SES =~ education + wealth
Alien67 =~ anomia67 + pwless67
Alien71 =~ anomia71 + pwless71
# regressions
Alien71 ~ Alien67 + SES
Alien67 ~ SES
# correlated residuals
anomia67 ~~ anomia71
pwless67 ~~ pwless71
'
fit <- sem(M,
           sample.cov = M.cov,
           sample.nobs = 932)
summary(fit, standardized = TRUE)


###################
#Stata commands
###################

#clear all
#cd "/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/Path Analysis and SEM/"
#Path analysis
#use "data/practical08v2.dta", clear

#Example 1.
#Six direct effects via 3 separate linear equations.
#sem (hrs10spt ghq12scr-> genhelf) (hrs10spt-> ghq12scr) ///
#(hrs10spt ghq12scr genhelf-> bmival), standardized 
#estat teffects, standardized compact

#Example 2.
#sem(Distress-> ghqconc ghqconfi ghqdecis ghqenjoy ghqface     ///
#ghqhappy ghqover ghqsleep ghqstrai ghqunhap ghquse ghqworth)   ///
#(hrs10spt->genhelf) (hrs10spt->Distress)                      /// 
#(Distress->genhelf) (genhelf hrs10spt Distress->bmival),      /// 
#standardized latent(Distress)
#estat teffects,standardized compact


#Mediation using data2
#use "data/data2.dta", clear
#Example 1.
#sem (gre hs col -> grad) (hs col -> gre)
#estat teffects, compact
#sgmediation2 grad, mv(gre) iv(hs) cv(col)

#Example 2.
#sem (gre hs col -> grad) (hs col -> gre) (hs -> col)
#estat teffects, compact


#Example 3.
#no direct effect of hs (only indirect).
#sgmediation gives different estimate
#sem (gre col -> grad) (col -> gre) (hs -> col)
#estat teffects, compact


#Example 4.
#sem (gre hs -> grad) (hs -> gre) 
#estat teffects, compact
#sgmediation2 grad, mv(gre) iv(hs) 

#use "data/nlsy97cfa.dta", clear
#Single-factor CFA
#sem(Cons->x1-x9),standardized nolog
#residual correlation
#sem(Conservative->x1 x3-x7 x9),covariance(e.x3*e.x4) standardized nolog

#two-factor CFA
#sem(Depress->x11-x13) (Cons->x1 x3-x7 x9), covariance(e.x3*e.x4) standardized 
#estat mindices, minchi2(7)

#using PoliticalDemocracy.dta
#use "data/PoliticalDemocracy.dta", clear
#Example 1
#sem(Ind60 -> x1 x2 x3) (Dem60 -> y1 y2 y3 y4) (Dem65 -> y5 y6 y7 y8) ///
#(Ind60-> Dem60) (Ind60 Dem60 -> Dem65), ///
#cov(e.y1*e.y5) cov(e.y2*e.y4) cov(e.y2*e.y6) cov(e.y3*e.y7) ///
#cov(e.y4*e.y8) cov(e.y6*e.y8) standardize 

#Example 2 (fix loadings)
#sem(Ind60@1 -> x1) (Ind60 -> x2) (Ind60 -> x3) ///
#(Dem60@1 -> y1) (Dem60@1 -> y2) (Dem60 -> y3 y4) (Dem65 -> y5 y6 y7 y8) ///
#(Ind60-> Dem60) (Ind60 Dem60 -> Dem65), ///
#cov(e.y1*e.y5) cov(e.y2*e.y4) cov(e.y2*e.y6) ///
#cov(e.y3*e.y7) cov(e.y4*e.y8) cov(e.y6*e.y8) 


#using HolzingerSwineford1939.dta
#use "data/HolzingerSwineford1939.dta", clear

#Example 1.
#sem(Visual -> x1 x2 x3) (Textual -> x4 x5 x6) (Speed -> x7 x8 x9), nolog

#Example 2 (LVs to be uncorrelated)
#sem(Visual -> x1 x2 x3) (Textual -> x4 x5 x6) (Speed -> x7 x8 x9), ///
#covariance(Visual*Textual@0 Visual*Speed@0 Textual*Speed@0) nolog

#Example 3 (LV variance fixed at 1).
#sem(Visual -> x1 x2 x3) (Textual -> x4 x5 x6) (Speed -> x7 x8 x9), ///
#var(Visual@1 Textual@1 Speed@1) 

#Example 4 (constraints)
#sem(Visual -> x1 x2 x3) (Textual -> x4 x5 x6) (Speed -> x7 x8 x9), ///
#covariance(Visual*Speed@0 Textual*Speed@0) var(Speed@1) nolog

#Example 7 (Equality constraint).
#sem(Visual -> x1 x3) (Visual@a -> x2) (Textual -> x4 x6) (Textual@a -> x5) ///
#(Speed -> x7 x8 x9), nolog

#Example 8 (constrain intercepts).
#sem(Visual -> x1 x2 x3)  (Textual -> x4 x5 x6) (Speed -> x7 x8 x9) ///
#(_cons@0.5 -> x3) (_cons@0.5 -> x4), nolog




#Multi-group.
#use "data/HolzingerSwineford1939.dta", clear
#gen school1=0
#replace school1=1 if school==2
#replace school1=2 if school==1
#label define a 1 "Pasteur" 2 "Grant-White"
#label values school1 a

#When we specify group(groupvar), the measurement part of the model is 
#constrained by default 
#to be the same across the groups, whereas the remaining parts will have 
#separate parameters for each group.

#Example 1 (all parameters vary)
#sem(Visual -> x1 x2 x3) (Textual -> x4 x5 x6) (Speed -> x7 x8 x9), ///
#group(school1) ///
#ginvariant(none) mean(Visual@0 Textual@0 Speed@0) nolog

#Example 2 (fix loadings)
#sem(Visual@1 -> x1) (Visual -> x2) (1: Visual@0.6 -> x3) (2: Visual -> x3) ///
#(Textual -> x4 x5 x6) (Speed -> x7 x8 x9), group(school1) ginvariant(none) ///
#mean(Visual@0 Textual@0 Speed@0) 

#Example 3.
#Constrain a single parameter (x3) to be equal across groups 
#sem(Visual@1 -> x1) (Visual -> x2) (Visual@a -> x3) ///
#(Textual -> x4 x5 x6) (Speed -> x7 x8 x9), group(school1) ginvariant(none)  ///
#mean(Visual@0 Textual@0 Speed@0)

#Example 4.
#Constrain x3 in 1st group only
#sem(Visual@1 -> x1) (Visual -> x2)  (1: Visual@1 -> x3) (2: Visual -> x3) ///
#(Textual -> x4 x5 x6) (Speed -> x7 x8 x9), group(school1) ginvariant(none)  ///
#mean(Visual@0 Textual@0 Speed@0)

#Example 5.
#Constrain loadings to be the same
#sem(Visual -> x1 x2 x3) (Textual -> x4 x5 x6) (Speed -> x7 x8 x9), ///
#group(school1) ginvariant(mcoef) ///
#mean(Visual@0 Textual@0 Speed@0)

#Example 6.
#Constrain intercepts & loadings to be the same
#sem(Visual -> x1 x2 x3) (Textual -> x4 x5 x6) (Speed -> x7 x8 x9), ///
#group(school1) ginvariant(mcons mcoef) 

#Example 7.
#x2 vary; intercept for x8.
#sem(Visual -> x1 x3) (Textual -> x4 x5 x6) (Speed -> x7 x9) ///
#(1: Visual@b1 -> x2) (2: Visual@b2 -> x2)  ///
#(1: Speed _cons@a1 -> x8) (2: Speed _cons@a2 -> x8) ///
#, group(school1) ginvariant(mcons mcoef) 

#Measurement invariance

#Fit 1.
#Example 1 (all parameters vary)
#sem(Visual -> x1 x2 x3) (Textual -> x4 x5 x6) (Speed -> x7 x8 x9), 
#group(school1) ///
#ginvariant(none) mean(Visual@0 Textual@0 Speed@0) nolog
#est store a

#Fit 2.
#Example 5.
#Constrain loadings to be the same
#sem(Visual -> x1 x2 x3) (Textual -> x4 x5 x6) (Speed -> x7 x8 x9), 
#group(school1) ginvariant(mcoef) ///
#mean(Visual@0 Textual@0 Speed@0)
#est store b

#Fit 3.
#Example 6.
#Constrain intercepts & loadings to be the same
#sem(Visual -> x1 x2 x3) (Textual -> x4 x5 x6) (Speed -> x7 x8 x9), 
#group(school1) ginvariant(mcons mcoef) 
#est store c

#lrtest a b
#lrtest b c


print(paste("Path analysis and SEM finished at", 
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

################
#FINISHED
################









