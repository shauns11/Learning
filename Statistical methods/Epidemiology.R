#############################.
#Epidemiology
#############################.

.libPaths()
print(R.version.string)

Macdrive<-
  "/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/"
UCLdrive<-
  "C:/Users/rmjdshc/OneDrive - University College London/Learning/"
folder <- "Teaching/Basic statistics"  
setwd(paste0(UCLdrive, folder))
#setwd(paste0(Macdrive, folder))

library(markdown)
library(car)
library(multcomp)
library(tidyverse)
library(summarytools)
library(pwr)
library(srvyr)
library(survey)
library(mStats)
library(broom)
library(haven)
library(lmtest)
library(labelled)
#main dataset
load("data/elsa.Rdata")
#paired t-test
load("data/contin2.Rdata")
#confEM 
confEM<-haven::read_dta("data/teaching_confEM.dta")

#################
#Categorical Data 1
###################

table(elsa$sex)
prop.table(table(elsa$sex))
summarytools::freq(elsa$sex)
freq(elsa$past_cvd)
cvd <- table(elsa$past_cvd)
cvd
prop.test(cvd) 
attributes(elsa$sex)
cvd_male <- table(elsa$past_cvd[elsa$sex == "male"])
cvd_male
prop.test(cvd_male)
cvd_female <- table(elsa$past_cvd[elsa$sex == "female"])
cvd_female
prop.test(cvd_female)
cvd_sex <- table(elsa$sex, elsa$past_cvd)
cvd_sex
#not apply Yates' continuity correction to match Stata output
prop.test(cvd_sex,correct=FALSE)

elsa <- elsa %>%
mutate(age_group = case_when(
age >= 45 & age <= 64 ~ "45 - 64",
age >= 65 ~ "65+"),
age_group = factor(age_group))
# Check we made the variable correctly
summary(elsa$age[elsa$age_group == "45 - 64"]) 
summary(elsa$age[elsa$age_group == "65+"]) 
             
cvd_age <- table(elsa$age_group, elsa$past_cvd)
cvd_age
prop.test(cvd_age)
             
freq(elsa$smok,cumul=FALSE)
elsa <- elsa %>%
mutate(current_smok = fct_collapse(smok, 
    "not current" = levels(smok)[1:3],
      "current" = levels(smok)[4:4]))
table(elsa$current_smok)
cvd_smok <- table(elsa$current_smok, elsa$past_cvd)
cvd_smok
prop.test(cvd_smok,correct=FALSE)
             
###################
#Categorical Data 2
###################

freq(elsa$heart_attack,cumul=FALSE)
elsa <- elsa %>%
  mutate(age10 = case_when(
    age >= 45 & age <= 54 ~ "45 - 54",
    age >= 55 & age <= 64 ~ "55 - 64",
    age >= 65 & age <= 74 ~ "65 - 74",
    age >= 75 ~ "75+"),
    age10 = factor(age10))
summarytools::freq(elsa$age10)
by(elsa$age, elsa$age10, summary) 

ctable(elsa$sex, elsa$heart_attack, prop="row", useNA="no")
ctable(elsa$age10, elsa$heart_attack, prop="row", useNA="no")
ctable(elsa$smok_bin, elsa$heart_attack, prop="row", useNA="no")
ctable(elsa$manual, elsa$heart_attack, prop="row", useNA="no")
ctable(elsa$sex, elsa$heart_attack, prop="row", useNA="no", chisq = TRUE)
ctable(elsa$manual, elsa$heart_attack, prop="row", useNA="no", chisq = TRUE)
ctable(elsa$smok_bin, elsa$heart_attack, prop="row", useNA="no", chisq = TRUE)
ctable(elsa$age10, elsa$heart_attack, prop="row", useNA="no", chisq = TRUE)

mStats::mhor(elsa,sex,heart_attack, 
             exp_value="male", 
             case_value = "Mentioned",
             digits=2)

#smokers as exposed: non-smokers as reference
mStats::mhor(elsa,smok_bin,heart_attack, 
             exp_value="ex/current reg", 
             case_value = "Mentioned",
             digits=2)

#manual as exposed: non-manual as reference
mStats::mhor(elsa,manual,heart_attack, 
             exp_value="manual", 
             case_value = "Mentioned",
             digits=2)

#exposed are 55-64:
mStats::mhor(elsa,age10,heart_attack, 
             exp_value="55 - 64",
             case_value = "Mentioned",
             digits=2)

#exposed are 65-74:
mStats::mhor(elsa,age10,heart_attack, 
             exp_value="65 - 74",
             case_value = "Mentioned",
             digits=2)

#exposed are 75+:
mStats::mhor(elsa,age10,heart_attack, 
             exp_value="75+",
             case_value = "Mentioned",
             digits=2)

###################
#Categorical Data 3
###################

elsa <- elsa %>%
  mutate(obese = ifelse(bmi > 30, "Obese", "Not Obese"),
         obese = factor(obese, levels = c("Obese", "Not Obese")))
freq(elsa$obese)

class(elsa$wealth5)
elsa$wealth5 <- factor(elsa$wealth5,
                       levels=c(1,2,3,4,5),
                       labels=c("lowest","2","3","4","highest"))
freq(elsa$wealth5)
ctable(elsa$wealth5, elsa$obese, prop="row", chisq = TRUE, useNA = "no")

#chi square test for trend.
# x ~ the number of "events" in each group (in this case people with obesity), 
#and 
# n ~ the number of "trials" in each group (in this case the number of people).

events <- table(elsa$wealth5[elsa$obese == "Obese"])
trials <- table(elsa$wealth5)
stats::prop.trend.test(x = events, n = trials)

class(elsa$obese)
class(elsa$wealth5)
filter_df <- elsa %>%
dplyr::filter(obese %in% c("Not Obese", "Obese"),
              wealth5 %in% c("lowest", "2", "3", "4", "highest"))
nrow(filter_df)

#lowest wealth as exposed
mStats::mhor(filter_df,wealth5,obese, 
             exp_value="lowest",
             case_value = "Obese",
             digits=2)

#4th highest wealth as exposed
mStats::mhor(filter_df,wealth5,obese, 
             exp_value="4",
             case_value = "Obese",
             digits=2)

#male as exposed:
ctable(elsa$sex, elsa$obese, prop="row", OR = TRUE, RR = TRUE, useNA = "no")

#never vs ex/current smokers
ctable(elsa$smok_bin, elsa$obese, prop="row", OR = TRUE, RR = TRUE, 
       useNA = "no")
                                     
################
#Continuous 1
################
class(elsa$bmi)
sjmisc::descr(elsa$bmi)
t.test(elsa$bmi,na.rm=TRUE)
sjmisc::descr(elsa$dbp)
t.test(elsa$dbp,na.rm=TRUE)
attributes(elsa$sex)
t.test(elsa$bmi[elsa$sex == "male"],na.rm=TRUE)
t.test(elsa$bmi[elsa$sex == "female"],na.rm=TRUE)
by(elsa$bmi, elsa$sex, t.test,na.rm=TRUE)
attributes(elsa$physact)
by(elsa$bmi, elsa$physact, t.test,na.rm=TRUE)
t.test(elsa$bmi[elsa$age >= 60], mu = 28.5,na.rm=TRUE)
t.test(elsa$bmi[elsa$age >= 60], mu = 27.5,na.rm=TRUE)

mean_60plus <- mean(elsa$bmi[elsa$age >= 60], na.rm = TRUE)
mean_60plus

t.test(elsa$bmi[
  elsa$age >= 60 & elsa$physact == levels(elsa$physact)[1]],
  mu = mean_60plus,na.rm=TRUE)


elsa %>%
  # use filter to keep the correct observations
  filter(age >= 60, physact == levels(physact)[1]) %>% 
  # use pull to extract the bmi variable
  pull(bmi) %>% 
  t.test(mu = mean_60plus,na.rm=TRUE)

################
#Continuous 2
################

t.test(elsa$sbp,na.rm=TRUE)
t.test(elsa$dbp,na.rm=TRUE)
t.test(elsa$bmi,na.rm=TRUE)
t.test(elsa$chol,na.rm=TRUE)

elsa %>%
  dplyr::select(sbp, dbp, bmi, chol) %>% 
  map(t.test,na.rm=TRUE)

t.test(elsa$sbp, mu = 130,na.rm=TRUE)

t.test(sbp ~ sex, elsa,na.rm=TRUE,var.equal=TRUE)
t.test(sbp ~ sex, elsa,na.rm=TRUE)
t.test(sbp ~ sex, elsa,na.rm=TRUE,var.equal=FALSE)

levels(elsa$bmi4)
elsa <- elsa %>%
  mutate(bmi_bin = forcats::fct_collapse(bmi4, 
                                         "Less than 25" = levels(bmi4)[1:2],
                                         "Over 25" = levels(bmi4)[3:4]))
table(elsa$bmi_bin, elsa$bmi4, useNA = "ifany")

t.test(sbp ~ bmi_bin, elsa,na.rm=TRUE,var.equal=TRUE)

#################################
#paired t-test (using contin2).
#################################

t.test(contin2$oidpsco1, contin2$oidpsco2, paired = TRUE,na.rm=TRUE)
levels(contin2$tx)
# change in the control group
t.test(contin2$oidpsco1[contin2$tx == levels(contin2$tx)[1]], 
       contin2$oidpsco2[contin2$tx == levels(contin2$tx)[1]], 
       paired = TRUE)  
# change in the treatment group
t.test(contin2$oidpsco1[contin2$tx == levels(contin2$tx)[2]], 
       contin2$oidpsco2[contin2$tx == levels(contin2$tx)[2]], 
       paired = TRUE)  

contin2 <- contin2 %>%
  mutate(diff_in_qol = oidpsco1 - oidpsco2)
t.test(diff_in_qol ~ tx, contin2,na.rm=TRUE,var.equal=TRUE)

################################
#Continuous 3 (using elsa)
###############################

elsa %>%
  dplyr::select(age, chol, bmi) %>%
  map(t.test,na.rm=TRUE)

qplot(elsa$chol)
qplot(elsa$bmi)
qplot(elsa$age)
cor.test(elsa$chol, elsa$age,na.rm=TRUE)
cor.test(elsa$bmi, elsa$age,na.rm=TRUE)
cor.test(elsa$chol, elsa$bmi,na.rm=TRUE)

elsa <- elsa %>%
  mutate(age_grps = case_when(
    age >= 65 ~ "65+", 
    age >= 50 & age < 65 ~ "50-64"), 
    age_grps = factor(age_grps, levels = c("50-64", "65+"))) 

# Check worked correctly
by(elsa$age, elsa$age_grps, summary) 
var.test(bmi ~ age_grps, elsa,nar.rm=TRUE)
t.test(bmi ~ age_grps, elsa,var.equal=FALSE)
t.test(bmi ~ age_grps, elsa, var.equal = TRUE)

attributes(elsa$physact)
table(elsa$physact)
prop.table(table(elsa$physact)) 
by(elsa$bmi, elsa$physact, mean, na.rm = TRUE)
aov_res <- stats::aov(bmi ~ physact, elsa)
summary(aov_res)
bartlett.test(bmi ~ physact, data = elsa)

by(elsa$chol, elsa$physact, mean, na.rm = TRUE)
aov_res <- stats::aov(chol ~ physact, elsa)
summary(aov_res)
bartlett.test(chol ~ physact, data = elsa)

################
#Non-parametric
################

sjmisc::descr(elsa$crp)
t.test(elsa$crp,na.rm=TRUE)
qplot(elsa$crp)
 
ggplot(elsa) +
  aes(x = crp) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(elsa$crp, na.rm = TRUE),
                            sd = sd(elsa$crp, na.rm = TRUE)))

sjmisc::descr(elsa$alco)
t.test(elsa$alco,na.rm=TRUE)
ggplot(elsa) +
  aes(x = alco) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, 
                args = list(
                  mean = mean(elsa$alco, na.rm = TRUE),
                  sd = sd(elsa$alco, na.rm = TRUE)))

stats::wilcox.test(crp ~ sex, elsa)
psych::describeBy(elsa$crp, elsa$sex)

wilcox.test(crp ~ manual, elsa)
psych::describeBy(elsa$crp, elsa$manual)

wilcox.test(crp ~ smok_bin, elsa)
psych::describeBy(elsa$crp, elsa$smok_bin)

stats::kruskal.test(crp ~ physact, elsa)
psych::describeBy(elsa$crp, elsa$physact)

kruskal.test(crp ~ bmi4, elsa)
psych::describeBy(elsa$crp, elsa$bmi4)

elsa %>%
  select(crp, alco, wealth10) %>%
  cor(use = "complete.obs", method = "spearman")

################
#Linear Regression 1
################

sjmisc::descr(elsa$age)
sjmisc::descr(elsa$sbp)

ggplot(elsa) +
  aes(x = sbp) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(
    mean = mean(elsa$sbp, na.rm = TRUE),
    sd = sd(elsa$sbp, na.rm = TRUE)))

ggplot(elsa) +
  aes(x = sbp) +
  geom_density(color = "red") +
  stat_function(fun = dnorm, args = list(
    mean = mean(elsa$sbp, na.rm = TRUE),
    sd = sd(elsa$sbp, na.rm = TRUE)))

ggplot(elsa) +
  aes(x = age, y = sbp) +
  geom_point()

ggplot(elsa) +
  aes(x = age, y = sbp) +
  geom_jitter(alpha = 0.3)

elsa %>%
  dplyr::select(age, sbp) %>%
  cor(use = "complete.obs")

model1 <- lm(sbp ~ age, elsa)
summary(model1)
confint(model1)

coef(model1)
coef(model1)["(Intercept)"] + coef(model1)["age"]*55

glht(model1, "`(Intercept)` + 55*`age` = 0") %>%
  confint()

newdata <- augment(model1)
newdata %>%
  dplyr::select(sbp, age, .fitted) %>%
  slice(1:5)
newdata %>%
  dplyr::select(sbp, age, .fitted, .resid) %>%
  slice(1:5)

ggplot(elsa) +
  aes(x = age, y = sbp) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = lm)

ggplot(newdata) +
  aes(x = .fitted, y = .resid) +
  geom_hline(yintercept = 0) +
  geom_jitter(alpha = 0.2)

ggplot(newdata) +
  aes(x = .fitted, y = .std.resid) +
  geom_hline(yintercept = 0) +
  geom_jitter(alpha = 0.2)

ggplot(newdata) +
  aes(x = .std.resid) +
  geom_density(color = "red") +
  stat_function(fun = dnorm)

###################
#Linear regression 2
###################

psych::describeBy(elsa$sbp, elsa$sex)

#plot for females
ggplot(elsa[elsa$sex == "female", ]) +
  aes(x = sbp) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, 
                args = list(
                  mean = mean(elsa$sbp[elsa$sex == "female"], na.rm = TRUE),
                  sd = sd(elsa$sbp[elsa$sex == "female"], na.rm = TRUE)))
#plot for males
ggplot(elsa[elsa$sex == "male", ]) +
  aes(x = sbp) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, 
                args = list(
                  mean = mean(elsa$sbp[elsa$sex == "male"], na.rm = TRUE),
                  sd = sd(elsa$sbp[elsa$sex == "male"], na.rm = TRUE)))
t.test(sbp ~ sex, elsa,na.rm=TRUE,var.equal = FALSE)

model1 <- lm(sbp ~ sex, elsa)
summary(model1)
confint(model1)

ggplot(elsa) +
  aes(x = sex, y = sbp, color = sex) +
  geom_jitter(alpha = 0.2) +
  geom_boxplot(fill = NA)

coef(model1)["(Intercept)"] + coef(model1)["sexfemale"]*0
coef(model1)["(Intercept)"] + coef(model1)["sexfemale"]*1
t.test(sbp ~ sex, elsa)

freq(elsa$sclass,cumul=FALSE)

elsa <- elsa %>%
  mutate(sclass_n = as.numeric(sclass),
         sclass_3 = case_when(
           sclass_n %in% 1:2 ~ "Prof/Managerial",
           sclass_n == 3  ~ "Skilled Non-Manual",
           sclass_n %in% 4:6 ~ "Manual/Routine") %>%
           factor(c(
             "Prof/Managerial", 
             "Skilled Non-Manual", 
             "Manual/Routine")))
table(elsa$sclass, elsa$sclass_3, useNA = "no")
psych::describeBy(elsa$sbp, elsa$sclass_3)

aov_res <- stats::aov(sbp ~ sclass_3, elsa)
summary(aov_res)
bartlett.test(sbp ~ sclass_3, data = elsa)

model2 <- lm(sbp ~ sclass_3, elsa)
summary(model2)
confint(model2)

#check labels for hypothesis test
model2$coefficients
car::linearHypothesis(model2, c(
  "sclass_3Skilled Non-Manual = 0",
  "sclass_3Manual/Routine = 0"))

coef(model2)
coef(model2)[1]                     # Professional/managerial
coef(model2)[1] + coef(model2)[2]  # Skilled non-manual
coef(model2)[1] + coef(model2)[3]      # Manual/routine

#attach labels to wealth for use later on.
freq(elsa$wealth5)
descr(elsa[c("age", "sbp")])
freq(elsa[c("sex", "sclass_3", "wealth5", "smok_bin")],cumul=FALSE)

#data with complete cases only
elsa2 <- elsa %>%
  select(age, sbp, sex, sclass_3, wealth5, smok_bin) %>%
  tidyr::drop_na() 
base::nrow(elsa2)
model3 <- lm(sbp ~ sex + sclass_3 + age, elsa2)
summary(model3)
confint(model3)
coef(model3)
coef(model3)[1] + coef(model3)[2] + coef(model3)[4] + (coef(model3)[5]*55)

glht(model3, 
     "`(Intercept)` + (55*`age`) + (`sexfemale`) + 
     (`sclass_3Manual/Routine`) = 0") %>%
  confint()

model4 <- lm(sbp ~ sex + sclass_3 + age + wealth5, elsa2)
summary(model4)
freq(elsa2$wealth5)

#RMSE is just the square-root of the mean of the squared residuals.

sqrt(mean(model3$residuals^2))  # model without wealth
sqrt(mean(model4$residuals^2))  # model with wealth

#check labels for hypothesis test
model4$coefficients

linearHypothesis(model4, c(
  "wealth52 = 0",
  "wealth53 = 0",
  "wealth54 = 0",
  "wealth5highest = 0"))


model5 <- lm(sbp ~ sex + sclass_3 + age + 
               wealth5 + smok_bin, elsa2)
summary(model5)
confint(model5)
sqrt(mean(model5$residuals^2))

#non-linear
model_n1 <- lm(chol ~ age + sex + bmi, elsa)
summary(model_n1)
model_n2 <- lm(chol ~ age + sex + bmi + I(bmi^2), elsa)
summary(model_n2)
sqrt(mean(model_n1$residuals^2))
sqrt(mean(model_n2$residuals^2))

freq(elsa$bmi4,cumul=FALSE)
elsa <- elsa %>%
  mutate(bmi3 = fct_collapse(bmi4, 
                             "Less than 25" = levels(bmi4)[1:2],
                             "25-30" = levels(bmi4)[3:3],
                             "30+" = levels(bmi4)[4:4]))
model_n3 <- lm(chol ~ age + sex + factor(bmi3), elsa)
sqrt(mean(model_n2$residuals^2))
sqrt(mean(model_n3$residuals^2))

###################
#Linear regression 3
###################

sjmisc::descr(elsa$sbp)
sum(is.na(elsa$sbp))   # number of missing values
ggplot(elsa) +
  aes(x = sbp) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(
    mean = mean(elsa$sbp, na.rm = TRUE),
    sd = sd(elsa$sbp, na.rm = TRUE)))

sjmisc::descr(elsa$age)
ggplot(elsa) +
  aes(x = age) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(
    mean = mean(elsa$age, na.rm = TRUE),
    sd = sd(elsa$age, na.rm = TRUE)))

elsa %>%
  # use filter to keep the correct observations
  filter(!is.na(sbp)) %>% 
  freq(sex)

model1 <- lm(sbp ~ age, elsa)
summary(model1)

lm(sbp ~ age*sex, elsa)
lm(sbp ~ age + sex + age:sex, elsa)

model2 <- lm(sbp ~ age*sex, elsa)
summary(model2)
confint(model2)

#intercept for women:
glht(model2, "`(Intercept)` + `sexfemale` = 0") %>%
  confint()
#slope for women:
glht(model2, "`age` + `age:sexfemale` = 0") %>%
confint()

summary(lm(sbp~age,data=base::subset(elsa,sex=="male")))   
summary(lm(sbp~age,data=base::subset(elsa,sex=="female")))

elsa %>%
  # use filter to keep the correct observations
  filter(!is.na(sbp)) %>% 
  freq(wealth5)

model3 <- lm(sbp ~ sex*wealth5, elsa)
summary(model3)
confint(model3)

#check labels for hypothesis test
model3$coefficients
linearHypothesis(model3, c(
  "sexfemale:wealth52 = 0",
  "sexfemale:wealth53 = 0",
  "sexfemale:wealth54 = 0",
  "sexfemale:wealth5highest = 0"))

summary(lm(sbp~wealth5,data=base::subset(elsa,sex=="male")))   
summary(lm(sbp~wealth5,data=base::subset(elsa,sex=="female")))

sjmisc::descr(elsa$bmi)
sum(is.na(elsa$bmi))
ggplot(elsa) +
  aes(x = bmi) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(
    mean = mean(elsa$bmi, na.rm = TRUE),
    sd = sd(elsa$bmi, na.rm = TRUE)))
freq(elsa$smok,cumul=FALSE)
freq(elsa$sex,cumul=FALSE)

elsa <- elsa %>%
  mutate(sclass_n = as.numeric(sclass),
         sclass_3 = case_when(
           sclass_n %in% 1:2 ~ "Prof/Managerial",
           sclass_n == 3  ~ "Skilled Non-Manual",
           sclass_n %in% 4:6 ~ "Manual/Routine") %>%
           factor(c(
             "Prof/Managerial", 
             "Skilled Non-Manual", 
             "Manual/Routine")))
freq(elsa$sclass_3,cumul=FALSE)


freq(elsa$physact,cumul=FALSE)
elsa <- elsa %>%
  mutate(physact_n = as.numeric(physact),
         low_phy = case_when(
           physact_n %in% 2:3 ~ "Not low",
           physact_n == 1  ~ "Low") %>%
           factor(c("Not low", "Low")))
freq(elsa$low_phy,cumul=FALSE)

sum(is.na(elsa$bmi)|is.na(elsa$smok)|is.na(elsa$sclass_3)|is.na(elsa$low_phy))
#dataframe of complete cases and just the variables we need
elsa2 <- elsa %>%
  select(bmi, smok, sex, sclass_3, low_phy) %>%
  tidyr::drop_na() 
head(elsa2)
base::nrow(elsa2)

psych::describeBy(elsa2$bmi, elsa2$smok)

model1 <- lm(bmi ~ smok, elsa2)
summary(model1)
confint(model1) 

#check labels for hypothesis test
model1$coefficients

linearHypothesis(model1, c(
  "smokUsed to smoke cigarettes occasionally = 0",
  "smokUsed to smoke cigarettes regularly = 0",
  "smokCurrent cigarette smoker = 0"))

t.test(bmi ~ sex, elsa2,var.equal=TRUE)
# linear regression to obtain the same result
model2 <- lm(bmi ~ sex, elsa2)
summary(model2)
confint(model2) 

model3 <- lm(bmi ~ sex*smok, elsa2)
summary(model3)
confint(model3) 

#check labels for hypothesis test
model3$coefficients

#Wald test of the interaction term
linearHypothesis(model3, c(
  "sexfemale:smokUsed to smoke cigarettes occasionally = 0",
  "sexfemale:smokUsed to smoke cigarettes regularly = 0",
  "sexfemale:smokCurrent cigarette smoker = 0"))

model4 <- lm(bmi ~ sclass_3, elsa2)
summary(model4)
confint(model4) 
model4$coefficients
linearHypothesis(model4, c(
  "sclass_3Skilled Non-Manual = 0",
  "sclass_3Manual/Routine = 0"))

model5 <- lm(bmi ~ sclass_3*smok, elsa2)
summary(model5)
confint(model5) 
model5$coefficients

linearHypothesis(model5, c(
  "sclass_3Skilled Non-Manual:smokUsed to smoke cigarettes occasionally = 0",
  "sclass_3Manual/Routine:smokUsed to smoke cigarettes occasionally = 0",
  "sclass_3Skilled Non-Manual:smokUsed to smoke cigarettes regularly = 0",
  "sclass_3Manual/Routine:smokUsed to smoke cigarettes regularly = 0",
  "sclass_3Skilled Non-Manual:smokCurrent cigarette smoker = 0",
  "sclass_3Manual/Routine:smokCurrent cigarette smoker = 0"))

model6 <- lm(bmi ~ smok + sclass_3, elsa2)
summary(model6)
confint(model6) 
model7 <- lm(bmi ~ low_phy, elsa2)
summary(model7)
confint(model7) 
ctable(elsa2$smok, elsa2$low_phy, prop="row",useNA="no", chisq = TRUE)
model8 <- lm(bmi ~ smok*low_phy, elsa2)
summary(model8)
confint(model8) 
model8$coefficients
linearHypothesis(model8, c(
  "smokUsed to smoke cigarettes occasionally:low_phyLow = 0",
  "smokUsed to smoke cigarettes regularly:low_phyLow = 0",
  "smokCurrent cigarette smoker:low_phyLow = 0"))

model9 <- lm(bmi ~ sclass_3 + smok*low_phy, elsa2)
summary(model9)
confint(model9)
model9$coefficients
linearHypothesis(model9, c(
  "smokUsed to smoke cigarettes occasionally:low_phyLow = 0",
  "smokUsed to smoke cigarettes regularly:low_phyLow = 0",
  "smokCurrent cigarette smoker:low_phyLow  = 0"))

summary(lm(bmi~sclass_3 + smok,data=subset(elsa2,low_phy=="Not low"))) 
summary(lm(bmi~sclass_3 + smok,data=subset(elsa2,low_phy=="Low"))) 


#transformations.
ggplot(elsa) +
  aes(x = crp) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(color = "red") +
  stat_function(fun = dnorm, args = list(
    mean = mean(elsa$crp, na.rm = TRUE),
    sd = sd(elsa$crp, na.rm = TRUE)))

describedata::gladder(elsa$crp)
describedata::ladder(elsa$crp)

########################################
#confounding and Effect modification.
########################################
#confEM<- haven::read_dta('teaching_confEM.dta') 

freq(confEM$srh,cumul=FALSE)
confEM <- confEM %>%
  mutate(srhnew = case_when(srh ==1 ~ "very good/good",
                            srh ==2 ~ "very good/good",
                            srh ==3 ~ "fair-very poor",
                            srh ==4 ~ "fair-very poor",
                            srh ==5 ~ "fair-very poor") %>%
           factor(c("very good/good", "fair-very poor"))) 
table(confEM$srhnew, confEM$srh)

confEM$partner <- factor(confEM$partner,
                         levels = c(0,1),
                         labels = c("partner", "no partner"))
freq(confEM$partner,cumul=FALSE)

freq(confEM$srhnew,cumul=FALSE)
summarytools::ctable(confEM$srhnew,confEM$partner,prop = "c",chisq=TRUE, 
                     useNA="no")
#no partner as exposed
mStats::mhor(confEM,partner,srhnew, 
             exp_value="no partner",
             case_value = "fair-very poor",
             digits=2)

freq(confEM$employ)
confEM$employ <- factor(confEM$employ,
                        levels = c(0,1),
                        labels = c("working", "not working"))
freq(confEM$employ)
summarytools::ctable(confEM$srhnew,confEM$employ,prop = "c",
                     chisq=TRUE, 
                     useNA="no")

summarytools::ctable(confEM$partner,confEM$employ,prop = "c",chisq=TRUE, 
                     useNA="no")

#no partner as exposed
mStats::mhor(confEM,partner,srhnew, 
             strata=employ,
             exp_value="no partner",
             case_value = "fair-very poor",
             digits=2)

freq(confEM$urban)
confEM$urban <- factor(confEM$urban,
                       levels = c(1,2,3),
                       labels = c("Densely-pop", "intermed", "thinly-pop"))
freq(confEM$urban)

#no partner as exposed
mStats::mhor(confEM,partner,srhnew, 
             strata=urban,
             exp_value="no partner",
             case_value = "fair-very poor",
             digits=2)

freq(confEM$sex)
confEM$sex <- factor(confEM$sex,
                     levels = c(0,1),
                     labels = c("male", "female"))
freq(confEM$sex)

#no partner as exposed
mStats::mhor(confEM,partner,srhnew, 
             strata=sex,
             exp_value="no partner",
             case_value = "fair-very poor",
             digits=2)

#men: compare partnership status after controlling for employment status
  confEM %>%
    dplyr::filter(sex=="male") %>% 
    mStats::mhor(.,partner,srhnew,
                 strata=employ,
                 exp_value="no partner",
                 case_value = "fair-very poor",
                 digits=2)
  
#women: compare partnership status after controlling for employment status
  confEM %>%
    dplyr::filter(sex=="female") %>% 
    mStats::mhor(.,partner,srhnew,
                 strata=employ,
                 exp_value="no partner",
                 case_value = "fair-very poor",
                 digits=2)
 
# sex treated as a confounder and control for employment status
  model1 <- glm(srhnew ~ partner + sex + employ,
                data = confEM,
                family = binomial(link = "logit"))

# sex treated as an effect modifier and control for employment status
  model2 <- glm(srhnew ~ partner + sex + employ + sex*partner,
                data = confEM,
                family = binomial(link = "logit"))

#likelihood-ratio test
  lrtest(model1, model2)
 
freq(confEM$back,cumul=FALSE)
confEM$back <- factor(confEM$back,
                        levels = c(0,1),
                        labels = c("no", "yes"))
freq(confEM$back,cumul=FALSE)

#crude association
  mStats::mhor(confEM,partner,back, 
               exp_value="no partner",
               case_value = "yes",
               digits=2)
 
#The role of sex
  mStats::mhor(confEM,partner,back,
               strata=sex,
               exp_value="no partner",
               case_value = "yes",
               digits=2)
  
  freq(confEM$educ,cumul=FALSE)
  confEM$educ <- factor(confEM$educ,
                        levels = c(0,1,2,3),
                        labels = c("primary", "secondary", 
                                   "tertiary", "university"))
  freq(confEM$educ,cumul=FALSE)
  
  #The role of education
  mStats::mhor(confEM,partner,back,
               strata=educ,
               exp_value="no partner",
               case_value = "yes",
               digits=2)
  
  freq(confEM$smok,cumul=FALSE)
  confEM$smok <- factor(confEM$smok,
                        levels = c(1,2,3),
                        labels = c("daily-smoker", "occasional", 
                                   "no smoking"))
  freq(confEM$smok,cumul=FALSE)
  
  #The role of smoking status
  mStats::mhor(confEM,partner,back,
               strata=smok,
               exp_value="no partner",
               case_value = "yes",
               digits=2)

  
confEM$fruit <- factor(confEM$fruit,
                         levels = c(0,1),
                         labels = c("<1x per week", ">=1x per week"))
class(confEM$fruit)
table(confEM$fruit)
  

 
  #The role of fruit consumption
  mStats::mhor(confEM,partner,back,
               strata=fruit,
               exp_value="no partner",
               case_value = "yes",
               digits=2)
 
  #men: compare partnership status controlling for educ
  confEM %>%
    dplyr::filter(sex=="male") %>% 
    mStats::mhor(.,partner,back,
                 strata=educ,
                 exp_value="no partner",
                 case_value = "fair-very poor",
                 digits=2)

  #wommen: compare partnership status controlling for educ
  confEM %>%
    dplyr::filter(sex=="female") %>% 
    mStats::mhor(.,partner,back,
                 strata=educ,
                 exp_value="no partner",
                 case_value = "fair-very poor",
                 digits=2)
  
  # sex as a confounder after controlling for educ
  model3 <- glm(back ~ partner + sex + educ,
                data = confEM,
                family = binomial(link = "logit"))
 
 # sex as an effect modifier after controlling for educ
  model4 <- glm(back ~ partner + sex + educ + sex*partner,
                data = confEM,
                family = binomial(link = "logit"))
 
  #likelihood-ratio test
  lrtest(model3, model4)
  
 ##########################
#Complex survey analysis
###########################

elsa_d <- elsa %>% 
  as_survey_design(ids = hh_id, 
                   weight = sampling_weight, 
                   strata = gor)
summary(elsa_d)
t.test(elsa$sbp)

elsa %>%
  select(sbp) %>%
  drop_na() %>%
  summarise(moe = 1.96*sd(sbp)/sqrt(n()))

#summarise, not summarize.
sbp_mean <- elsa_d %>%
  summarise(sbp = srvyr::survey_mean(sbp, 
                                     na.rm = TRUE, 
                                     vartype = c("se", "ci"))) %>%
as.data.frame(sbp_mean)
sbp_mean

sbp_mean
MoE_complex<-(1.96*sbp_mean$sbp_se)
MoE_complex

deff <- elsa_d %>%
  summarise(sbp = srvyr::survey_mean(sbp, 
                                     na.rm = TRUE, 
                                     deff = "replace")) %>%
as.data.frame(deff)
deff

sbp_bybmi <- elsa_d %>%
  filter(!is.na(bmi4)) %>%
  group_by(bmi4) %>%
  dplyr::summarize(sbp = srvyr::survey_mean(sbp, 
                                            na.rm = TRUE, 
                                            vartype = "ci"),
                   n = n()) %>%
  as.data.frame(sbp_bybmi)
sbp_bybmi

elsa_d %>%
  filter(bmi4 %in% levels(bmi4)[c(2, 4)]) %>%
  survey::svyttest(sbp ~ bmi4, .)

#assuming SRS.
model1 <- lm(sbp ~ age, elsa)
summary(model1)

(model1a <- svyglm(sbp ~ age,elsa_d))
summary(model1a)
confint(model1a)

#assuming SRS.
model2 <- lm(sbp ~ age*sex, elsa)
summary(model2)
summary(lm(sbp~age,data=base::subset(elsa,sex=="male")))  
summary(lm(sbp~age,data=base::subset(elsa,sex=="female")))

(model2a <- svyglm(sbp ~ age,elsa_d))
summary(model2a)
confint(model2a)
                                      
#new design object: subpop is men.
elsa_d_male <- subset(elsa_d , sex=="male" )
#now the svyglm.
(model2a_men <- svyglm(sbp ~ age,elsa_d_male))
              summary(model2a_men)
              confint(model2a_men)
                                      
#new design object: subpop is women.
elsa_d_female <- subset(elsa_d , sex=="female" )
#now the svyglm.
(model2a_women <- svyglm(sbp ~ age,elsa_d_female))
summary(model2a_women)
confint(model2a_women)
                                      
                                      
#assuming SRS.
cvd <- table(elsa$past_cvd)
cvd
prop.test(cvd) 
                                                                            
#include a new variable in the design object.
elsa_d <- elsa_d %>%
mutate(cvd_hist = 2 - as.numeric(past_cvd)) 
                                                                            
survey::svyciprop(~ cvd_hist, elsa_d,na.rm = TRUE) 
                                                                            
elsa_d %>%
summarise(cvd_hist = srvyr::survey_mean(cvd_hist, 
          na.rm = TRUE, 
            proportions = TRUE, 
    vartype = "ci"))
                                                                            
survey::svymean(~cvd_hist, elsa_d, na.rm=TRUE, deff = "replace") 
                                                                             
elsa_d %>%
filter(!is.na(bmi4)) %>%
group_by(bmi4) %>%
summarise(cvd_hist = srvyr::survey_mean(cvd_hist, 
          na.rm = TRUE, 
          proportions = TRUE, 
    vartype = "ci"))
                                                                            
survey::svychisq(~ cvd_hist + bmi4, elsa_d)

####################
#Power & sample size
####################

means <- c(48.7, 47)
sds <- c(17.6, 17.7)
n <- c(2000, 2000)
pool_sd <- sqrt(sum(sds^2)/2)       # pooled SD
d <- (means[1] - means[2])/pool_sd  # Effect size
pwr.t.test(n = n, d = d, sig.level = 0.05)

pwr.t.test(n = n, d = d, sig.level = 0.01)


        #n=1500; alpha=5%
                means <- c(48.7, 47)
                sds <- c(17.6, 17.7)
                n <- c(1500, 1500)
                pool_sd <- sqrt(sum(sds^2)/2)       # pooled SD
                d <- (means[1] - means[2])/pool_sd  # Effect size
                pwr.t.test(n = n, d = d, sig.level = 0.05)
                
                #n=1500; alpha=1%
                means <- c(48.7, 47)
                sds <- c(17.6, 17.7)
                n <- c(1500, 1500)
                pool_sd <- sqrt(sum(sds^2)/2)       # pooled SD
                d <- (means[1] - means[2])/pool_sd  # Effect size
                pwr.t.test(n = n, d = d, sig.level = 0.01)
                
#n=2500; alpha=5%  
means <- c(48.7, 47)
sds <- c(17.6, 17.7)
n <- c(2500, 2500)
pool_sd <- sqrt(sum(sds^2)/2)       # pooled SD
d <- (means[1] - means[2])/pool_sd  # Effect size
pwr.t.test(n = n, d = d, sig.level = 0.05)

#n=2500; alpha=1% 
means <- c(48.7, 47)
sds <- c(17.6, 17.7)
n <- c(2500, 2500)
pool_sd <- sqrt(sum(sds^2)/2)       # pooled SD
d <- (means[1] - means[2])/pool_sd  # Effect size
pwr.t.test(n = n, d = d, sig.level = 0.01)

get_d <- function(n, means, sds){
  pool_sd <- sqrt(sum((n-1)*(sds^2))/(sum(n)-2))  
  d <- (means[1] - means[2])/pool_sd
}

#n=2000 in each group; alpha=1% 
means <- c(48.7, 47)
sds <- c(17.6, 17.7)
n <- c(2000, 2000)
d <- get_d(n, means, sds)
pwr.t2n.test(n1 = n[1], n2 = n[2], d = d, sig.level = 0.01)

#n1=1500; n2=2500;alpha=1% 
means <- c(48.7, 47)
sds <- c(17.6, 17.7)
n <- c(1500, 2500)
d <- get_d(n, means, sds)
pwr.t2n.test(n1 = n[1], n2 = n[2], d = d, sig.level = 0.01)

#n1=500; n2=3500;alpha=1% 
means <- c(48.7, 47)
sds <- c(17.6, 17.7)
n <- c(500, 3500)
d <- get_d(n, means, sds)
pwr.t2n.test(n1 = n[1], n2 = n[2], d = d, sig.level = 0.01)

men <- c(3981, 1205)
women <- c(4288, 1917)
men[2]/sum(men)
women[2]/sum(women)

men_depress <- men[2]/sum(men)
women_depress <- women[2]/sum(women)

pwr.2p2n.test(h = ES.h(men_depress, women_depress), 
              n1 = sum(men), n2 = sum(women),
              sig.level = 0.05)


n <- c(1000, 1000)
means <- c(5.7, 5.9)
sds <- c(1.3, 1.2)
d <- get_d(n, means, sds) # effect size
pwr.t.test(d = d, power = 0.8,sig.level=0.05)

n <- c(1000, 1000)
means <- c(5.7, 5.9)
sds <- c(1.3, 1.2)
d <- get_d(n, means, sds)
pwr.t.test(d = d, power = 0.9,sig.level=0.05)

# difference of 1 unit, 80% power, 5% sig.level
# d = mean difference / SD.
pwr.t.test(d = 1/4.6, power = 0.8, sig.level = 0.05)  

# difference of 1 unit, 80% power, 1% sig.level
# d = mean difference / SD.
pwr.t.test(d = 1/4.6, power = 0.8, sig.level = 0.01)  
 
# difference of 1 unit, 90% power, 5% sig.level
# d = mean difference / SD.
pwr.t.test(d = 1/4.6, power = 0.9, sig.level = 0.05)  

#80% power
pwr.2p.test(ES.h(0.105, 0.12), power = 0.8, sig.level=0.05)

#90% power
pwr.2p.test(ES.h(0.105, 0.12), power = 0.9, sig.level=0.05)

pwr.2p.test(ES.h(0.34, 0.29), power = 0.9, sig.level=0.05)

print(paste("Epidemiology finished at", format(Sys.time(), 
                                      "%Y-%m-%d %H:%M:%S")))

##### FINISHED #####








































