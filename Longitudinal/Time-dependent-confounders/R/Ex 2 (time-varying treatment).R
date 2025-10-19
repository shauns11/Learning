################################.
#IPTW (time-varying treatment).
################################.

library(haven)
library(ipw)
library(survey)
library(dplyr)
library(tidyverse)
library(tibble)

#Example 2a.
#df <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/01. IPTW/data/ipw_Ex2a_R.dta")
df <- haven::read_dta("N:/Learning/Longitudinal/01. IPTW/data/ipw_Ex2a_R.dta")
df = as.data.frame(df)

w<-ipw::ipwtm(
  exposure=treat,
  family="binomial",
   link="logit",
    numerator=~1 + sex,
    denominator=~1 + sex + x,
    id=id,
    timevar=time,
    type="first",
    data=df)
summary(w$ipw.weights)
df$ipw<-w$ipw.weights

msm <- (survey::svyglm(outcome ~ treat + time + sex, 
                       design = survey::svydesign(id =~id, 
weights = df$ipw,data = df)))
msm

###########.
#Example 2b.
###########.

#df <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/01. IPTW/data/ipw_Ex2b_R.dta")
df <- read_dta("N:/Learning/Longitudinal/01. IPTW/data/ipw_Ex2b_R.dta")
df = as.data.frame(df)

w<-ipw::ipwtm(
   exposure=treat,
    family="binomial",
    link="logit",
    numerator=~1 + male + age,
    denominator=~1 + male + age + v,
    id=id,
    timevar=time,
    type="first",
    data=df)
summary(w$ipw.weights)
df$ipw<-w$ipw.weights

msm <- (survey::svyglm(y ~ treat + time + male + age, 
            design = survey::svydesign(id =~id, weights = df$ipw,
                                               data = df)))
msm

##################
#FINISHED
##################
