################################.
#IPTW with cross-sectional data.
################################.

library(haven)
library(ipw)
library(survey)

#Ex 1a.

#df <- haven::read_dta("C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/01. IPTW/data/ipw_Ex1a_R.dta")
df <- haven::read_dta("N:/Learning/Longitudinal/01. IPTW/data/ipw_Ex1a_R.dta")
df = as.data.frame(df)

tx <- ipw::ipwpoint(exposure = Tx, family = "binomial", link = "logit",
    numerator = ~ 1, 
    denominator = ~ confvar, 
    data = df)
summary(tx$ipw.weights)
df$ipw <- tx$ipw.weights

msm <- (survey::svyglm(y ~ Tx, 
	design = survey::svydesign(~ 1, weights = ~ ipw,data = df)))
coef(msm)
confint(msm)


#Example 1b.

#df <- read_dta("C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/01. IPTW/data/ipw_Ex1b_R.dta")
df <- haven::read_dta("N:/Learning/Longitudinal/01. IPTW/data/ipw_Ex1b_R.dta")
df = as.data.frame(df)

#confounder in the denominator.

tx <- ipw::ipwpoint(exposure = Tx, family = "binomial", link = "logit",
numerator = ~ 1, 
denominator = ~ confvar + age, 
                 data = df)
summary(tx$ipw.weights)

#merge into dataframe
df$ipw <- tx$ipw.weights

#run model.
msm <- (survey::svyglm(y ~ Tx, 
	design = survey::svydesign(~ 1, weights = ~ ipw,data = df)))
coef(msm)
confint(msm)

##############
#FINISHED.
##############

