
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
library(WeightIt)
library(estimatr)

#############
#IPW.
#############
#Example 1.

#data2 <- haven::read_dta("data/cattaneo2.dta")
#Github location.
url_stem <- "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
data2 <-haven::read_dta(glue(url_stem,'Data/cattaneo2.dta'))
head(data2)

# Rename 'mbsmoke' to 'treat'
data2 <- data2 %>% rename(treat = mbsmoke)
# Propensity score model
ps_model <- glm(treat ~ fage + mage + mmarried + fbaby, data = data2, family = binomial())
# Predict propensity scores
data2$pscore <- predict(ps_model, type = "response")

# IP weights for POMs and ATE
data2$ipw <- ifelse(data2$treat == 1, 1 / data2$pscore, 1 / (1 - data2$pscore))
# Define survey design
design_ate <- svydesign(ids = ~1, weights = ~ipw, data = data2)

# Potential means by treatment group
svyby(~bweight, ~treat, design_ate, svymean)

# Estimate ATE as the difference in means
ate <- svyglm(bweight ~ treat, design = design_ate)
summary(ate)

# IP weights for ATET
data2$ipw2 <- ifelse(data2$treat == 1, 1, data2$pscore / (1 - data2$pscore))
# Design for ATET
design_atet <- svydesign(ids = ~1, weights = ~ipw2, data = data2)

# Estimate group means
svyby(~bweight, ~treat, design_atet, svymean)

# ATET from regression
atet <- svyglm(bweight ~ treat, design = design_atet)
summary(atet)

#alternative.
library(WeightIt)
library(estimatr)

# Weighting for ATE
w_ate <- weightit(treat ~ fage + mage + mmarried + fbaby, data = data2, 
                  method = "ps", estimand = "ATE")
data2$w_ate <- w_ate$weights

lm_ate <- lm_robust(bweight ~ treat, weights = w_ate, data = data2)
summary(lm_ate)

# Weighting for ATET
w_atet <- weightit(treat ~ fage + mage + mmarried + fbaby, 
                   data = data2, method = "ps", estimand = "ATT")
data2$w_atet <- w_atet$weights

lm_atet <- lm_robust(bweight ~ treat, weights = w_atet, data = data2)
summary(lm_atet)

############
#Example 2.
############

set.seed(6795)
# Set number of observations
n <- 1000
# Create dataset
id <- 1:n
treat <- runif(n) < 0.2
male <- runif(n) < 0.5
outcome <- sample(1:10, n, replace = TRUE)
x <- sample(1:10, n, replace = TRUE)
# Combine into a data frame
df <- data.frame(id, treat, male, outcome, x)
head(df)

#model for the Tx in the denominator.
tx <- ipwpoint(
exposure = treat,
family = "binomial",
link = "logit",
numerator = ~ 1,
denominator = ~ x + male,
data = df)
summary(tx$ipw.weights)
df$ipw <- tx$ipw.weights
  
#ATE:
msm <- (svyglm(outcome ~ treat, 
design = svydesign(~ 1, 
weights = ~ ipw,
data = df)))
print(coef(msm))
print(confint(msm))

print(paste("IPW estimator finished at", format(Sys.time(), 
                                                "%Y-%m-%d %H:%M:%S")))

##########.
#FINISHED. 
##########.


    
    