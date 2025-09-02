##########################
#IPWRA estimator
#https://onlinelibrary.wiley.com/doi/full/10.1002/sim.9234
##########################

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
library(twang)
library(glue)

##################
#Example 1
##################

#data <- haven::read_dta("data/cattaneo2.dta")
url_stem <- "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
data <-haven::read_dta(glue(url_stem,'Data/cattaneo2.dta'))
head(data)

# Rename variables
data <- data %>%
  rename(treat = mbsmoke)

# Propensity score model for the treatment variable
ps_model <- glm(treat ~ mmarried + mage + fbaby + medu, 
                family = binomial(), data = data)

# Calculate propensity scores
data$pscore <- predict(ps_model, type = "response")

# Compute the inverse probability weights
data$ipw <- ifelse(data$treat == 0, 1 / (1 - data$pscore), 1 / data$pscore)

# Fit the regression for the treated group and the control group
model_treat <- lm(bweight ~ mage + prenatal1 + mmarried + fbaby, 
                  data = data[data$treat == 1, ], weights = ipw)
model_control <- lm(bweight ~ mage + prenatal1 + mmarried + fbaby, 
                    data = data[data$treat == 0, ], weights = ipw)
model_treat

data$pom_t<-(3201.664) + (-6.452*data$mage) + (26.590*data$prenatal1) + 
  (136.652*data$mmarried) + (50.282*data$fbaby)
length(data$pom_t)

model_control


data$pom_c<-(3187.79) + (3.15*data$mage) + (66.36*data$prenatal1) + 
  (156.70*data$mmarried) + (-71.50*data$fbaby)
length(data$pom_c)

# Summary statistics for treated and control group predictions
summary(data$pom_c)
summary(data$pom_t)

# Calculate the treatment effect (difference in predictions)
data$ate <- data$pom_t - data$pom_c
mean_ate <- mean(data$ate, na.rm = TRUE)
# Print the mean of the 'ate' variable
print(mean_ate)

##################
#Example 2
##################

#data <- haven::read_dta("data/cattaneo2.dta")
url_stem <- "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
data <-haven::read_dta(glue(url_stem,'Data/cattaneo2.dta'))
head(data)

# Rename variables
data <- data %>%
  rename(treat = mbsmoke)

# Install the twang package if not already installed
# install.packages("twang")

# Propensity score model for the treatment variable
ps_model <- glm(treat ~ fage + mage + fbaby + mmarried, 
                family = binomial(), data = data)

# Calculate propensity scores
data$pscore <- predict(ps_model, type = "response")

# Compute the inverse probability weights
data$ipw <- ifelse(data$treat == 0, 1 / (1 - data$pscore), 1 / data$pscore)

# Fit the regression for the treated group and the control group
model_treat <- lm(bweight ~ fage + mage + mmarried + prenatal1, 
                  data = data[data$treat == 1, ], weights = ipw)
model_control <- lm(bweight ~ fage + mage + mmarried + prenatal1 , 
                    data = data[data$treat == 0, ], weights = ipw)
model_treat

data$pom_t<-(3241.334) + (1.788*data$fage) + (-8.818*data$mage) 
+ (122.586*data$mmarried) + (28.622*data$prenatal1) 
length(data$pom_t)

model_control
data$pom_c<-(3114.5369) + (-0.5052*data$fage) + (5.4908*data$mage) 
+ (161.6814*data$mmarried) + (54.3625*data$prenatal1) 
length(data$pom_c)

# Summary statistics for treated and control group predictions
summary(data$pom_c)
summary(data$pom_t)

# Calculate the treatment effect (difference in predictions)
data$ate <- data$pom_t - data$pom_c
mean_ate <- mean(data$ate, na.rm = TRUE)
# Print the mean of the 'ate' variable
print(mean_ate)

print(paste("IPWRA finished at", format(Sys.time(), 
                                    "%Y-%m-%d %H:%M:%S")))

##########################
#IPWRA estimator finished.
##########################.




