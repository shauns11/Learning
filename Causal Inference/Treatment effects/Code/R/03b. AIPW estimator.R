########################
#AIPW estimator
########################

.libPaths()
print(R.version.string)
Macdrive <- "/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/"
UCLdrive <-"C:/Users/rmjdshc/OneDrive - University College London/Learning/"
folder <- "Causal Inference/Treatment Effects"  
setwd(paste0(UCLdrive, folder))

#setwd(paste0(Macdrive, folder))
# Load necessary libraries
library(haven)    # For reading Stata files
library(dplyr)    # For data manipulation
library(stats)    # For logistic regression and linear regression
library(glue)

# Step 1: Load the data and rename variables
#data <- haven::read_dta("data/cattaneo2.dta")
url_stem <- "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
data <-haven::read_dta(glue(url_stem,'Data/cattaneo2.dta'))
head(data)

data <- data %>%
  rename(treat = mbsmoke)  # Rename 'mbsmoke' to 'treat'

# Step 2: Estimate the propensity score using logistic regression
ps_model <- glm(treat ~ mmarried + mage + fbaby + medu, 
                family = binomial(), data = data)

# Calculate propensity scores
data$pscore <- predict(ps_model, type = "response")

# Step 3: Compute the inverse probability weights (IPW) for treated 
# and control groups
# IPW for control group
data$ipw0 <- ifelse(data$treat == 0, 1 / (1 - data$pscore), 0) 
# IPW for treated group
data$ipw1 <- ifelse(data$treat == 1, 1 / data$pscore, 0)       

# Step 4: Estimate the Potential Outcomes Model (POM) for treated group
# No weights in the outcome model
treated_data <- subset(data, treat == 1)

# Fit regression model for the treated group
model_treat <- lm(bweight ~ mage + prenatal1 + mmarried + fbaby, 
                  data = treated_data)

# Generate potential outcome for treated group (POM1)
data$pom1 <- 3227.169 + (41.43991 * data$fbaby) 
+ (133.6617 * data$mmarried) +
  (25.11133 * data$prenatal1) + (-7.370881 * data$mage)

# Adjust the prediction with the inverse probability weight (IPW)
data$pom1 <- data$pom1 + data$ipw1 * (data$bweight - data$pom1)

# Step 5: Estimate the Potential Outcomes Model (POM) for control group
control_data <- subset(data, treat == 0)

# Fit regression model for the control group
model_control <- lm(bweight ~ mage + prenatal1 + mmarried + fbaby, 
                    data = control_data)

# Generate potential outcome for control group (POM0)
data$pom0 <- 3202.746 + (-71.3286 * data$fbaby) 
+ (160.9513 * data$mmarried) +
  (64.40859 * data$prenatal1) + (2.546828 * data$mage)

# Adjust the prediction with the inverse probability weight (IPW)
data$pom0 <- data$pom0 + data$ipw0 * (data$bweight - data$pom0)

# Step 6: Calculate ATE using the potential outcomes
pom_c <- mean(data$pom0, na.rm = TRUE)  # Mean potential outcome for control 
pom_t <- mean(data$pom1, na.rm = TRUE)  # Mean potential outcome for treated 

# Calculate the ATE
ATE <- pom_t - pom_c

# Print the ATE
cat("AIPW =", ATE, "\n")

print(paste("AIPW finished at", format(Sys.time(), 
                            "%Y-%m-%d %H:%M:%S")))
##########.
#FINISHED.
##########.

