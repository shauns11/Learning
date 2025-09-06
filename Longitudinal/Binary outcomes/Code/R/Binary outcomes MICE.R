##############################################.
#Binary outcomes in longitudinal data.
#Multilevel models and GEE.
##############################################.

.libPaths()
print(R.version.string)
Macdrive<-
  "/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/"
UCLdrive<-
  "C:/Users/rmjdshc/OneDrive - University College London/Learning/"
folder<-
  "Longitudinal/01. Binary outcomes"  
setwd(paste0(UCLdrive, folder))
#setwd(paste0(Macdrive, folder))

# Install if necessary
install.packages(c("mice", "lme4", "mitml", "miceadds"))

# Load libraries
library(mice)
library(lme4)
library(mitml)
library(miceadds)

# Simulated example
set.seed(123)
n_group <- 50
n_per_group <- 20
n <- n_group * n_per_group

data <- data.frame(
  group = factor(rep(1:n_group, each = n_per_group)),
  x1 = rnorm(n),
  x2 = sample(c(NA, 1:5), n, replace = TRUE),
  y = rbinom(n, 1, 0.4)
)

# Introduce some missingness
data$x1[sample(1:n, 100)] <- NA
head(data)

# Passive imputation of cluster ID as factor
data$group <- as.factor(data$group)

# Predictor matrix: avoid using outcome and ID as predictors
ini <- mice(data, maxit = 0)
pred <- ini$pred
pred[, "y"] <- 0  # do not use outcome in imputations
pred["group", ] <- -2  # specify group as a cluster-level variable

# Perform imputation
imp <- mice(data, m = 5, predictorMatrix = pred, 
            method = "pmm", seed = 123)

# Fit the model: binary outcome ~ x1 + x2 + (1 | group)
fit <- with(imp, glmer(y ~ x1 + x2 + (1 | group), family = binomial))

library(broom.mixed)
pooled <- pool(fit)  # where 'fit' is the mira object from with()
summary(pooled)

#odds ratios.
summary_pooled <- summary(pooled, conf.int = TRUE, exponentiate = TRUE)
print(summary_pooled)


print(paste("Binary outcomes MICE finished at", format(Sys.time(), 
                                                "%Y-%m-%d %H:%M:%S")))

##########.
#FINISHED. 
##########.



