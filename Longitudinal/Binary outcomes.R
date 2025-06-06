##############################################.
#Binary outcomes in longitudinal data.
#Multilevel models.
#GEE.
##############################################.

.libPaths()
print(R.version.string)
Macdrive<-
  "/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/"
UCLdrive<-
  "C:/Users/xxxx/OneDrive - University College London/Learning/"
folder<-
  "Longitudinal/01. Binary outcomes"  
setwd(paste0(UCLdrive, folder))
#setwd(paste0(Macdrive, folder))

library(glmmTMB)
library(GGally)
library(haven)
library(dplyr)
library(tidyverse)

#################################
#Multilevel modelling.
#mixed-effects logistic regression.
#https://stats.oarc.ucla.edu/r/dae/mixed-effects-logistic-regression/
##################################.

require(ggplot2)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)
require(lattice)

################
#Example 1
################

df <- haven::read_dta("data/Bangladeshi mothers.dta")

#model 1.
m1 <- glmer(antemed ~ 1 + (1 | comm), family = binomial, data = df)
summary(m1)

#model 2.
# Calculate the mean of 'mage'
mean_mage <- mean(df$mage, na.rm = TRUE)
# Center 'mage' around its mean
df$magec <- df$mage - mean_mage
m2 <- glmer(antemed ~ 1 + + magec + wealth + urban + (1 | comm), 
            family = binomial, data = df)
summary(m2)
exp(coef(summary(m2)))

#model 3.
m3 <- glmer(antemed ~ magec + wealth + urban + (1 + wealth | comm), 
               family = binomial(link = "logit"), 
               data = df)
summary(m3)
exp(coef(summary(m3)))

# observed vs predicted.
# Fit the model
m3 <- glmer(antemed ~ magec + wealth + urban + (1 + wealth | comm), 
               data = df, family = binomial(link = "logit"))

# Predicted probabilities
df$pred_prob <- predict(m3, type = "response")

# Create deciles of predicted probabilities
df$prob_dec <- cut(df$pred_prob, 
                          breaks = quantile(df$pred_prob, 
                      probs = seq(0, 1, 0.1), na.rm = TRUE),
                          include.lowest = TRUE, labels = FALSE)
# Label the variable
attr(df$prob_dec, "label") <- "Predicted probability decile"
table(df$prob_dec)

# Summary table
summary_table <- df %>%
  dplyr::group_by(prob_dec) %>%
  dplyr::summarise(frequency = n(),
    mean_pred_prob = mean(pred_prob, na.rm = TRUE),
    mean_antemed = mean(antemed, na.rm = TRUE)
  )
print(summary_table)

??summarise
??group_by

# Collapse data by decile
collapsed_df <- df %>%
  dplyr::group_by(prob_dec) %>%
  dplyr::summarise(
    observed = mean(antemed, na.rm = TRUE),
    predicted = mean(pred_prob, na.rm = TRUE)
  )


# Calibration plot
ggplot(collapsed_df, aes(x = predicted, y = observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Predicted probability", y = "Observed proportion") +
  theme_minimal()

####################
#Additional example.
####################.


df <- read.csv("https://stats.idre.ucla.edu/stat/data/hdp.csv")
df <- within(df, {
  Married <- factor(Married, levels = 0:1, labels = c("no", "yes"))
  DID <- factor(DID)
  HID <- factor(HID)
  CancerStage <- factor(CancerStage)
})
head(df)


# Estimate the model
m1 <- glmer(remission ~ IL6 + CRP + CancerStage + LengthofStay + Experience +
              (1 | DID), 
            data = df, family = binomial, 
    control = glmerControl(optimizer = "bobyqa"),
            nAGQ = 10)

# print the mod results without correlations among fixed effects
print(m1, corr = FALSE)

se <- sqrt(diag(vcov(m1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m1), LL = fixef(m1) - 1.96 * se, 
          UL = fixef(m1) + 1.96 *se))
exp(tab)

###############################
# Estimate the model (3-level)
###############################

m2 <- glmer(remission ~ Age + LengthofStay + FamilyHx + IL6 + CRP +
              CancerStage + Experience + (1 | DID) + (1 | HID),
            data = df, family = binomial, nAGQ=1)
print(m2, corr=FALSE)

lattice::dotplot(ranef(m2, which = "DID", condVar = TRUE), 
                 scales = list(y = list(alternating = 0)))
lattice::dotplot(ranef(m2, which = "HID", condVar = TRUE))


# random slopes
# estimate the model and store results in m
m3 <- glmer(remission ~ Age + LengthofStay + FamilyHx + IL6 
            + CRP + CancerStage +
              Experience + (1 + LengthofStay | DID) + (1 | HID), 
            data = df, family = binomial,
            nAGQ = 1)
print(m3, corr = FALSE)

print(paste("Multilevel models binary outcomes finished at", 
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")))


#####################
#FINISHED
#####################


#########################
#GEE models.
#########################

#https://cehs-research.github.io/eBook_multilevel/gee-binary-outcome-respiratory-illness.html

library(tidyverse)    # all things tidy
library(pander)       # nice looking genderal tabulations
library(furniture)    # nice table1() descriptives
library(texreg)       # Convert Regression Output to LaTeX or HTML Tables
library(psych)        # contains some useful functions, like headTail
library(interactions)
library(performance)
library(lme4)         # Linear, generalized linear, & nonlinear mixed models
library(corrplot)     # Vizualize correlation matrix
library(gee)          # Genderalized Estimation Equation Solver
library(geepack)      # Genderalized Estimation Equation Package 
library(MuMIn)        # Multi-Model Inference (calculate QIC)
library(HSAUR)        # package with the dataset

remotes::install_github("sarbearschwartz/texreghelpr") # first time
library(texreghelpr)

data("respiratory", package = "HSAUR")
str(respiratory)
psych::headTail(respiratory)

##########################################.
#Wide format has one line per participant.
##########################################.

data_wide <- respiratory %>% 
  tidyr::spread(key = month,
                value = status,
                sep = "_") %>% 
  dplyr::rename("BL_status" = "month_0") %>% 
  dplyr::arrange(subject) %>% 
  dplyr::select(subject, centre, 
                sex, age, treatment, 
                BL_status, starts_with("month"))  
tibble::glimpse(data_wide)
psych::headTail(data_wide)

##########################################
#Long format has one line per observation.
##########################################
data_long <- data_wide%>% 
  tidyr::gather(key = month,
                value = status,
                starts_with("month")) %>% 
  dplyr::mutate(month = str_sub(month, start = -1) %>% as.numeric) %>% 
  dplyr::mutate(status = case_when(status == "poor" ~ 0,
                                   status == "good" ~ 1)) %>% 
  dplyr::arrange(subject, month) %>% 
  dplyr::select(subject, centre, sex, age, treatment, BL_status, month, status) 
tibble::glimpse(data_long)
psych::headTail(data_long)

#Export stata dataset.
haven::write_dta(data_long,"outputs/respiratory.dta")


#Demographics and Baseline Measure
data_wide %>% psych::describe(skew = FALSE)

data_wide %>% 
  dplyr::group_by(treatment) %>% 
  furniture::table1("Center" = centre, 
                    "Sex" = sex, 
                    "Age" = age, 
                    "Baseline Status" = BL_status, 
                    caption = "Participant Demographics",
                    output = "markdown",
                    na.rm = FALSE,
                    total = TRUE,
                    test = TRUE)

#Status over time.
data_wide %>% 
  dplyr::group_by(treatment) %>% 
  furniture::table1("Month One" = month_1, 
                    "Month Two" = month_2, 
                    "Month Three" = month_3, 
                    "Month Four" = month_4, 
                    caption = "Respiratory Status Over Time",
                    output = "markdown",
                    na.rm = FALSE,
                    total = TRUE,
                    test = TRUE)

#Correlation between repeated observations:
data_wide %>% 
  dplyr::select(starts_with("month")) %>% 
  dplyr::mutate_all(function(x) x == "good") %>% 
  cor() %>% 
  corrplot::corrplot.mixed()

#using long data.
data_month_trt_prop <- data_long %>% 
  dplyr::group_by(treatment, month) %>% 
  dplyr::summarise(n = n(),
                   prop_good = mean(status),
                   prop_sd = sd(status),
                   prop_se = prop_sd/sqrt(n))

data_month_trt_prop 

####################
#GEE: Independence.
####################

m1 <- gee::gee(status ~ centre + treatment + sex + BL_status + 
                 I(age-33) + I((age-33)^2),
               data = data_long,
               family = binomial(link = "logit"),
               id = subject,
               corstr = "independence",
               scale.fix = TRUE,
               scale.value = 1)
summary(m1)

####################
#GEE: Exchangeable.
####################

m2 <- gee::gee(status ~ centre + treatment + sex + BL_status + 
                 I(age-33) + I((age-33)^2),
               data = data_long,
               family = binomial(link = "logit"),
               id = subject,
               corstr = "exchangeable",
               scale.fix = TRUE,
               scale.value = 1) 
summary(m2)

###################.
#display estimates.
###################

texreg::knitreg(list(m1,m2),
                custom.model.names = c("GEE-INDEP", "GEE-EXCH"),
                caption = "Estimates on Logit Scale",
                single.row = TRUE,
                digits = 4)


texreg::knitreg(list(extract_gee_exp(m1), 
                     extract_gee_exp(m2)),
                custom.model.names = c("GEE-INDEP", "GEE-EXCH"),
                caption = "Estimates on Odds-Ratio Scale",
                single.row = TRUE,
                ci.test = 1,
                digits = 3)


#manual extraction
m1 %>% coef() %>% exp()
m2 %>% coef() %>% exp()


#After accounting for baseline status, age, sex and center, 
#participants in the active treatment group had nearly four times higher 
#odds of having ‘good’ respiratory status, when compared to the placebo, 
#exp(b) = 3.881, p<.001, 95% CI [1.85, 8.14].

###############
###############

#https://library.virginia.edu/data/articles/getting-started-with-
#generalized-estimating-equations

#We often model longitudinal or clustered data with mixed-effect or 
#multilevel models. 
#So how is GEE different? The main difference is that it’s a marginal model. 
#It seeks to model a population average. 
#Mixed-effect/multilevel models are subject-specific, or conditional, models. 
#They allow us to estimate different parameters for each subject or cluster. 
#In other words, the parameter estimates are conditional on the subject/cluster. 
#This in turn provides insight to the variability between subjects or clusters. 
#We can also obtain a population-level model from a mixed-effect model, but it’s 
#basically an average of the subject-specific models.

URL <- "http://static.lib.virginia.edu/statlab/materials/data/depression.csv"
dat <- read.csv(URL, stringsAsFactors = TRUE)
dat$id <- factor(dat$id)
dat$drug <- relevel(dat$drug, ref = "standard")
head(dat, n = 3)

library(magrittr)
with(dat, tapply(depression, list(diagnose, drug, time), mean)) %>% 
  ftable() %>% 
  round(2)

# independence; use robust SE.
#This indicates we assume no correlation between responses within subjects.
library(gee) 
m1 <- gee(depression ~ diagnose + drug*time,
          data = dat, 
          id = id, 
          family = binomial,
          corstr = "independence")
summary(m1)

# exchangeable; equally correlated
m2 <- gee(depression ~ diagnose + drug*time,
          data = dat, 
          id = id, 
          family = binomial,
          corstr = "exchangeable")
summary(m2)

#autoregressive structure
m3 <- gee(depression ~ diagnose + drug*time,
          data = dat, 
          id = id, 
          family = binomial,
          corstr = "AR-M", Mv = 1)
summary(m3)
m3$working.correlation

#unstructured” matrix. This allows all correlations to freely vary.
m4 <- gee(depression ~ diagnose + drug*time,
          data = dat, 
          id = id, 
          family = binomial,
          corstr = "unstructured")
summary(m4)
m4$working.correlation


library(lme4) 
m5 <- glmer(depression ~ diagnose + drug*time + (1|id), 
            data = dat, family = binomial)
summary(m5, corr = FALSE)

library(ggeffects) # version 1.2.0
plot(ggemmeans(m5, terms = c("time", "drug"), 
               condition = c(diagnose = "severe"))) + 
  ggplot2::ggtitle("GLMER Effect plot")


library(emmeans) # version 1.8.4-1
emm_out <- emmeans(m2, specs = c("time", "drug"), 
                   at = list(diagnose = "severe"), 
                   cov.keep = "time", 
                   regrid = "response") %>% 
  as.data.frame()

library(ggplot2) # version 3.4.1
ggplot(emm_out) +
  aes(x = time, y = prob, color = drug, fill = drug) +
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, 
                  color = NULL), alpha = 0.15) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = NULL) +
  scale_y_continuous(labels = scales::percent) +
  theme_ggeffects() +
  labs(title = "GEE Effect plot", y = "depression")

##

# Load necessary libraries
library(haven)     
library(dplyr)    
library(geepack)   

# Load dataset
data <- read_dta("data/respiratory.dta")

# Summarize age variable
summary(data$age)

# Transform variables
data <- data %>%
  mutate(age = age - 33,  # Centering age at 33
         agesq = age^2)   # Squaring age

# Fit GEE model (Independent correlation structure)
gee_model <- geeglm(status ~ centre + treatment + sex + BL_status + age + agesq, 
                    family = binomial(link = "logit"), 
                    id = subject, 
                    corstr = "independence", 
                    data = data)

# Display results as exponentiated coefficients (odds ratios)
exp(coef(summary(gee_model)))

print(paste("GEE for binary outcomes finished at", 
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

##################
#FINISHED
##################


















