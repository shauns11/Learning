##############################################################################
#Competing risks PH regression
#https://argoshare.is.ed.ac.uk/healthyr_book/get-and-check-the-data.html
#Saves Stata file and shows equivalent Stata code for models 
#(2023/06/09)
##############################################################################

library(tidyverse)
library(finalfit)
library(dplyr)
library(forcats)
library(survival)
melanoma <- boot::melanoma 

melanoma <- melanoma %>%
  mutate(
    # Overall survival
    status_os = if_else(status == 2, 0, # "still alive"
                        1), # "died of melanoma" or "died of other causes"
    
    # Diease-specific survival
    status_dss = if_else(status == 2, 0, # "still alive"
                         if_else(status == 1, 1, # "died of melanoma"
                                 0)), # "died of other causes is censored"
    
    # Competing risks regression
    status_crr = if_else(status == 2, 0, # "still alive"
                         if_else(status == 1, 1, # "died of melanoma"
                                 2)), # "died of other causes"
    
    # Label and recode other variables
    age = ff_label(age, "Age (years)"), 
    thickness = ff_label(thickness, "Tumour thickness (mm)"),
    sex = factor(sex) %>% 
      fct_recode("Male" = "1", 
                 "Female" = "0") %>% 
      ff_label("Sex"),
    ulcer = factor(ulcer) %>% 
      fct_recode("No" = "0",
                 "Yes" = "1") %>% 
      ff_label("Ulcerated tumour")
  )

survival_object <- melanoma %$% 
  Surv(time, status_os)

# Explore:
head(survival_object) # + marks censoring, in this case "Alive"

# Expressing time in years
survival_object <- melanoma %$% 
  Surv(time/365, status_os)

# Overall survival in whole cohort
my_survfit <- survfit(survival_object ~ 1, data = melanoma)
my_survfit # 205 patients, 71 events

summary(my_survfit, times = c(0, 1, 2, 3, 4, 5))

dependent_os <- "Surv(time/365, status_os)"
explanatory  <- c("ulcer")

melanoma %>% 
  surv_plot(dependent_os, explanatory, pval = TRUE)

coxph(Surv(time, status_os) ~ age + sex + thickness + ulcer, data = melanoma) %>% 
  summary()

dependent_os  <- "Surv(time, status_os)"
dependent_dss <- "Surv(time, status_dss)"
dependent_crr <- "Surv(time, status_crr)"
explanatory   <- c("age", "sex", "thickness", "ulcer")

melanoma %>% 
  finalfit(dependent_os, explanatory)

melanoma %>% 
  finalfit(dependent_os, explanatory, add_dependent_label = FALSE) %>% 
  rename("Overall survival" = label) %>% 
  rename(" " = levels) %>% 
  rename("  " = all)
explanatory_multi <- c("age", "thickness", "ulcer")
melanoma %>% 
  finalfit(dependent_os, explanatory, 
           explanatory_multi, keep_models = TRUE)
explanatory <- c("age", "sex", "thickness", "ulcer", "year")
melanoma %>% 
  coxphmulti(dependent_os, explanatory) %>% 
  cox.zph() %>% 
  {zph_result <<- .} %>% 
  plot(var=5)
zph_result
explanatory <- c("age", "sex", "ulcer", "thickness", 
                 "strata(year)")
melanoma %>% 
  finalfit(dependent_os, explanatory)

# Simulate random hospital identifier
melanoma <- melanoma %>% 
  mutate(hospital_id = c(rep(1:10, 20), rep(11, 5)))

# Cluster model
explanatory <- c("age", "sex", "thickness", "ulcer", 
                 "cluster(hospital_id)")
melanoma %>% 
  finalfit(dependent_os, explanatory)

melanoma %>% 
  hr_plot(dependent_os, explanatory)

explanatory   <- c("age", "sex", "thickness", "ulcer")
dependent_dss <- "Surv(time, status_dss)"
dependent_crr <- "Surv(time, status_crr)"

melanoma %>%
  # Summary table
  summary_factorlist(dependent_dss, explanatory, 
                     column = TRUE, fit_id = TRUE) %>%
  # CPH univariable
  ff_merge(
    melanoma %>%
      coxphmulti(dependent_dss, explanatory) %>%
      fit2df(estimate_suffix = " (DSS CPH univariable)")
  ) %>%
  # CPH multivariable
  ff_merge(
    melanoma %>%
      coxphmulti(dependent_dss, explanatory) %>%
      fit2df(estimate_suffix = " (DSS CPH multivariable)")
  ) %>%
  # Fine and Gray competing risks regression
  ff_merge(
    melanoma %>%
      crrmulti(dependent_crr, explanatory) %>%
      fit2df(estimate_suffix = " (competing risks multivariable)")
  ) %>%
  select(-fit_id, -index) %>%
  dependent_label(melanoma, "Survival")


haven::write_dta(melanoma,"N:/Temp/File1.dta")


################################
## Stata code
###############################


#use "N:/Temp/File1.dta", clear
#summ age thickness 
#tab1 sex ulcer

#summ time
#replace time=time/365

#tab1 status
#1=57  (died of melanoma)
#2=134 (censored)
#3=14 (other causes)

##############################################################
#overall survival: no distinction between types of death
##############################################################

  
#tab1 status_os // (0=censored; 1="died of melanoma" or "died of other causes")
#stset time,failure(status_os)
#stcox c.age i.sex c.thickness i.ulcer, nolog  efron  // default in R

##############################################################
#disease-specific survival: other deaths as censored
##############################################################

#tab1 status_dss // (0=censored or died of other cause; 1="died of melanoma")

#stset, clear
#stset time,failure(status_dss)
#stcox c.age i.sex c.thickness i.ulcer, nolog  efron  // default in R

#################
#competing-risk
#################
  
#tab1 status_crr  // (0=censored; 1 = melanoma; 2=other cause)

#stset, clear
#gen failtype=0
#replace failtype=1 if status==1    // failure event (melanoma)
#replace failtype=2 if status==3   // competing event (died of other causes)
#stset time,failure(failtype==1) // 57 died of melanoma; 14 of other causes

#model and estimate cumulative incidence in the presence of competing risks
#We modeled the cumulative incidence of melanoma (status = 1)
#in the presence of competing other deaths (status = 3) with various covariates. 
#The parameter estimates under the SHR column are subhazard ratios and measure the effects of covariates on the
#cumulative incidence of melanoma. 

#stcrreg c.age i.sex c.thickness i.ulcer, compete(failtype==2) nolog   
#stcrreg c.age i.sex, compete(failtype==2) nolog  

#The subhazard for males (sex = 1) for a given age is 1.8 times higher than the subhazard for females (sex=0).
#Men have a higher incidence of melanoma, and we can use stcurve after
#stcrreg to visualize that reduction, holding age at its mean value.

#stcurve, cif at1(sex = 1) at2(sex = 2)
