################################################################
###               Longitudinal Data Analysis                 ###
###     Mixed Models for Continuous Outcomes     ###
################################################################

.libPaths()
print(R.version.string)

Macdrive<-
  "/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/"
UCLdrive<-
  "C:/Users/rmjdshc/OneDrive - University College London/Learning/"
folder<-"Teaching/LDA/Session 1/R"  
#setwd(paste0(Macdrive, folder))
setwd(paste0(UCLdrive, folder))
#load libraries.
library(dplyr) 
library(effects) 
library(effectsize)
library(haven)
library(Hmisc)
library(latticeExtra)
library(lme4)
library(lmtest)
library(psych)
library(rlang)
library(sjstats)
library(tidyr)
library(tidyverse)
library(summarytools)

# Part 1 - Preparing the data for longitudinal analysis
# Read in ELSA data
ELSA_wave1<-haven::read_dta("data/Wave_1_elsa.dta")
ELSA_wave2<-haven::read_dta("data/Wave_2_elsa.dta")
ELSA_wave3<-haven::read_dta("data/Wave_3_elsa.dta")
ELSA_wave4<-haven::read_dta("data/Wave_4_elsa.dta")
ELSA_wave5<-haven::read_dta("data/Wave_5_elsa.dta")
ELSA_wave6<-haven::read_dta("data/Wave_6_elsa.dta")
ELSA_wave7<-haven::read_dta("data/Wave_7_elsa.dta")
head(ELSA_wave1)
## summary of all variables 
summary(ELSA_wave1)
## Check column names 
colnames(ELSA_wave1)
## Check class of variables ##
lapply(ELSA_wave1, class)

### Preparing and merging the Data
## Drop participants who did not have a valid memory score 
## (cflisen0) at baseline
ELSA_wave1_new <- ELSA_wave1 %>% filter(!is.na(cflisen0)) 
## Recode sex from "Female" and "Male" to 1 and 0 respectively
ELSA_wave1_new$dhsex <- ifelse(ELSA_wave1_new$dhsex == 1, 0, 1)
## Set 'dhsex' to integer
ELSA_wave1_new$dhsex <- as.integer(ELSA_wave1_new$dhsex)
## Make sure that the recoding has been successful
head(ELSA_wave1_new$dhsex)
## At this stage we should have a 'wide' dataset with N = 11,035 rows
## Check the length of the new dataframe
length(ELSA_wave1_new$idauniq)

## Keep only the variables that we need in new dataframes using 'select' command
ELSA_wave1_new <- ELSA_wave1_new %>% 
  dplyr::select(idauniq,cflisen0,QoLscore0,dhsex) 
ELSA_wave2_new <- ELSA_wave2 %>% 
  dplyr::select(idauniq,cflisen1,QoLscore1) 
ELSA_wave3_new <- ELSA_wave3 %>% 
  dplyr::select(idauniq,cflisen2,QoLscore2) 
ELSA_wave4_new <- ELSA_wave4 %>% 
  dplyr::select(idauniq,cflisen3,QoLscore3)
ELSA_wave5_new <- ELSA_wave5 %>% 
  dplyr::select(idauniq,cflisen4,QoLscore4)
ELSA_wave6_new <- ELSA_wave6 %>% 
  dplyr::select(idauniq,cflisen5,QoLscore5)
ELSA_wave7_new <- ELSA_wave7 %>% 
  dplyr::select(idauniq,cflisen6,QoLscore6)

## Merge the dataframes to create a wide format dataframe using 'left_join' 
ELSA_wide <- dplyr::left_join(ELSA_wave1_new, ELSA_wave2_new, by='idauniq') %>%
  dplyr::left_join(.,ELSA_wave3_new , by='idauniq') %>% 
  dplyr::left_join(.,ELSA_wave4_new , by='idauniq') %>%
  dplyr::left_join(.,ELSA_wave5_new , by='idauniq') %>%
  dplyr::left_join(.,ELSA_wave6_new , by='idauniq') %>%
  dplyr::left_join(.,ELSA_wave7_new , by='idauniq')

## Note: left_join() keeps all rows in the first dataset (x) and all the 
## matching rows 
## in the second dataset (y)
## You can use help() to check other available options to merge two dataframes 
## Look at the new dataframe
View(ELSA_wide)

# Part 2 - Descriptive Statistics and Reshaping the Data ------------------
### Summarise wave specific values of 'QoLscore*' variable
Hmisc::describe(ELSA_wide$QoLscore0)
Hmisc::describe(ELSA_wide$QoLscore1)
Hmisc::describe(ELSA_wide$QoLscore2)
Hmisc::describe(ELSA_wide$QoLscore3)
Hmisc::describe(ELSA_wide$QoLscore4)
Hmisc::describe(ELSA_wide$QoLscore5)
Hmisc::describe(ELSA_wide$QoLscore6)

### Create a correlation matrix of 'QoLscore*' variable 
cor <- ELSA_wide %>%
  dplyr::select(QoLscore0,
         QoLscore1,
         QoLscore2,
         QoLscore3,
         QoLscore4,
         QoLscore5,
         QoLscore6) %>% 
  as.matrix() %>%
  rcorr(type = "pearson")

### Use '[X]' to access objects in lists
cor[1]    # correlation matrix
cor[2]    # number of observations

### Reorder the variables before reshaping
ELSA_wide_new <- ELSA_wide %>% 
  dplyr::relocate(starts_with(c("cflisen","QoLscore")), .after = idauniq)

### View the new wide dataframe to make sure reordering was successful
View(ELSA_wide_new)

### Reshape the data from wide to long format
ELSA_long <- stats::reshape(as.data.frame(ELSA_wide_new),                                  
                     idvar = "idauniq", 
                     direction = "long",
                     varying = list(c(2:8),c(9:15)),
                     timevar = "time",
                     v.names=c("cflisen","QoLscore"))

### Check the number of records in this long data set.
ELSA_long %>% dplyr::summarise(r=n())
### View the new long data frame
View(ELSA_long)

### Drop any missing values of 'QoLscore' and 'cflisen'
ELSA_long <- ELSA_long %>% dplyr::filter(!is.na(QoLscore))
ELSA_long <- ELSA_long %>% dplyr::filter(!is.na(cflisen))

### Check the number of records
ELSA_long %>% dplyr::summarise(n=n()) 

### Sort the data by idauniq and time using arrange
ELSA_long <- ELSA_long %>% dplyr::arrange(ELSA_long, idauniq, time)
### Look at the first couple of rows
head(ELSA_long)
### Summarise memory scores
Hmisc::describe(ELSA_long$cflisen)
### Examine distribution of QoL scores
Hmisc::describe(ELSA_long$QoLscore)

### Create a new variable to summarise the number of waves 
### that participants were present
ELSA_long <- ELSA_long %>%
  dplyr::group_by(idauniq) %>%
  mutate(nwaves = n()) 

### View the dataframe to make sure that this has been successful
View(ELSA_long)

### Create an idauniq-level dataset (called IDdata) using same principle 
## as tag in Stata 
## (NB: tag picks one record per cluster (i.e. individual) and is useful 
## for when you are wanting to 
## summarise records in clustered data)
IDdata <- ELSA_long %>%
  group_by(idauniq) %>%
  arrange(time) %>%
  filter(row_number()==1)  # takes the data from the first row only

IDdata$nwaves<-as.factor(IDdata$nwaves)
freq(IDdata$nwaves)

# Part 3 - Random Effects Model - Varying Intercepts --------

### Fit null model ("randint") with random effects
randint <- lme4::lmer(QoLscore ~ 1      
                + (1|idauniq),    
                data = ELSA_long, 
                REML = FALSE)   

### Look at model table 
summary(randint)

### Now add the intercept and residual values 
## (taken from "random effects" section in randint summary table above) 
## into ICC equation 
ICC <- (55.70)/(55.70 + 23.36)
ICC

### Add fixed effects for 'cflisen' and 'sex' into the null random effects model
M1 <- lme4::lmer(QoLscore ~ 1                     
           + (1|idauniq) + cflisen + dhsex, 
           data = ELSA_long,                
           REML = FALSE)                    

### Look at model table
summary(M1)

### Plot the pop.average trajectory 
## (model predicted average for QoL based on observed predictors and estimated 
## fixed effects)
## Create new column with predicted values
ELSA_long$yhat <- predict(M1, re.form = NA)  

### Take a look at these values 
summary(ELSA_long$yhat)

### Create reduced dataframe
fitted.data <- base::subset(ELSA_long, 
                  select = c("idauniq", "dhsex", "cflisen", "yhat"))

### Take a look at the first couple of rows of this new dataframe
head(fitted.data)

### Plot the fitted data
### Make sure plot window is large enough.
with(fitted.data,
     {plot(cflisen, yhat,
  main = "Random intercept model: main effects",
  xlab="cflisen",ylab="QoL");
  lines(cflisen[dhsex==0], yhat[dhsex==0],type = "l", col="blue")
  lines(cflisen[dhsex==1], yhat[dhsex==1],type = "l", col="red")
legend("topleft", c("Male","Female"), lty = c(1,1), col = c("blue","red"), 
       cex = 0.75)})

# Part 4 - Random Effects Model - Varying Intercepts and Slopes --------

### The commands that follow show you how to: 
     # - fit the model; 
     # - store the estimates; 
     # - save the predicted values of the random intercepts and slopes; 
     # - obtain histograms; 
     # - and run a LR test comparing the models with (M2) and without 
# random slopes (M1).


### Add a random slope to the model and allow estimation 
### of the correlation between randomly varying intercepts and slopes
M2 <- lme4::lmer(QoLscore ~ 1            
           + (1 + cflisen|idauniq) 
           + cflisen + dhsex,      
           data = ELSA_long,                                   
           REML = FALSE)                                          

# NOTE: Ignore warning messages

### Look at model table
summary(M2)

### Run a Likelihood ratio test to compare the models 
lrtest(M2, M1) 

# Alternatively, you can use anova() to run a Likelihood ratio test
anova (M2, M1) 

### Create new variables for the predicted values and random effects
ELSA_long$person.specific <- fitted(M2)     
head(ELSA_long$person.specific)
ELSA_long$pop.average <- predict(M2, re.form = NA) 
head(ELSA_long$pop.average)

### Look at these new variables
head(ELSA_long)

### Create new dataframe containing random effect
random.effects <- as.data.frame(ranef(M2)$idauniq)    

### Look at the names of the variables in this new dataframe
names(random.effects)

### Add the idauniq column
random.effects <- cbind(idauniq = rownames(random.effects), random.effects)

### Check that this was successful
names(random.effects)
head(random.effects)

### Summarise the intercept ("(Intercept)") column
summary(random.effects$"(Intercept)")    

### Summarise the slope ("cflisen") column
summary(random.effects$"cflisen")  

### Plot a histogram of the random effects
## Plot density of the intercept
hist(random.effects$"(Intercept)", 
     main="Histogram for random intercepts", 
     xlab="Predicted random intercepts", 
     border="blue", 
     col="gold", 
     xlim=c(-30,30), 
     las=1, breaks=30, prob = TRUE)
lines(density(random.effects$"(Intercept)"))

### Plot density of the slope
hist(random.effects$"cflisen", 
     main="Histogram for random slope", 
     xlab="Predicted random slopes", 
     border="blue", 
     col="gold", 
     xlim=c(-1,1), 
     las=1, breaks=30, prob = TRUE)
lines(density(random.effects$"cflisen"))

# OPTIONAL - Plotting the Person-Specific Trajectories ----------------------

### To keep things manageable, we will focus on just nine participants. 

### Create a new dataframe with a subset of nine participants
ELSA_9IDs <-subset(ELSA_long,
      idauniq==104827|idauniq==104858|idauniq==107495|
    idauniq==108668|idauniq==111069|idauniq==113025|
    idauniq==118249|idauniq==118796|idauniq==119758)
head(ELSA_9IDs)

### Plot trajectories for these nine participants using the predicted values 
### obtained earlier
myplot <- ggplot(data=ELSA_9IDs,aes(cflisen)) + 
  geom_line(aes(y=pop.average,colour="pop.average")) + 
  geom_line(aes(y=person.specific,colour="person.specific trajectory")) + 
  facet_wrap(~idauniq) +
  scale_colour_manual(values=c("red","blue"))

myplot

### List the residuals for intercept and slope
mydata <- subset(random.effects,idauniq==104827|
                   idauniq==104858|idauniq==107495|idauniq==108668|
                   idauniq==111069|idauniq==113025|idauniq==118249|
                   idauniq==118796|idauniq==119758)

mydata

rm(list = ls())
print(paste("Mixed models finished at", format(Sys.time(), 
                                           "%Y-%m-%d %H:%M:%S")))

##### FINISHED #####

################################################################
###            Growth Curve Models               ###
################################################################

.libPaths()
print(R.version.string)
Macdrive <- 
  "/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/"
UCLdrive <- 
  "C:/Users/rmjdshc/OneDrive - University College London/Learning/"
folder <- "Teaching/LDA/Session 2/R"  
setwd(paste0(UCLdrive, folder))
#setwd(paste0(Macdrive, folder))

### Load relevant packages
library(haven)
library(Hmisc)
library(lme4) 
library(lmtest)  
library(psych)
library(summarytools)
library(stargazer)
library(tidyverse)
library(broom.mixed)
library(purrr)

# Part 1 - Read in the data -----------------------------------------

### Read in ELSA data
ELSA_wide <- haven::read_dta("data/GCMPractical.dta")
### Check the data 
head(ELSA_wide)
summary(ELSA_wide)

# Part 2 - Descriptive Statistics and Reshaping the Data ------------------

### Check column names
colnames(ELSA_wide)

### Check variable classes
lapply(ELSA_wide, class)
str(ELSA_wide)

### Recode sex from "Male(1)" and "Female(2)" to 1 and 0 respectively
table(ELSA_wide$indsex)
ELSA_wide$indsex <- ifelse(ELSA_wide$indsex == 1, 0, 1)

### Set 'indsex' to interger
ELSA_wide$indsex <- as.integer(ELSA_wide$indsex)

### Make sure that the recoding has been successful
head(ELSA_wide$indsex)

### Summarise wave specific values of 'CF_exec*'
describe(ELSA_wide$CF_exec1)
describe(ELSA_wide$CF_exec2)
describe(ELSA_wide$CF_exec3)
describe(ELSA_wide$CF_exec4)
describe(ELSA_wide$CF_exec5)

### create a correlation matrix of 'CF_exec*' variables 
cor <- ELSA_wide %>%
  select(CF_exec1,
         CF_exec2,
         CF_exec3,
         CF_exec4,
         CF_exec5) %>% 
  as.matrix() %>%
  rcorr(type = "pearson")

### Use '[X]' to access objects in lists
cor[1]    # correlation matrix
cor[2]    # number of observations

### Generate a missing data indicator for 'CF_exec*'
## NB: this variable allows us to understand how many 'CF_exec*' waves 
## are missing for each participant 

# Create a new dataframe including only the 'CF_exec*' variables 
miss_var<-  ELSA_wide %>%
  select(CF_exec1,
         CF_exec2,
         CF_exec3,
         CF_exec4,
         CF_exec5)

# Generate missing data indicator using 'apply'
miss_var$nmis <- apply(miss_var, MARGIN = 1, function(x) sum(is.na(x)))

### Check the new variable
head(miss_var)
miss_var$nmis<-as.factor(miss_var$nmis)
freq(miss_var$nmis)

### Check other variables by selecting only people who had valid CF data at all waves 
summary(ELSA_wide[miss_var$nmis ==0,])

### Reorder the variables before reshaping
ELSA_wide_new <- ELSA_wide %>% 
  relocate(starts_with(c("indsex", "indager", "agegroup", "educ1", 
                         "CF_exec","QoLscore", "age")), .after = idauniq)


### Check the reordering was successful
colnames(ELSA_wide_new)

### Reshape the data from wide to long format
ELSA_long <- reshape(as.data.frame(ELSA_wide_new),                                  
                     idvar = "idauniq", 
                     direction = "long",
                     varying = list(c(6:10),c(11:15), c(16:20), c(21:25)),
                     timevar = "time",
                     sep = "" )
head(ELSA_long)

### Rename the new variables in the long dataset (new var=old var)
ELSA_long <- rename (ELSA_long, CF_exec = CF_exec1)
ELSA_long <- rename (ELSA_long, QoLscore = QoLscore1)
ELSA_long <- rename (ELSA_long, wave = wave1)
ELSA_long <- rename (ELSA_long, age = age1)
head(ELSA_long)

### Sort the data by idauniq and time using arrange
ELSA_long <- ELSA_long %>%
  arrange(ELSA_long, idauniq, time)
head(ELSA_long)

### Keep only the variables that we need in new dataframe 
ELSA_long <- ELSA_long %>% 
  select(idauniq, indsex, indager, agegroup, educ1, 
         CF_exec, QoLscore, age, time) 

### Look at the first couple of rows, we have 56955 records
head(ELSA_long)

### Drop any missing values of 'CF_exec'
ELSA_long <- ELSA_long %>% filter(!is.na(CF_exec))

### Check the filtering was successful 
table(is.na(ELSA_long$CF_exec))
table(!is.na(ELSA_long$CF_exec))

### Examine distribution of CF_exec scores
### Summarise data, we now have 37173 records
describe(ELSA_long$CF_exec)
hist(ELSA_long$CF_exec)

### Create a new variable to summarise the number of waves 
### that participants have CF scores
ELSA_long <- ELSA_long %>%
  group_by(idauniq) %>%
  mutate(nwaves = n()) 

### Check the new variable
describe(ELSA_long$nwaves)
table(ELSA_long$nwaves)

### Create an idauniq-level dataset (called IDdata) 
IDdata <- ELSA_long %>%
  group_by(idauniq) %>%
  arrange(time) %>%
  filter(row_number()==1)  # takes the data from the first row only

### Check how many people had at least one observed value of 
### CF_exe over the follow-up period
IDdata$nwaves<-as.factor(IDdata$nwaves)
freq(IDdata$nwaves)

# Part 3 - Growth Curve Model ---------------------------------------------

### Set waves to run from 0 to 4 (instead of from 1 to 5)
ELSA_long$time = ELSA_long$time-1

### Check that this recoding has been successful
summary(ELSA_long$time)

### Fit a linear growth curve model ("linuncond") with time
linuncond <- lmer(CF_exec ~ 1                  
                  + time + (1 + time|idauniq), 
                  data = ELSA_long,            
                  REML = FALSE)                

CI_linuncond<-confint(linuncond) 
CI_linuncond

# Note: the random parts of the model are: '.sig.*' and 'sigma'; 
# and the fixed parts of the model are: '(Intercept)' and 'time'
# sig01: sd(intercept)
# sig02: corr(intercept;slope)
# sig03: sd(slope)
# sigma: sd(level-1 residual)

### Look at model table
summary(linuncond)
class(linuncond)
class(linuncond) <- "lmerMod"
### We can display this in a neater way using stargazer
stargazer(linuncond, ci = TRUE, type="text", 
          title="Results", align=TRUE) 
# we include ci = TRUE to get the confidence intervals 
# for the fixed part of the model

### You can also use 'tidy' to save the model results into a dataframe
table_linuncond<- tidy(linuncond)
table_linuncond

#add column for the model name
table_linuncond$model<-"linuncond"

### Create new dataframe with individuals that have data over all 
### 5 waves (i.e. complete data; n = 4144)
ELSA_long_complete <- ELSA_long[which(ELSA_long$nwaves=='5'),]
length(unique(ELSA_long_complete$idauniq))

## Run the model again with individuals that have complete data
linuncond_complete <- lmer(CF_exec ~ 1                
                           + time + (1 + time|idauniq),  
                           data = ELSA_long_complete,  
                           REML = FALSE)              

### Look at model table
summary(linuncond_complete) 
class(linuncond_complete) <- "lmerMod"

### Display results using stargazer
stargazer(linuncond_complete, type="text", title="Results", align=TRUE)

### Display results from both models using stargazer
stargazer(linuncond, linuncond_complete, type="text", title="Results", 
          align=TRUE)

### Create and export dataframe with output from multiple models 

# create dataframe for output of linuncond_complete
table_linuncond_complete<- tidy(linuncond_complete)

# add column with model name
table_linuncond_complete$model<-"linuncond_complete"

# combine output of linuncond and linuncond_complete into a single dataframe 
table_allMod<- rbind(table_linuncond, table_linuncond_complete)
table_allMod

#export to excel 
write.csv(table_allMod, file="outputs/table_allMod.csv")

# Part 4 - Quadratic Growth Curve Model -----------------------------------

### Add a wave^2 column ###
ELSA_long$time_2 <- ELSA_long$time^2

### Run quadratic model (with full (i.e. not complete cases) dataset) 
quaduncond <- lmer(CF_exec ~ 1                            
                   + time + time_2 + (1 + time |idauniq), 
                   data = ELSA_long,                      
                   REML = FALSE)                          
## Ignore warning message

### Look at model table
summary(quaduncond)
class(quaduncond) <- "lmerMod"

### Display results using stargazer
stargazer(quaduncond, type="text", title="Results", align=TRUE)

stargazer(linuncond, quaduncond, type="text", title="Results", align=TRUE)

### Run a Likelihood Ratio (LR) test to compare the quadratic model 
### ("quaduncond") with the linear model ("linuncond")
lrtest(quaduncond, linuncond) ## Larger model goes first

# Part 5 - Conditional Growth Curve Model ---------------------------------

### Look at mean age at the start of the study ("indager")
mean(ELSA_long$indager) 

### Look at the distribution of age at the start of the study
hist(ELSA_long$indager)

### Centre age at the start of the study around mean(indager) (i.e. 63.76)
ELSA_long$indager_centre <- (ELSA_long$indager - 63.76)

### Create new variable with interaction between time and age at the 
### start of the study 
ELSA_long$timeage <- (ELSA_long$time*ELSA_long$indager_centre)

### Fit conditional model ("cond_timeage") with time*age interaction
cond_timeage <- lmer(CF_exec ~ time 
                     + indager_centre + timeage + (1 + time|idauniq), 
                     data = ELSA_long,
                     REML = FALSE)

### Look at model table
summary(cond_timeage)
class(cond_timeage) <- "lmerMod"

### NB: You can also add an interaction term directly to the model 
### (without creating a new variable before)
summary(lmer(CF_exec ~ time*indager_centre + (1 + time|idauniq), 
             data = ELSA_long,
             REML = FALSE))

### Display results using stargazer
stargazer(cond_timeage, type="text", title="Results", align=TRUE)

### Create new variable with interaction between time and sex 
ELSA_long$timesex <- (ELSA_long$time*ELSA_long$indsex)

### Fit conditional model ("cond_timeage_timesex") with 
### time*age and time*sex interactions
cond_timeage_timesex <- lmer(CF_exec ~ 1 + time 
                             + indager_centre + timeage + factor(indsex) 
                             + timesex + (1 + time|idauniq), 
                             data = ELSA_long, 
                             REML = FALSE)

### Look at model table
summary(cond_timeage_timesex)
class(cond_timeage_timesex) <- "lmerMod"

M1 <- lmer(CF_exec ~ 1 + time 
           + indager_centre + timeage + factor(indsex) 
           + (1 + time|idauniq), 
           data = ELSA_long, 
           REML = FALSE)
summary(M1)

### Display results using stargazer
stargazer(cond_timeage_timesex, type="text", title="Results", align=TRUE)

### Run a Likelihood Ratio (LR) test to compare the time*age ("cond_timeage") 
### with the time*age + time*sex ("cond_timeage_timesex") models 
lrtest(cond_timeage_timesex, cond_timeage) 

### You can also use the `anova` function to compare the BIC and AIC values 
anova(cond_timeage_timesex, cond_timeage) 

### Create new variable with interaction between time and education 
ELSA_long$timeedu <- (ELSA_long$time*ELSA_long$educ1)

### Fit conditional model ("cond_timeage_timesex_timeedu") 
### with time*age, time*sex and time*education interactions
cond_timeage_timesex_timeedu <- lmer(CF_exec ~ time 
                                     + indager_centre + timeage + indsex 
                                     + timesex + timeedu + educ1 + 
                                       (1 + time|idauniq), 
                                     data = ELSA_long, 
                                     REML = FALSE)

### Look at model table
summary(cond_timeage_timesex_timeedu)
class(cond_timeage_timesex_timeedu) <- "lmerMod"

### Display results using stargazer
stargazer(cond_timeage_timesex_timeedu, type="text", title="Results", align=TRUE)

### Run LR test and compare AIC and BIC of the three models 
anova(cond_timeage_timesex_timeedu, cond_timeage_timesex, cond_timeage) 


# Part 6 - Age as metric of time ------------------------------------------

### mean age 
summary(ELSA_long$age)

### Centre age throughout the study around mean (i.e. time-varying age 
### centered to mean)
ELSA_long$age_centre <- (ELSA_long$age - 67.07)

### Fit conditional model ("cond_age") with centred age as time
cond_age <- lmer(CF_exec ~ age_centre + (age_centre|idauniq),  
                 data = ELSA_long)

## Ignore warning messages

### Look at the model table
summary(cond_age)
class(cond_age) <- "lmerMod"

### Display results using stargazer
stargazer(cond_age, type="text", title="Results", align=TRUE)

# Part 7 - Making Graphs --------------------------------------------------

### Fit simple growth model with time only ("time")
time <- lmer(CF_exec ~ time + (time|idauniq), 
             data = ELSA_long, 
             REML = FALSE)


### Look at the model table ###
summary(time)
class(time) <- "lmerMod"

### Display results using stargazer
stargazer(time, type="text", title="Results", align=TRUE)

### Create a function ('get_pred') which selects the variables from the 
### 'time' model that you want to plot
get_pred <- function(string, t, mod){                   
  multcomp::glht(mod, string) %>%                      
    confint() %>%                                     
    pluck("confint") %>%                              
    as_tibble() %>%                                     
    mutate(time = !!t) %>%                             
    select(time, est = Estimate, lci = lwr, uci = upr)  
}

### Set the time sequence from 0 - 4 years 
time_seq <- seq(from = 0.001, to = 4, length = 40) 

### Apply the function to each element of the 'time_seq' values
map_dfr(time_seq,  
        ~ paste0("(Intercept) + ", .x, "*time = 0") %>%  
          get_pred(.x, time)) %>%                        
  ggplot() +                                             
  aes(x = time, y = est, ymin = lci, ymax = uci) +       
  geom_ribbon(color = NA, alpha = 0.2) +               
  geom_line() +                                     
  labs(x = "Time", y = "Predicted")                 

### Run the model with education and education*time
time_educ <- lmer(CF_exec ~ time + educ1 + educ1*time + (1 + time|idauniq),
                  data = ELSA_long,
                  REML = FALSE)
summary(time_educ)

### Run the same command as above but separately for low and 
### high education ('1' and '0' respectively)
## Low education ('1')
educ_low <- map_dfr(time_seq,
                    ~ paste0("(Intercept) + educ1 + ", 
                             .x, "*time + ", 
                             .x, "*`time:educ1` = 0") %>%   
                      get_pred(.x, time_educ))                         


## High education ('0')
educ_high <- map_dfr(time_seq,
                     ~ paste0("(Intercept) + ", .x, "*time = 0") %>%   
                       get_pred(.x, time_educ))                        

### Create a matrix with these values and plot them in ggplot
bind_rows(Low = educ_low, High = educ_high, .id = "educ") %>% 
  ggplot() +
  aes(x = time, y = est, ymin = lci, ymax = uci,              
      color = educ, fill = educ) +                            
  geom_ribbon(color = NA, alpha = 0.2) +                     
  geom_line() +                                           
  labs(x = "Time", y = "Predicted")                          

rm(list = ls())

print(paste("Growth curve models finished at", 
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

##### FINISHED #####



