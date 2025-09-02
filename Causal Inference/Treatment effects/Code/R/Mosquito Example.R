#################################################################
############## Mosquito example #################################
#Matching and IPW.
#https://evalf20.classes.andrewheiss.com/example/matching-ipw/
#https://evalf20.classes.andrewheiss.com/
################################################################

.libPaths()
print(R.version.string)
Macdrive <- "/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/"
UCLdrive <-"C:/Users/rmjdshc/OneDrive - University College London/Learning/"
folder <- "Causal Inference/Treatment Effects"  
setwd(paste0(UCLdrive, folder))
#setwd(paste0(Macdrive, folder))

library(dagitty)
library(ggdag)
library(modelsummary)
library(tidyverse)
library(glue)
library(broom)
library(MatchIt)


#nets <- read_csv("data/mosquito_nets.csv")

url_stem <- "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
nets <-read_csv(glue(url_stem,'Data/mosquito_nets.csv'))
head(nets)

set.seed(1234)   

#Researchers are interested in whether using mosquito nets 
#decreases an individual's risk of contracting malaria. 
#They have collected data from 1,752 households 
#and have variables related to environmental factors, individual health, 
#and household characteristics. 

#The data is not experimental-researchers have no control over who uses 
#mosquito nets, and individual households make their own choices over whether 
#to apply for free nets or buy their own nets, 
#as well as whether they use the nets if they have them.

#Our goal in this example is to estimate the causal effect of bed net usage on 
#malaria risk using only observational data. 
#This was not an RCT, so it might seem a little sketchy 
#to make claims of causality. 
#But if we can draw a correct DAG and adjust for the correct nodes, 
#we can isolate the net ??? malaria relationship and talk about causality.
#Because this is simulated data, we know the true causal effect of the net 
#program because I built it into the data. 
#The true average treatment effect (ATE) is -10. 
#Using a mosquito net causes the risk of malaria to decrease by 10 points, 
#on average. Which estimators can achieve ATE=-10?

#1. DAG and adjustment sets

#Before running any models, we need to find what we need to adjust for.

mosquito_dag <- ggdag::dagify(
  malaria_risk ~ net + income + health + temperature + resistance,
  net ~ income + health + temperature + eligible + household,
  eligible ~ income + household,
  health ~ income,
  exposure = "net",
  outcome = "malaria_risk",
  coords = list(
    x = c(malaria_risk = 7, net = 3, income = 4, health = 5,
          temperature = 6, resistance = 8.5, eligible = 2, household = 1),
    y = c(malaria_risk = 2, net = 2, income = 3, health = 1,
          temperature = 3, resistance = 2, eligible = 3, household = 2)),
  labels = c(
    malaria_risk = "Risk of malaria", 
    net = "net", 
    income = "Income",
    health = "Health", 
    temperature = "temp", 
    resistance = "resist",
    eligible = "program", 
    household = "N in household")
)
adjustmentSets(mosquito_dag)

ggdag_status(mosquito_dag, use_labels = "label", text = FALSE) +
  theme_dag()

#Following the logic of do-calculus, we can find all the nodes that 
#confound the relationship between net usage and malaria risk, 
#since those nodes open up backdoor paths and distort the causal effect 
#we care about. 

adjustmentSets(mosquito_dag)

#adjust for health, income and temperature.

#Based on the relationships between all the nodes in the DAG, 
#adjusting for health, income, and temperature is enough to close 
#all backdoors and identify the relationship between net use and malaria risk.

#Naive estimator.
nets %>% 
  dplyr::group_by(net) %>% 
  dplyr::summarize(number = n(),
            avg = mean(malaria_risk))

naive<-(25.6 - 41.9)
naive
M1 <- lm(malaria_risk ~ net, data = nets)
broom::tidy(M1)


#2. Matching

#We can use matching techniques to pair up similar observations 
#and make the unconfoundedness assumption-that if we see two observations 
#that are pretty much identical, and one used a net and one didn't, 
#the choice to use a net was random.

#Because we know from the DAG that income, temperatures, and health 
#help cause both net use and malaria risk (and confound that relationship!), 
#we'll try to find observations with similar values of income, temperatures, 
#and health that both used and didn't use nets.

#We can use the matchit() function to match points based on Mahalanobis 
#distance. 
#We can include the replace = TRUE option to make it so that 
#points that have been matched already can be matched again 
#(that is, we're not forcing a one-to-one matching; 
#we have one-to-many matching instead).

matched_data <- matchit(net ~ income + temperature + health,
                        data = nets,
                        method = "nearest",
                        distance = "mahalanobis",
                        replace = TRUE)
summary(matched_data)

#681 treated matched with 439 control.

#We can create a new data frame of those matches with match.data():

md <- match.data(matched_data)
View(md)
#1120 rows (681 + 439).

#Estimation using matched data.

#Now that the data has been matched, it should work better for modeling. 
#Also, because we used income, temperatures, and health in the matching 
#process, 
#we've adjusted for those DAG nodes and have closed those backdoors, 
#so our model can be pretty simple here:

#estimation (Y ~ Tx [no wt])
M2 <- lm(malaria_risk ~ net, 
         data = md)
broom::tidy(M2)

#The 12.9 point decrease here is better than the naive estimate, 
#but it's not the true 10 point ate. 
#Perhaps that's because the matches aren't great, 
#or maybe we threw away too much data. 
#There are a host of diagnostics you can look at to see how well things 
#are matched.
#The most likely culprit for the incorrect estimate is that 
#there's some imbalance in the data. 
#Because we set replace = TRUE, we did not do 1:1 matching-
#untreated observations were paired with more than one treated observation. 
#As a result, the multiply-matched observations are getting overcounted 
#and have too much importance in the model. 
#Fortunately, matchit() provides us with a column called weights 
#that allows us to scale down the overmatched observations when running 
#the model. 

#Importantly, these weights have nothing to do with causal inference 
#or backdoors or inverse probability weighting-
#their only purpose is to help scale down the imbalance arising from 
#overmatching. 

#If you use replace = FALSE and enforce 1:1 matching, 
#the whole weights column will just be 1.

#We can incorporate those weights into the model and 
#get a more accurate estimate:

#estimation (Y ~ Tx [wtd])

M2a <- lm(malaria_risk ~ net, data = md,weights = weights)
broom::tidy(M2a)

#After weighting to account for under- and over-matching, 
#we find a -10.5 point causal effect. 
#We've closed the confounding backdoors and isolated the arrow 
#between net use and malaria risk.

#3. Inverse probability weighting

#One potential downside to matching is that you generally have to 
#throw away a sizable chunk of your data-
#anything that's unmatched doesn't get included in the final matched data.

#An alternative approach to matching is to assign every observation 
#some probability of receiving treatment, and 
#then weight each observation by its inverse probability-
#observations that are predicted to get treatment and then don't, 
#or observations that are predicted to not get treatment and then do 
#will receive more weight than the observations that get/don't get treatment 
#as predicted.

#Generating these inverse probability weights requires a two step process: 
#(1) we first generate propensity scores, 
#or the probability of receiving treatment, and then 
#(2) we use a special formula to convert those propensity scores 
#into weights. 
#Once we have inverse probability weights weights, 
#we can incorporate them into our regression model.

#When we include variables in the model that generates propensity scores, 
#we're making adjustments and closing backdoors in the DAG, 
#just like we did with matching. 
#But unlike matching, we're not throwing any data away! 
#We're just making some observations more important and others less important.

#First we build a model that predicts net usage based on income, temperatures, 
#and health 
#(since those nodes are our confounders from the DAG):

pmodel <- glm(net ~ income + temperature + health,
              data = nets,
              family = binomial(link = "logit"))  

#.fitted is the R name.
PrTx <- augment_columns(pmodel,
                        nets,type.predict = "response") %>% 
  rename(pscore = .fitted)

# Look at the first few rows of a few columns
PrTx %>% 
  dplyr::select(id, net, income, temperature, health, pscore) %>% 
  head()

ipw <- PrTx %>%
  mutate(ipw = (net_num / pscore) + ((1 - net_num) / (1 - pscore)))

# Look at the first few rows of a few columns
ipw %>% 
  dplyr::select(id, net, income, temperature, health, pscore, ipw) %>% 
  head()

#(Y ~ Tx [wt=ipw])
M3 <- lm(malaria_risk ~ net, 
         data = ipw,
         weights = ipw)
broom::tidy(M3)

#truncate weight
# If the IPW is larger than 8, make it 8, otherwise use the current IPW

ipw <- ipw %>% 
  mutate(ipw_trunc = ifelse(ipw > 8, 8, ipw))

M3a <- lm(malaria_risk ~ net, 
          data = ipw,
          weights = ipw_trunc)
broom::tidy(M3a)

modelsummary(list("Naive" = M1, 
                  "Matched" = M2, 
                  "Matched + weights" = M2a, 
                  "IPW" = M3, 
                  "IPW truncated at 8" = M3a))

#Both matching and IPW work well for closing backdoors and adjusting 
#for confounders. Try multiple ways.

print(paste("Mosquito Example finished at", format(Sys.time(), 
                                              "%Y-%m-%d %H:%M:%S")))
########### END

