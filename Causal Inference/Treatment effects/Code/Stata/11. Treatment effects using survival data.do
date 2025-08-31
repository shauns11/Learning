/*
Treatment effects using survival data.
Note: this does not accommodate time-varying confounders.

We have some fictional data on the time to a second heart attack among women aged 45–55 years.
The treatment, smoking, is stored in the 0/1 indicator smoke. 
These data also contain each woman's age at the time of her first heart attack (age), and indices of her exercise level (exercise), diet
quality (diet), and education attainment (education) prior to her first heart attack.
stteffects uses the outcome variable and the failure indicator computed by stset. 
In this dataset, atime is the observed time in years to the second heart attack, and fail is the 0/1 indicator that a second heart attack was observed and recorded in atime. 
When fail=1, atime records the time to the second attack; when fail=0, atime records a censored observation of the time to the second attack
*/


*clear
*use https://www.stata-press.com/data/r18/sheart
*save "C:\Users\rmjdshc\OneDrive - University College London\Learning\Causal Inference\Treatment Effects\data\sheart.dta"
*global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Causal Inference/Treatment effects/"
*global data "${projdir}/data"
*global outputs "${projdir}/outputs/Temp"
*use "${data}/sheart.dta", clear

use https://www.stata-press.com/data/r18/sheart, clear

stset atime, failure(fail)

****************************
*1: Regression adjustment 
****************************
stteffects ra (age exercise diet education) (smoke)
stteffects ra (age exercise diet education) (smoke), atet

/*
When all women in the population smoke, the average time to a second heart attack is estimated
to be 1.96 years less than when no women smoke. The estimated average time to a second heart attack when no women smoke is 4.24 years.
A Weibull model was used for the outcome. The ratio of the ATE to control-level POM measures the importance of the effect. 
In this example, when all women smoke, the time to a second heart attack falls by an estimated 46% relative to the
case in which none of them smoke
*/ 

di (2.28-4.24)/(4.24)

*Treatment assignment is handled by fitting separate models for each treatment level and averaging the predicted outcomes. 
*As is standard in the survival-time literature, the censoring term in the log-likelihood function accounts for censoring.

****************************
*2: IPW 
****************************
stteffects ipw (smoke age exercise diet education) (age exercise diet education)
stteffects ipw (smoke age exercise diet education) (age exercise diet education), atet

/*
When all women in the population smoke, the average time to a second heart attack is estimated to be 2.19 years less than when no women smoke. 
The estimated average time to a second heart attack when no women smoke is 4.23 years. 
When all women smoke, the average time to a second heart attack falls by an estimated 52% relative to the case when no women smoke.
*/

di (2.19-4.23)/(4.23)

/*
The estimates have changed; however, the interpretation is the same as for the RA estimator
because the IPW and RA estimators are estimating the same population effects. Under correct model
specification, the estimates will differ in finite samples, but the size of these differences will decrease as the sample size gets larger. 
Recall that IPW estimators are weighted averages of observed outcomes and that the weights control
for the missing outcomes. Weights in survival-time data have two components: (i) one for the missing
potential outcome and (ii) one for data lost to censoring. 
We used a logit model for treatment assignment, so the component of the weights that controls for the missing potential outcome comes from the estimated logit parameters. 
We used a Weibull model for the time to censoring, so the component of the weights that controls for data lost to censoring comes from the estimated Weibull parameters.
*/


**********************
*3 IPWRA
**********************

/*
More efficient estimators are obtained by combining IPW and RA. Both the treatment and the outcome models must be correctly specified to estimate the ATE.
The IPWRA estimator uses estimated weights that control for missing data to obtain missingness adjusted
regression coefficients that are used to compute averages of predicted outcomes to estimate the POMs. 
The estimated ATE is a contrast of the estimated POMs. These weights always involve a model for treatment assignment. 
You choose whether to account for censoring by including a term in the log-likelihood function or whether to use weights that also account for the data lost to censoring.
*/

***********************************************
*3a LAC-IPWRA (likelihood adjusted censoring)
**********************************************

stteffects ipwra (age exercise diet education) (smoke age exercise diet education)

*We did not specify a model for the time to censoring, so censoring is handled by including a term in the log-likelihood function in the Weibull outcome model. 

***********************************************
*3b WAC-IPWRA (weighted-adjusted censoring)
**********************************************

*(i) We model the time to a second heart attack as a function of age, exercise, diet, and education; 
*(ii) we model assignment to the treatment smoke as a function of the same covariates; 
*(iii) we model the time to censoring as a function of age, exercise, and diet.

stteffects ipwra (age exercise diet education) (smoke age exercise diet education) (age exercise diet)

/*
The weights for censoring are constructed from the estimated parameters because we specified a
time-to-censoring model. Under correct specification, both versions of the IPWRA estimator estimate the same ATE and
control-level POM as estimated by RA and IPW. The addition of the time-to-censoring model makes the WAC-IPWRA somewhat less robust than
the LAC-IPWRA estimator. Weighting methods to control for censoring also place more restrictive
assumptions on the censoring process. For example, the censoring time must be random, otherwise
it would be impossible to construct the weights. In Assumptions and tradeoffs below, we discuss the
tradeoffs among the estimators and the assumptions that each requires. For the moment, we note that
we believe the LAC-IPWRA estimator is more robust than the WAC-IPWRA estimator.
*/

**********************************
*4 Weighted regression adjustment
**********************************

/*
When estimating the parameters of an outcome model, the weighted regression-adjustment (WRA) estimator uses weights instead of a term in the log-likelihood function to adjust for censoring.
These weights are constructed from a model for the censoring process. The estimated parameters are
subsequently used to compute averages of predicted outcomes that estimate the POMs. A contrast of the estimated POMs estimates the ATE.
*/

stteffects wra (age exercise diet education) (smoke) (age exercise diet)

/*
The WRA estimators estimate the same effect parameters as the RA estimator, so the interpretation is the same.
In many survival-time applications, using weights to adjust for censoring is probably less robust than just including a term in the log-likelihood function for the outcome model.
*/

di "Finished"







