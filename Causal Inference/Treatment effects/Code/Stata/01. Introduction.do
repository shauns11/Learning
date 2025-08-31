********************************************
*** Treatment effects: Introduction.
********************************************

clear all
global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Causal Inference/Treatment effects/"
global data "${projdir}/data"
global outputs "${projdir}/outputs"

capture ssc install covbal psmatch2
adopath + "C:/Users/rmjdshc/OneDrive - University College London/Learning/Software/Stata/ado files"
adopath + "C:/Users/rmjdshc/OneDrive - University College London/StataMP17/"
adopath + "C:/Users/rmjdsjmhc/OneDrive - University College London/StataMP18/"

*Github location.
global Git_projdir "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
global Git_data "${Git_projdir}/Data/"

/* Introduction

Causal inference:

Formally define causal effects:
(i) assumptions necessary to identify causal effects from data;
(ii) rules about which variables need to be controlled for;
(iii) sensitivity analyses to determine the impact of violations of assumptions on conclusions.

Potential outcomes:

The counterfactual outcomes are called potential outcomes in the treatment-effects literature. 
Potential outcomes to ascertain causal effects at average (population) level.
These are the outcomes we would see under each treatment: 
(i) IF everyone in the pop. was treated with the Tx; 
(ii) IF everyone in the pop. was not treated with the Tx.

Counterfactual outcomes: the outcomes that would have been observed had the Tx been different. A matched pair can provide the counterfactual. 
We can estimate **population-level (average)** causal effects: but not causal effects at the individual level. 

A population-level causal effect: 
Compare two scenarios: everyone in the pop. on Tx versus no-one in the pop. on Tx. 
Focus on the causal effects of an intervention: what would the outcome be if everyone took treatment versus if no-one took the treatment 
(this is the same setting: the same population, not a comparison of different groups). 

We do NOT therefore restrict to the subpopulation of those who received the Tx.
A causal effect: the outcome being different if everyone took Tx vs no-one taking Tx. 
The fundamental problem of causal inference: we can only observe one potential outcome for each person. 
However, with certain assumptions, we can estimate pop-level (average) causal effects.

*Conditioning*: restricting to a subpopulation (not a causal effect). 
In our data, we observe who gets the Tx and who does not. 
Comparing these **different** groups contains potential selection bias: (those on Tx may be systematically different from those not on Tx).

*Setting*: reference to everyone (pop. on Tx versus pop. not on Tx). 
Manipulating Tx on the same people (establish causal effect): isolate the causal effect: compare the mean of potential outcomes. 
Average causal effect: compare two hypothetical populations: (everyone treated vs no-one treated): 
this is not equivalent to comparing the two sub-populations (those on Tx versus those not on Tx). 
We hypothetically manipulate Tx on the same people: isolate causal effect of Tx via difference in mean of potential outcomes. 
There is no restriction to a subpopulation. 
A marginal causal effect of Tx (e.g. a mean difference in potential outcomes). 
We do **not** condition on pre-treatment covariates X: we average over the distribution of X.

Causal effect of Tx on the treated: how well treatment works among treated people. 
For the treated population: compare potential outcomes (everyone treated vs no-one treated). 
This is a comparison of potential outcomes among treated people (not a comparison of different subpopulations).

We can use observational data to estimate the effect caused by receiving one treatment
instead of another. In observational data, treatment assignment is not controlled
by those who collect the data: thus some common variables affect treatment assignment 
(A) and treatment specific outcomes (Y). When **all** the variables that affect 
both treatment assignment and outcomes are observable, the outcomes are said to be 
conditionally independent of the treatment. That is, we assume no endogeneity 
of the Tx variable. Disregarding endogeneity may 
underestimate the effect of Tx on outcome. We want the treatment to be exogenous — to be not influenced by unobserved 
factors that also affect the outcome. The Tx may be endogenous if those choosing the treatment are systematically different; 
or when there are unobserved confounders that effect both the treatment and the outcome.
Make the potential outcomes conditionally independent of the treatment (ignorability). 

The missing-data problem arises because we observe only the potential outcome for the 
treatment level received: participants either did or did not receive the treatment 
(we do not observe both potential outcomes for the same individual). 
The fundamental problem of causal inference is that we can only observe 
one potential outcome for each person. However, with certain assumptions, 
we can estimate population-level (average) causal effects (ATE). 
We can estimate population-level (average) causal effects: but not 
causal effects at the individual-level. We estimate population-level 
effects by SETTING (if everyone in the population received the Tx 
versus if no-one in the population received the Tx).

**The Average Causal Effect **

Comparing two hypothetical populations: if *everyone* in the population 
was treated versus if *everyone* in the population was not treated. 
A causal effect compares potential outcomes on the same population. 
This is not equivalent to comparing the two sub-populations we see in the data: 
those on Tx versus those not on Tx (these are different subpopulations).
													
Causal inference requires the estimation of the unconditional means of the outcomes for 
each treatment level. In our data, we only observe the outcome of each subject 
conditional on the received treatment (0 or 1) regardless of whether the data are observational 
or experimental. 

(i) For experimental data, random assignment of the treatment guarantees 
that the treatment is independent of the outcome; so averages of the outcomes 
conditional on observed treatment estimate the unconditional means of interest. 
Because of randomisation, the treatment groups are free of selection bias. 

(ii) For observational data, we can model the treatment assignment process 
(via a propensity score model). If our model is correct, the treatment 
assignment process is considered as good as random *conditional* on the covariates (X) in the model.

*ATE*: expected gain from Tx for a randomly selected unit from the population.
*ATET*: average gain from Tx for those who were *actually* treated.

ATET may be a lot lower (e.g. the treated may be more healthier).

*Stata's teffects

In Stata, teffects commands estimate treatment-specific predicted outcomes (POs) *for each subject*
then computes the means of these POs. These are contrasted to estimate the ATE and ATT. 
ATE = (pom_t) - (pom_c).

****************************************************
*** Different estimation methods via Stata's teffects
****************************************************

A covariate is said to be balanced when its distribution does not vary over treatment levels. 

(i) Covariates are balanced in experimental data because treatment assignment is independent of the covariates 
because of the study design. 

(ii) In contrast, covariates must be balanced by weighting or matching in observational data because 
treatment assignment is related to the covariates that also affect the outcome.

Once we know WHICH variables to control for in estimating the causal effect of Tx (i.e. avoid bias due to confounding), 
the question then becomes HOW to control for those variables (X's) in estimating the effect of Tx 
using observed data. General approaches include IPTW and matching.

Six treatment-effects estimators in Stata:

(1) RA: Regression adjustment
(2) IPW: Inverse probability weighting
(3) IPWRA: Inverse probability weighting with regression adjustment
(4) AIPW: Augmented inverse probability weighting
(5) NNM: Nearest-neighbor matching
(6) PSM: Propensity-score matching

Here are some rules of thumb:

(1) Under correct specification, all the estimators should produce similar results. 
Similar estimates however do not guarantee correct specification because all the specifications could be wrong.

(2) When you know the determinants of treatment status, IPW is a natural base-case estimator.
When you know the determinants of treatment status, PSM is another base-case estimator.

(3) When you instead know the determinants of the outcome, RA is a natural base-case estimator.

(4) The doubly robust estimators, AIPW and IPWRA, give us an extra shot at correct specification.

(5) When you have lots of continuous covariates (Xs), NNM will crucially hinge on the bias adjustment, 
and the computation gets to be extremely difficult.

(6) The IPW estimators are not reliable when the estimated treatment probabilities get too close to 0 or 1 (No overlap).
Overlap means that subjects have a positive probability of receiving either treatment or control: 
that is, there is no group that is completely excluded from one of the treatment options.

***************************
*Checking covariate balance
***************************

The estimators implemented in teffects use a model or matching method to make the outcome conditionally independent of Tx by 
conditioning on covariates (Xs). If this model or matching method is well specified, it should balance the covariates. 

(i) Matching estimators define a matched sample, and IPW estimators define a weighted sample, 
each of which can be used to compute covariate balance statistics.

(ii) RA estimators do not define an adjusted sample that can be used to compute covariate balance statistics, 
and tebalance does not work after teffects ra. 

(iii) Only the IPW component of the estimators that combine RA and IPW defines a weighted sample that can be used to compute 
balance statistics. So, tebalance produces the same results after teffects aipw or teffects ipwra as it does after teffects ipw.


***************************************
*Estimator 1: Regression adjustment 
***************************************

RA estimators model the outcome (Y) to account for the nonrandom treatment assignment.
RA estimators use the contrasts of the averages of treatment-specific predicted outcomes to estimate 
treatment effects. RA estimators use a two-step approach to estimating treatment effects:

(1) They fit separate regression models of the outcome on a set of covariates for each treatment level.

(2) They compute the averages of the predicted outcomes for each subject and treatment level. 
These averages reflect the POMs. The contrasts of these averages provide estimates of the ATEs. 

By restricting the computations of the means to the subset of treated subjects, we obtain the ATETs. 

RA estimators are consistent as long as the treatment is independent of the potential outcomes after conditioning on the covariates.

*/

***************
*RA: Example 1
***************

/* Predicted scores from regressing Y ~ X separately by Tx status. 
RA estimators model the outcome to account for nonrandom treatment assignment. 
Some researchers prefer to model the treatment assignment process (Tx ~ X), rather than and not 
specify a model for the outcome.

Note that we estimate an average treatment effect (ATE), conditional on covariate values, for each subject. 
Furthermore, we estimate this effect for each subject, regardless of which treatment was actually received. 
Averages of these effects **over all the subjects** in the data, estimate the ATE. ATE is the difference in the POM. 
The ATE on the treated (ATET) is like the ATE, but it uses only the subjects who were observed to be Tx=1.

*/

clear
input id	treat	score	income
1	0	73	2761
2	0	74	1254
3	0	70	2607
4	0	69	1966
5	0	73	2760
6	1	68	3385
7	1	68	3472
8	1	71	3914
9	1	70	2438
10	1	72	3020
end
teffects ra (score income) (treat), pomeans nolog
teffects ra (score income) (treat), ate nolog
teffects ra (score income) (treat), atet nolog

/*
Step 1. Fit two separate regressions (Y ~ X) on whole sample and generate yhat when Tx=0 and when Tx=1. 
Step 2. Calculate mean predicted value (whole sample).
(1) The ATE: difference between the two means. 
(2) The ATET: difference between the two means (for observed Tx=1). 
*/

qui: regress score income if treat==0
predict pom_c
qui: regress score income if treat==1
predict pom_t
qui:sum pom_t
local pom_t = r(mean)    
qui:sum pom_c
local pom_c = r(mean)
di "ATE="`pom_t' - `pom_c' 

*ATET.
*For those on Tx: compare observed vs counterfactual. 
regress score income if treat==0

*(i) obtain coefficients from model on the control.
*(ii) use those coefficients with observed X for the treated.

predict yhat if treat == 1  

* compare POMs
qui:sum score if treat ==1       /* observed y for tx */
local pom_t = r(mean)

qui:sum yhat                     /* predicted y for tx if NOT on Tx */
local pom_c = r(mean)      
di "ATET="`pom_t' - `pom_c'

* ATE = difference (individual level)
generate diff = pom_t-pom_c
qui: summ diff
di "ATE=" r(mean)

*ATET = difference (individual level)
qui: summ diff if treat==1
di "ATET=" r(mean)

*ATET with regression & margins 
qui reg score i.treat##(c.income)
margins r.treat, subpop(treat)

***************
*RA: Example 2
***************

clear
*use "${data}/cattaneo2.dta", clear
use "${Git_data}/cattaneo2.dta", clear

*just one X variable (mage)
rename mbsmoke treat
teffects ra (bweight mage) (treat), pomeans aequations nolog
teffects ra (bweight mage) (treat), ate nolog
teffects ra (bweight mage) (treat), atet nolog
predict te                        /* te */
predict pom_c, cmean              /* predicted y if control */
qui:generate pom_t = (pom_c + te)     /* predicted y if Tx */

*POM: everyone on treatment vs everyone on control.
summ pom_c      /* POM (control) */
summ pom_t      /* POM (treat) */
drop te pom_c pom_t

*predicted y if control.
qui:regress bweight mage if treat==0
predict pom_c

*predicted y if treat.
qui:regress bweight mage if treat==1
predict pom_t

qui: generate te = (pom_t - pom_c)
summ te                      /* ATE */
summ te if treat==1          /* ATET */

/*
The ATE is the average of the differences between the birthweights 
when each mother smokes and the birthweights when no mother smokes. 
The average birthweight if all mothers were to smoke would be 240 grams less 
than the average of 3,403 grams that would occur if none of the mothers had smoked. 
The average amount by which infants weights are affected by their mothers decision to smoke. 

The ATET is the average amount by which the weight of babies **born to smoking mothers** 
was decreased as a result of smoking. The average birthweight is 223 grams less 
when all the mothers who smoke do so (Tx=1) than the average
of 3,361 grams that would have occurred if none of these mothers had smoked.
This is a comparison only among those treated. The ATET differs from the ATE because 
the distribution of the covariates among mothers who smoke (Tx=1) differs from the 
distribution of the covariates for nonsmoking mothers (Tx=0).
*/


****************
**RA: Example 3
****************

clear
*use "${data}/cattaneo2.dta", clear
use "${Git_data}/cattaneo2.dta", clear
teffects ra (bweight fage mage mmarried prenatal1) (mbsmoke), pomeans nolog
teffects ra (bweight fage mage mmarried prenatal1) (mbsmoke), ate nolog
teffects ra (bweight fage mage mmarried prenatal1) (mbsmoke), atet nolog
*(Tx - control) for all observations.
qui:regress bweight fage mage mmarried prenatal1 if mbsmoke==0
predict pom_c
qui:regress bweight fage mage mmarried prenatal1 if mbsmoke==1
predict pom_t

* ATE = difference between two POMs.
qui:sum pom_t
local pom_t = r(mean)    
qui:sum pom_c
local pom_c = r(mean)
di "ATE="`pom_t' - `pom_c' 

*ATET.
*For those on Tx: compare observed vs counterfactual. 
regress bweight fage mage mmarried prenatal1 if mbsmoke==0
*(i) obtain coefficients from model on the control.
*(ii) use those coefficients with observed X for the treated.
predict yhat if mbsmoke == 1  

* compare POMs
qui:sum bweight if mbsmoke ==1     /* observed y for tx */
local pom_t = r(mean)

qui:sum yhat              /* predicted y for tx if NOT on Tx */
local pom_c = r(mean)      
di "ATET="`pom_t' - `pom_c'

*Individual level.
generate diff = pom_t - pom_c
qui: summ diff
di "ATE=" r(mean)
qui: summ diff if mbsmoke==1
di "ATET=" r(mean)

****************************
*RA: Example 4.
****************************

clear
set seed 6795
set obs 1000
gen id=_n
gen treat = uniform() < .2
gen outcome= runiformint(1,10)
gen x= runiformint(1,10)

teffects ra (outcome x) (treat), ate nolog
teffects ra (outcome x) (treat), atet nolog

qui: regress outcome x if treat==0
predict pom_c
qui: regress outcome x if treat==1
predict pom_t
qui:sum pom_t
local pom_t = r(mean)    
qui:sum pom_c
local pom_c = r(mean)
di "ATE="`pom_t' - `pom_c' 

*ATET.
*For those on Tx: compare observed vs counterfactual. 
qui:regress outcome x if treat==0
*(i) obtain coefficients from model on the control.
*(ii) use those coefficients with observed X for the treated.
predict yhat if treat == 1
  
* compare POMs
qui:sum outcome if treat ==1       /* observed y for tx */
local pom_t = r(mean)
qui:sum yhat                     /* predicted y for tx if assumed NOT on Tx */
local pom_c = r(mean)      
di "ATET="`pom_t' - `pom_c'



************************************************************
*Estimator 2: Inverse-probability-weighted (IPW) estimators
************************************************************

/*

IPW estimators use a model for the treatment to make the outcome conditionally independent of the treatment (ignorability). 
If this model is well specified, it will also balance the covariates 
(Xs are the confounders; related to both treatment status and outcome). 
*/


*****************
*IPW: Example 1
*****************

*Regression of Y~Tx weighted by ipw. 

clear
input id	treat	score	income
1	0	73	2761
2	0	74	1254
3	0	70	2607
4	0	69	1966
5	0	73	2760
6	1	68	3385
7	1	68	3472
8	1	71	3914
9	1	70	2438
10	1	72	3020
end
teffects ipw (score) (treat income, logit), pomeans
teffects ipw (score) (treat income, logit), ate
teffects ipw (score) (treat income, logit), atet 
tebalance summarize

/*
ATE:
Logit model of treatment.
generate predicted probability of treatment (pscore).
calculate IPW:
IPW = 1/(1-pscore) IF Tx==0.
IPW = 1/(pscore) IF Tx==1.
estimate mean Y using IPW weights
calculate the difference.
*/


qui:logit treat c.income    /* model for Tx */
predict pscore
generate ipw=0
replace ipw=1/(1-pscore) if treat==0
replace ipw=1/(pscore) if treat==1

qui: summ score [aweight=ipw] if treat==0
local pom_c = r(mean)
qui: summ score [aweight=ipw] if treat==1
local pom_t = r(mean)
di "ATE="`pom_t' - `pom_c'

*regression framework (msm) using the weight.
regress score treat [aweight=ipw]
svyset [pweight=ipw]
svy: glm score i.treat,family(gaussian) link(identity)

/*
ATET: For those on Tx: compare observed vs counterfactual. 
*/

generate ipw1=0
replace ipw1=pscore/(1-pscore) if treat==0
replace ipw1=1 if treat==1
qui: summ score if treat==0 [aweight=ipw1]
local pom_c = r(mean)
qui: summ score if treat==1 [aweight=ipw1]
local pom_t = r(mean)
di "ATET="`pom_t' - `pom_c'

regress score treat [aweight=ipw1]
svyset [pweight=ipw1]
svy: glm score i.treat,family(gaussian) link(identity)

*smd using -covbal-
*SMDs < 0.1 are often considered negligible and suggest that groups are balanced.

drop pscore
qui: logit treat income
predict pscore, pr
gen iptw = cond(treat, 1/pscore, 1/(1-pscore))
covbal treat income
covbal treat income, wt(iptw) for(%9.3f)

*smd by hand
qui: summ income if treat==0 [aw=iptw]
local c1 = r(mean)
local c2 = r(Var)
qui: summ income if treat==1 [aw=iptw]
local t1 = r(mean)
local t2 = r(Var)
di (`t1'-`c1')/sqrt((`c2'+`t2')/2)


******************************.
**IPW: Example 2
******************************.

clear
*use "${data}/cattaneo2.dta", clear
use "${Git_data}/cattaneo2.dta", clear
rename mbsmoke treat
teffects ipw (bweight) (treat fage mage mmarried fbaby, logit), pomeans nolog
teffects ipw (bweight) (treat fage mage mmarried fbaby, logit), ate nolog
teffects ipw (bweight) (treat fage mage mmarried fbaby, logit), atet nolog

*Calculate by hand.
logit treat fage mage mmarried fbaby
predict pscore
generate ipw=0
replace ipw=1/(1-pscore) if (treat==0)
replace ipw=1/pscore if (treat==1)
svyset [pweight=ipw]
svy:mean bweight,over(treat) coeflegend
*ATE
lincom _b[c.bweight@1.treat] - _b[c.bweight@0bn.treat] 

*ATE
regress bweight treat [aweight=ipw]
svyset [pweight=ipw]
svy: glm bweight i.treat,family(gaussian) link(identity)

*ATET.
drop ipw
svyset,clear
generate ipw=0
replace ipw=pscore/(1-pscore) if (treat==0)
replace ipw=1 if (treat==1)
svyset [pweight=ipw]
svy:mean bweight,over(treat) coeflegend
lincom _b[c.bweight@1.treat] - _b[c.bweight@0bn.treat] 
*model
regress bweight treat [aweight=ipw]
svy: glm bweight i.treat,family(gaussian) link(identity)

****************************
**IPW: Example 3
****************************

clear
*use "${data}/cattaneo2.dta", clear
use "${Git_data}/cattaneo2.dta", clear
rename mbsmoke treat
teffects ipw (bweight) (treat mmarried mage prenatal1 fbaby), nolog
teffects ipw (bweight) (treat mmarried mage prenatal1 fbaby), nolog atet

/*
The tebalance postestimation commands produce diagnostic statistics, 
test statistics, and diagnostic plots to assess whether a teffects command 
balanced the covariates over treatment levels. A perfectly balanced 
covariate has a standardized difference of zero and variance ratio of one.
*/

*Summarizing the covariates by group in the raw data by specifying the baseline option.
tebalance summarize, baseline
tebalance summarize
*null hypothesis is that the Tx model balanced the covariates.
tebalance overid, nolog  

qui:logit treat mmarried mage prenatal1 fbaby   /* model for Tx */
predict pscore
generate ipw=0
replace ipw=1/(1-pscore) if treat==0
replace ipw=1/(pscore) if treat==1

qui: summ bweight [aweight=ipw] if treat==0
local pom_c = r(mean)
qui: summ bweight [aweight=ipw] if treat==1
local pom_t = r(mean)
di "ATE="`pom_t' - `pom_c'


****************************
**IPW: Example 4
****************************

clear
set seed 6795
set obs 1000
gen id=_n
gen treat = uniform() < .2
gen outcome= runiformint(1,10)
gen x= runiformint(1,10)

teffects ipw (outcome) (treat x, logit), ate nolog
teffects ipw (outcome) (treat x, logit), atet nolog

*Calculate by hand.
logit treat x
predict pscore
generate ipw=0
replace ipw=1/(1-pscore) if (treat==0)
replace ipw=1/pscore if (treat==1)
svyset [pweight=ipw]
svy:mean outcome,over(treat) coeflegend
*ATE
lincom _b[c.outcome@1.treat] - _b[c.outcome@0bn.treat] 

*ATE via regress.
regress outcome treat [aweight=ipw]
svyset [pweight=ipw]
svy: glm outcome i.treat,family(gaussian) link(identity)

*ATET (counterfactual vs observed).
*different weight for those on Tx.
drop ipw
svyset,clear
generate ipw=0
replace ipw=pscore/(1-pscore) if (treat==0)
replace ipw=1 if (treat==1)
svyset [pweight=ipw]
svy:mean outcome,over(treat) coeflegend
lincom _b[c.outcome@1.treat] - _b[c.outcome@0bn.treat] 
*model
regress outcome treat [aweight=ipw]
svy: glm outcome i.treat,family(gaussian) link(identity)


********************************************************************
*Estimator 3: IPWRA: IPW with regression adjustment estimator (doubly-robust I).
********************************************************************

/*
RA estimators model the outcome to account for nonrandom treatment assignment. 
IPW estimators model the treatment to account for nonrandom treatment assignment. 
IPWRA estimators model both the outcome and the treatment to account for nonrandom treatment assignment.

IPWRA uses IPW weights to estimate corrected regression coefficients 
that are subsequently used to perform regression adjustment. 
The covariates in the outcome model and the treatment model 
do not have to be the same, and they often are not because the variables 
that influence a subject's selection of treatment group 
are often different from the variables associated with the outcome. 

The IPWRA estimator has the **double-robust** property, which means that the estimates 
of the effects will be consistent if either the treatment model or the outcome model 
- but not both - are misspecified. 

Conceptually, IP weighting:

1. Estimates selection to treatment (treatment model)
2. Predicts treatment for all observations
3. Assigns:
(i) the inverse of probability of treatment for treated individuals and 
(ii) the inverse probability of not being treated for control individuals
4. Re-estimates the outcome model using these new weights

The IP weights (i) magnify treatment individuals who otherwise look like they would not have selected treatment 
and (ii) magnify control individuals who otherwise look like they would have selected treatment. 
We create counterfactuals where they are not observed in the data.

*/

*******************
**IPWRA: Example 1
*******************

clear
input id	treat	score	income
1	0	73	2761
2	0	74	1254
3	0	70	2607
4	0	69	1966
5	0	73	2760
6	1	68	3385
7	1	68	3472
8	1	71	3914
9	1	70	2438
10	1	72	3020
end
teffects ipwra (score income) (treat income, logit), pomeans aequations nolog
teffects ipwra (score income) (treat income, logit), ate nolog
teffects ipwra (score income) (treat income, logit), atet nolog
tebalance density income
*tebalance summarize
*tebalance overid, nolog

/*
ATE:
(1) Logit model of treatment:
generate predicted probability of treatment (p).
IPW = 1/(1-p) IF treatment==0.
IPW = 1/(p) IF treatment==1.
(2) Use RA method using IPW.
Step 1.
Fit two separate regressions and generate yhat:
Treat=0; Treat=1.
Step 2.
Calculate difference between 2 predicted values.
summarise difference (ATE).
*/

qui: logit treat income
predict pscore if e(sample)
gen ipw=.
replace ipw = 1/(1-pscore) if treat ==0
replace ipw = 1/pscore if treat == 1
qui: reg score treat income [pweight= ipw] if treat ==0
predict pom_c
qui: reg score treat income [pweight= ipw] if treat ==1
predict pom_t
qui: summ pom_t
local pom_t = r(mean)
qui: summ pom_c
local pom_c = r(mean)
di "ATE="`pom_t' - `pom_c'

*Running a single model does not produce the same results as teffects.
*See: http://sites.utexas.edu/prc/files/IPWRA.pdf.
*Teffects estimates treatment-specific predicted outcomes (POs) **FOR EACH SUBJECT**
*then computes the means of these POs. 
*These are contrasted to estimate the ATE and ATET.

svyset [pweight=ipw]
svy: glm score i.treat income,family(gaussian) link(identity)

*************************
*IPWRA: Example 2
*************************

clear
*use "${data}/cattaneo2.dta", clear
use "${Git_data}/cattaneo2.dta", clear
rename mbsmoke treat
teffects ipwra (bweight mage prenatal1 mmarried fbaby)                ///
                 (treat mmarried c.mage fbaby medu, logit)   ///
                 , pomeans aequations nolog

				 teffects ipwra (bweight mage prenatal1 mmarried fbaby)                ///
(treat mmarried c.mage fbaby medu, logit)   ///
, ate nolog


teffects ipwra (bweight mage prenatal1 mmarried fbaby)                ///
(treat mmarried c.mage fbaby medu, logit)   ///
, atet nolog
*teffects overlap, nolog

*ATE.
qui: logit treat mmarried c.mage fbaby medu
predict pscore
generate ipw=0
replace ipw=1/(1-pscore) if (treat==0)
replace ipw=1/pscore if (treat==1)

*By hand: fit separate regressions.
qui: regress bweight mage prenatal1 mmarried fbaby if treat==0 [pw=ipw]
predict pom_c
qui: regress bweight mage prenatal1 mmarried fbaby if treat==1 [pw=ipw]
predict pom_t
qui: summ pom_c              
qui: summ pom_t              
generate te=pom_t - pom_c
summ te                             

*I could not replicate the ATET.


*********************
*IPWRA: Example 3
*********************

*use "${data}/cattaneo2.dta", clear
use "${Git_data}/cattaneo2.dta", clear
qui: teffects ipwra (bweight fage mage mmarried prenatal1) ///
(mbsmoke fage mage mmarried fbaby,logit), pomeans aequations nolog
teffects ipwra (bweight fage mage mmarried prenatal1) ///
(mbsmoke fage mage mmarried fbaby,logit), ate aequations nolog

qui: logit mbsmoke fage mage mmarried fbaby
predict pscore
generate ipw=0
replace ipw=1/(1-pscore) if mbsmoke==0
replace ipw=1/(pscore) if mbsmoke==1
qui: reg bweight fage mage mmarried prenatal1 if mbsmoke==1 [pweight=ipw]
predict double pom_t
qui: reg bweight fage mage mmarried prenatal1 if mbsmoke==0 [pweight=ipw]
predict double pom_c
qui: summ pom_t
local pom_t = r(mean)
qui: summ pom_c
local pom_c = r(mean)
di "ATE="`pom_t' - `pom_c'

******************************
**IPWRA: Example 4
******************************

clear
set seed 6795
set obs 1000
gen id=_n
gen treat = uniform() < .2
gen outcome= runiformint(1,10)
gen x= runiformint(1,10)
teffects ipwra (outcome x) (treat x,logit), ate nolog

qui: logit treat x
predict pscore
generate ipw=0
replace ipw=1/(1-pscore) if treat==0
replace ipw=1/(pscore) if treat==1
*separate regressions: weighted by ipw
qui: reg outcome x if treat==1 [pweight=ipw]
predict double pom_t
qui: reg outcome x if treat==0 [pweight=ipw]
predict double pom_c
qui: summ pom_t
local pom_t = r(mean)
qui: summ pom_c
local pom_c = r(mean)
di "ATE="`pom_t' - `pom_c'

********************************************************************
*Estimator 4: The augmented IPW estimator (doubly-robust II)
********************************************************************

/*
IPWRA estimators model both the outcome and the treatment to account for nonrandom 
treatment assignment. So do AIPW estimators. The AIPW estimator adds a bias-correction term to the IPW estimator. 
(i) If the treatment model is correctly specified, the bias-correction term is 0 and the model is reduced to the IPW estimator. 
(ii) If the treatment model is misspecified but the outcome model is correctly specified, the bias-correction term corrects the estimator. 
Thus, the bias-correction term gives the AIPW estimator the same double-robust property as the IPWRA estimator.
*/

******************
*AIPW: Example 1
*******************

clear
input id	treat	score	income
1	0	73	2761
2	0	74	1254
3	0	70	2607
4	0	69	1966
5	0	73	2760
6	1	68	3385
7	1	68	3472
8	1	71	3914
9	1	70	2438
10	1	72	3020
end
teffects aipw (score income) (treat income, logit), pomeans
teffects aipw (score income) (treat income, logit), ate
qui: logit treat income
predict pscore if e(sample)

*Need two separate weights
gen double ipw0 = 0.treat/(1-pscore)
gen double ipw1 = 1.treat/pscore 

*POM (treatment)
*No weights in outcome model 
qui: reg score income if treat == 1
predict double pom1
*Use weight to correct prediction (note observed outcome here)
replace pom1 = pom1 + ipw1*(score - pom1)

*POM (control)
qui: reg score income if treat == 0
predict double pom0
replace pom0 = pom0 + ipw0*(score - pom0)
qui: sum pom0
local pom_c = r(mean)
qui: sum pom1
local pom_t = r(mean)
di "AIPW="`pom_t' - `pom_c'	

*******************
* AIPW: Example 2
*******************

clear
*use "${data}/cattaneo2.dta", clear
use "${Git_data}/cattaneo2.dta", clear
rename mbsmoke treat
teffects aipw (bweight mage prenatal1 mmarried fbaby)                 ///
                (treat mmarried c.mage fbaby medu, logit)    ///
                , pomeans aequations nolog

				teffects aipw (bweight mage prenatal1 mmarried fbaby)                 ///
                (treat mmarried c.mage fbaby medu, logit)    ///
                , ate nolog
*teffects overlap, nolog	

qui: logit treat mmarried c.mage fbaby medu
predict pscore if e(sample)
*Need two separate weights
gen double ipw0 = 0.treat/(1-pscore)
gen double ipw1 = 1.treat/pscore 

*POM (treatment)
*No weights in outcome model 
qui: reg bweight mage prenatal1 mmarried fbaby if treat == 1
predict double pom1
*Use weight to correct prediction (note observed outcome here)
replace pom1 = pom1 + ipw1*(bweight - pom1)

*POM (control)
qui: reg bweight mage prenatal1 mmarried fbaby if treat == 0
predict double pom0
replace pom0 = pom0 + ipw0*(bweight - pom0)
qui: sum pom0
local pom_c = r(mean)
qui: sum pom1
local pom_t = r(mean)
di "AIPW="`pom_t' - `pom_c'	


*********************************
*AIPW: Example 3
*********************************.

clear
set seed 6795
set obs 1000
gen id=_n
gen treat = uniform() < .2
gen outcome= runiformint(1,10)
gen x= runiformint(1,10)
teffects aipw (outcome x) (treat x, logit), ate

*propensity score.
qui: logit treat x
predict pscore if e(sample)

*Need two separate weights
gen double ipw0 = 0.treat/(1-pscore)
gen double ipw1 = 1.treat/pscore 

*POM (treatment)
*No weights in outcome model 
qui: reg outcome x if treat==1
predict double pom1
*Use weight to correct prediction (note observed outcome here)
replace pom1 = pom1 + ipw1*(outcome - pom1)

*POM (control)
qui: reg outcome x if treat==0
predict double pom0
replace pom0 = pom0 + ipw0*(outcome - pom0)
qui:sum pom0
local pom_c = r(mean)
qui:sum pom1
local pom_t = r(mean)
di "AIPW="`pom_t' - `pom_c'	

********************************************************************
*Estimator 5: Matching estimators
********************************************************************

/*

Matching methods estimate causal effects only among the treated population. 
We do not have the average causal effect in the population. The distribution of X's in the control group 
is adjusted to match that in the treated group. The target population is therefore a subset 
(the treated population): we compare potential outcomes among the treated population: 
if all those in the Tx group receive the Tx versus no-one in the Tx group receiving the Tx.

Matching pairs the observed outcome of a person in one treatment group 
with the outcome of the "closest" person in the other treatment group. 
The outcome of the closest person is used as a prediction for the missing potential outcome (counterfactual). 
The average difference between the observed outcome and the predicted outcome estimates the ATE.	

First, there is a cost to matching on continuous covariates: the inability to find good matches with more than one continuous 
covariate causes large-sample bias in our estimator because our matches become increasingly poor. Second, we must specify a measure of similarity. 
When matching directly on the covariates, distance measures are used and the nearest neighbor selected. 
An alternative is to match on an estimated probability of treatment, known as the propensity score.

*Direct matching*: matching directly on the confounders (e.g. distance score then NNM): 
not matching via the propensity score. In observational studies confounders 
occur before the treatment decision (paths leading from pre-treatment covariates X to Tx). 
Balance the Tx groups with respect to X. We identify X via a DAG. Then need to avoid confounding bias (e.g. matching; IPTW). 

There is no path from pre-treatment covariates X to Tx in a RCT (no backdoor path from Tx to Y): the design phase of a RCT ensures a
causal Tx effect (covariate balance by design). Balanced with respect to observed and unobserved X.

In observational studies, Tx groups differ on X. Matching: match units in the treatment group to units in the control group
on the pre-treatment covariates X. Once data are matched estimation can proceed as in a RCT. But only estimates causal effects 
among the treated population. Try to achieve covariate balance (stochastic balance). Typically with matching: we are making
inferences about the treated population.

*Direct matching: (i) a metric of distance (e.g. Mahalanobis distance) and (ii) a method of selecting matches (e.g. NNM or optimal matching). 
For each unit in the treated group we have a distance score for each pair (Tx and every unit in the control group). 

*Calipers*: make the positivity assumption more plausible. Exclude treated subjects for whom there does not exist a good match.
A bad match can be defined using a caliper: maximum acceptable distance. Only match a treated subject if the best match has 
distance less than the caliper. Excluding subjects with no matches within caliper may improve balance: but makes population harder to define.

*Analysing data after matching: 
(1) Assess covariate balancing via standardised differences: absolute difference (values >.2 indicate serious imbalance).
(2) Estimate the Tx effect (among the treated) taking the matching into account. Treat data as if it were a RCT.
Randomisation tests (exact tests; permutation tests)
Matched pairs: McNemars test (binary); paired t-test (continuous);
conditional logistic regression; stratified Cox model; GEE models

*********************************
*Nearest-neighbor matching
*********************************

Nearest-neighbor matching (NNM) uses distance between covariate patterns to define "closest". There are many ways to define the distance 
between two covariate patterns. We could use squared differences as a distance measure, but this measure ignores problems with scale and covariance. 
Weighting the differences by the inverse of the sample covariance matrix handles these issues. 
NNM estimators are flexible; it estimates the ATE for a much wider class of models. The cost of this flexibility is that the 
NNM estimator requires much more data and the amount of data it needs grows with each additional 
continuous covariate.

*/


*******************************
*NNM: Example 1
********************************

*NNM on continous covariates
clear
*use "${data}/cattaneo2.dta", clear
use "${Git_data}/cattaneo2.dta", clear
teffects nnmatch (bweight mmarried mage) (mbsmoke),generate(EX1matchv)
*tebalance summarize

*NNM on discrete covariates
teffects nnmatch (bweight mmarried mage) (mbsmoke), ematch(mmarried prenatal1) generate(EX2matchv)
*tebalance summarize
tebalance box mage
*graph export "figure11.png", replace
tebalance density mage
*graph export "figure12.png", replace

****************
*NNM: Example 2
****************

clear
input id	treat	score	income2
1	0	73	5
2	0	74	4
3	0	70	3
4	0	69	2
5	0	73	1
6	1	68	1
7	1	68	2
8	1	71	3
9	1	70	4
10	1	72	5
end
qui: teffects nnmatch (score income2) (treat),  nneighbor(1)  generate(mid)
teffects nnmatch (score income2) (treat),  nneighbor(1)  ate
teffects nnmatch (score income2) (treat),  nneighbor(1)  atet

*Original ID variable is ID: Stata creates mid1.

*match the score using match id.
preserve
keep mid1 score
rename mid1 id
rename score mscore
sort id
save "${outputs}\Temp1.dta",replace
restore
*match into analysis dataset
sort id
merge 1:1 id using "${outputs}\Temp1.dta"

*ATE = (Tx - control).
generate diff=0
replace diff = (mscore - score) if treat==0
replace diff = (score - mscore) if treat==1
qui: summ diff
di "ATE=" r(mean)

qui: summ diff if treat==1
di "ATET=" r(mean)

/*
teffects nnmatch estimates the ATE and ATT from observational data by NNM. 
NNM estimators impute the missing potential outcome for each subject by using an average of the outcomes of 
similar subjects that receive the other treatment level.  Similarity between subjects is based on a weighted 
function of the covariates for each observation. The treatment effect is computed by taking the average of the difference between the observed 
and imputed potential outcomes **FOR EACH SUBJECT**.
*/

********************
**NNM: Example 3
********************

clear
input id	treat	score	income2
1	0	73	5
2	0	74	5
3	0	70	3
4	0	69	2
5	0	73	1
6	1	68	1
7	1	68	2
8	1	71	3
9	1	70	4
10	1	72	5
end

preserve
keep id score
sort id
save "${outputs}\scores.dta",replace
restore


qui: teffects nnmatch (score income2) (treat),  nneighbor(1)  generate(mid)
teffects nnmatch (score income2) (treat),  nneighbor(1)  ate
teffects nnmatch (score income2) (treat),  nneighbor(1)  atet

*Attach the OBS score using the correct id. Then use id1 for merging later on. 

preserve
keep mid1 
gen id = mid1
merge m:1 id using "${outputs}\scores.dta", noreport
keep if _merge==3
rename score mscore1
keep mid1 mscore1
sort mid1 
save "${outputs}\Temp1.dta",replace
restore


preserve
keep mid2 
gen id = mid2
merge m:1 id using "${outputs}\scores.dta", noreport
keep if _merge==3
rename score mscore2
keep mid2 mscore2
sort mid2 
save "${outputs}\Temp2.dta",replace
restore

preserve
keep mid3 
gen id = mid3
merge m:1 id using "${outputs}\scores.dta", noreport
keep if _merge==3
rename score mscore3
keep mid3 mscore3
sort mid3 
save "${outputs}\Temp3.dta",replace
restore

sort mid1
merge m:m mid1 using "${outputs}\Temp1.dta", nogen noreport
sort mid2
merge m:m mid2 using "${outputs}\Temp2.dta", nogen noreport
sort mid3
merge m:m mid3 using "${outputs}\Temp3.dta", nogen noreport
sort id

*average of the matched scores.
egen mscore = rowmean(mscore1 mscore2 mscore3)

*ate = (Tx - control).
generate diff=0
replace diff = (mscore - score) if treat==0        
replace diff = (score - mscore) if treat==1        
qui: summ diff
di "ATE=" r(mean)
qui: summ diff if treat==1
di "ATET=" r(mean)

***************
**NNM: Example 4
***************

*use "${data}\cattaneo2.dta", clear
use "${Git_data}/cattaneo2.dta", clear

gen id=_n
teffects nnmatch (bweight mmarried mage fage medu prenatal1) (mbsmoke), ///
ematch(mmarried prenatal1) nneighbor(1)  generate(mid)
teffects nnmatch (bweight mmarried mage fage medu prenatal1) (mbsmoke), ///
ematch(mmarried prenatal1) nneighbor(1) atet

preserve
keep id bweight
sort id
save "${outputs}\bweight.dta",replace
restore

forvalues i = 1(1)16 {
preserve
keep mid`i'
gen id = mid`i'
merge m:1 id using "${outputs}\bweight.dta", noreport
keep if _merge==3
rename bweight mbweight`i'
keep mid`i' mbweight`i'
sort mid`i' 
save "${outputs}\Temp`i'.dta",replace
restore
}

forvalues i = 1(1)16 {
sort mid`i'	
merge m:m mid`i' using "${outputs}\Temp`i'.dta", nogen noreport
}

egen mbweight = rowmean(mbweight1-mbweight16)
*ATE = (Tx - control).
generate diff=0
replace diff = (mbweight - bweight) if mbsmoke==0        
replace diff = (bweight - mbweight) if mbsmoke==1       
qui: summ diff
di "ATE=" r(mean)
qui: summ diff if mbsmoke==1
di "ATET=" r(mean)


**************************
*NNM
**************************

clear
set seed 67952
set obs 1000
gen id=_n
gen treat = uniform() < .4
gen outcome= runiformint(1,10)
gen x1= runiformint(1,10)
gen x2= runiformint(1,10)
teffects nnmatch (outcome x1 x2) (treat), nneighbor(1) metric(mahalanobis) ate nolog gen(match) 
teffects nnmatch (outcome x1 x2) (treat), nneighbor(1) metric(mahalanobis) atet nolog

preserve
keep id outcome
sort id
save "${outputs}\outcome.dta",replace
restore

*max number of matches
local nmatch = e(k_nnmax)

forvalues i = 1(1)`nmatch' {
preserve
keep match`i'
gen id = match`i'
merge m:1 id using "${outputs}\outcome.dta", noreport
keep if _merge==3
rename outcome m_outcome`i'
keep match`i' m_outcome`i'
sort match`i' 
save "${outputs}\Temp`i'.dta",replace
restore
}

forvalues i = 1(1)`nmatch' {
sort match`i'	
merge m:m match`i' using "${outputs}\Temp`i'.dta", nogen noreport
}

egen m_outcome = rowmean(m_outcome1-m_outcome`nmatch')

*ATE = (Tx - control).
generate diff=0
replace diff = (m_outcome - outcome) if treat==0        
replace diff = (outcome - m_outcome) if treat==1       
qui: summ diff
di "ATE=" r(mean)
qui: summ diff if treat==1
di "ATET=" r(mean)


*************************************
**Matching on a propensity score
**************************************

/*
NNM uses bias adjustment to remove the bias caused by matching on more than 
one continuous covariate. The generality of this approach makes it very 
appealing, but it can be difficult to think about issues of fit and 
model specification.  Propensity-score matching (PSM) matches on an estimated 
probability of treatment known as the propensity score. There is no need 
for bias adjustment because we match on only one continuous covariate (the PS score). 
PSM has the added benefit that we can use all the standard methods for checking the 
fit of binary regression models prior to matching.
*/

******************
*PSM: Example 1
*******************

*using a caliper: specify the maximum distance for 
*which two observations are potential neighbors
clear
*use "${data}\cattaneo2.dta"
use "${Git_data}/cattaneo2.dta", clear
teffects psmatch (bweight) (mbsmoke mmarried mage fage),generate(mid) caliper(0.1)
*tebalance summarize
*tebalance box
*tebalance density

***************
*PSM: Example 3
***************

clear
input id	treat	score	income2
1	0	73	4
2	0	74	4
3	0	70	2
4	0	69	6
5	0	73	5
6	1	68	6
7	1	68	6
8	1	71	5
9	1	70	4
10	1	72	7
end
teffects psmatch (score) (treat income2), gen(mid) ate
teffects psmatch (score) (treat income2), atet

preserve
keep id score
sort id
save "${outputs}\score.dta",replace
restore

forvalues i = 1(1)2 {
preserve
keep mid`i'
gen id = mid`i'
merge m:1 id using "${outputs}\score.dta", noreport
keep if _merge==3
rename score mscore`i'
keep mid`i' mscore`i'
sort mid`i' 
save "${outputs}\Temp`i'.dta",replace
restore
}

forvalues i = 1(1)2 {
sort mid`i'	
merge m:m mid`i' using "${outputs}\Temp`i'.dta", nogen noreport
}

egen mscore = rowmean(mscore1-mscore2)
*ate = (Tx - control).
generate diff=0
replace diff = (mscore - score) if treat==0   /* Tx - control  */
replace diff = (score - mscore) if treat==1  /* Tx - control  */
qui: summ diff
di "ATE=" r(mean)

qui: summ diff if treat==1
di "ATET=" r(mean)


******************
**PSM: Example 4
******************

*use "${data}\cattaneo2.dta", clear
use "${Git_data}/cattaneo2.dta", clear

gen id=_n
teffects psmatch (bweight) (mbsmoke mmarried mage medu fbaby), nneighbor(1)  generate(mid) 
teffects psmatch (bweight) (mbsmoke mmarried mage medu fbaby), nneighbor(1)  atet 

preserve
keep id bweight
sort id
save "${outputs}\bweight.dta",replace
restore

forvalues i = 1(1)74 {
preserve
keep mid`i'
qui: gen id = mid`i'
merge m:1 id using "${outputs}\bweight.dta", noreport nonotes
qui: keep if _merge==3
rename bweight mbweight`i'
qui: keep mid`i' mbweight`i'
sort mid`i' 
qui: save "${outputs}\Temp`i'.dta",replace
restore
}

forvalues i = 1(1)74 {
sort mid`i'	
merge m:m mid`i' using "${outputs}\Temp`i'.dta", nogen noreport
}

egen mbweight = rowmean(mbweight1-mbweight74)
*ate = (Tx - control).
generate diff=0
replace diff = (mbweight - bweight) if mbsmoke==0        
replace diff = (bweight - mbweight) if mbsmoke==1        
qui: summ diff
di "ATE=" r(mean)
qui: summ diff if mbsmoke==1
di "ATET=" r(mean)

******************
* PSM: Example 5
******************

clear
input id	treat	score	income2	wealth
1	0	68	4	18
2	0	68	7	14
3	0	70	2	19
4	0	69	6	18
5	0	73	5	16
6	0	69	4	18
7	0	71	7	20
8	0	72	2	20
9	0	74	6	20
10	0	73	5	17
11	0	68	4	17
12	0	68	7	20
13	0	70	2	15
14	0	69	6	20
15	0	73	5	19
16	0	69	4	18
17	0	71	7	18
18	0	72	2	15
19	0	74	6	20
20	0	73	5	17
21	1	68	7	15
22	1	68	4	14
23	1	71	5	19
24	1	70	6	20
25	1	72	2	22
26	1	71	7	13
27	1	70	4	17
28	1	69	5	21
29	1	70	6	18
30	1	72	2	14
31	1	68	7	13
32	1	68	4	23
33	1	71	5	14
34	1	70	6	22
35	1	72	2	19
36	1	71	7	16
37	1	70	4	20
38	1	69	5	21
39	1	70	6	18
40	1	72	2	19
end
teffects psmatch (score) (treat income2 wealth,logit),gen(mid)
teffects psmatch (score) (treat income2 wealth,logit),atet

preserve
keep id score
sort id
save "${outputs}\score.dta",replace
restore

forvalues i = 1(1)3 {
preserve
qui: keep mid`i'
qui: gen id = mid`i'
qui: merge m:1 id using "${outputs}\score.dta", noreport
qui: keep if _merge==3
rename score mscore`i'
keep mid`i' mscore`i'
sort mid`i' 
save "${outputs}\Temp`i'.dta",replace
restore
}

forvalues i = 1(1)3 {
sort mid`i'	
merge m:m mid`i' using "${outputs}\Temp`i'.dta", nogen noreport
}

egen mscore = rowmean(mscore1-mscore3)
*ate = (Tx - control).
generate diff=0
replace diff = (mscore - score) if treat==0        
replace diff = (score - mscore) if treat==1        
qui: summ diff
di "ATE=" r(mean)

qui: summ diff if treat==1 
di "ATET=" r(mean)


************************************
*PSM: Example 6
************************************

clear
set seed 67952
set obs 1000
gen id=_n
gen treat = uniform() < .4
gen outcome= runiformint(1,10)
gen x1= runiformint(1,10)
gen x2= runiformint(1,10)
teffects psmatch (outcome) (treat x1 x2,logit),gen(match)
teffects psmatch (outcome) (treat x1 x2,logit),atet

local nmatch = e(k_nnmax) 

preserve
keep id outcome
sort id
save "${outputs}\outcome.dta",replace
restore

*max number of matches
local nmatch = e(k_nnmax)

forvalues i = 1(1)`nmatch' {
preserve
keep match`i'
gen id = match`i'
merge m:1 id using "${outputs}\outcome.dta", noreport
keep if _merge==3
rename outcome m_outcome`i'
keep match`i' m_outcome`i'
sort match`i' 
save "${outputs}\Temp`i'.dta",replace
restore
}

forvalues i = 1(1)`nmatch' {
sort match`i'	
merge m:m match`i' using "${outputs}\Temp`i'.dta", nogen noreport
}

egen m_outcome = rowmean(m_outcome1-m_outcome`nmatch')

*ATE = (Tx - control).
generate diff=0
replace diff = (m_outcome - outcome) if treat==0        
replace diff = (outcome - m_outcome) if treat==1       
qui: summ diff
di "ATE=" r(mean)
qui: summ diff if treat==1
di "ATET=" r(mean)


*********************************************
*Standardisation (binary confounder)
*********************************************

/*Predicted scores from regressing Y ~ X separately by Tx status: standardisation. 
We stratify (by Tx status) and then average over the 
distribution of X (the confounder). 
The Tx groups have the same distribution of X.
The standard distribution of X in this example is: 
X=0 (0.4545)
X=1 (0.5455)

When Tx=0: Effect = (Y(X=0) * Pr(X=0)) + (Y[X=1] * Pr(X=1))
When Tx=1: Effect = (Y(X=0) * Pr(X=0)) + (Y[X=1] * Pr(X=1))
When Tx=0: Effect = (Y(X=0) * 0.4545) + (Y[X=1] * 0.5455)
When Tx=1: Effect = (Y(X=0) * 0.4545) + (Y[X=1] * 0.5455)
*/

clear
input id death	treat	sex	count
1	0	0	0	3800
2	0	1	0	950
3	1	0	0	200
4	1	1	0	50
5	0	0	1	2700
6	0	1	1	2700
7	1	0	1	300
8	1	1	1	300
end

*death as the outcome.
*sex as the potential confounder.
teffects ra (death sex, logit) (treat) [fweight=count], pom
teffects ra (death sex, logit) (treat) [fweight=count], ate
teffects ra (death sex, logit) (treat) [fweight=count], atet

qui:tab treat death [fw=count], row
*Stratify on the X variable:
qui:tab treat death if sex==0 [fw=count], row
qui:tab treat death if sex==1 [fw=count], row
*distribution of confounder over the whole sample (weights for wtd average)
local x0 (5000/11000)
local x1 (6000/11000)
*Mean potential outcomes for each group.
*By averaging over the distribution of X.
*If EVERYONE had Tx vs if EVERYONE did not have Tx.

*E(Y for Control group)
*(rate for Control when X=0 * P(X=0) + rate for Control when X=1 * P(X=1) 
di (0.05)*`x0' + (0.1)*`x1'

*E(Y for Treatment group)
*(rate for Tx when X=0 * P(X=0) + rate for Tx when X=1 * P(X=1) 
di (0.05)*`x0' + (0.1)*`x1'

qui: logit death sex if treat==0 [fweight=count]
predict pom_c
qui: logit death sex if treat==1 [fweight=count]
predict pom_t
sum pom_t [fw=count]
local pom_t = r(mean)    
sum pom_c [fw=count]
local pom_c = r(mean)
di "ATE="`pom_t' - `pom_c' 

qui: logit death sex if treat == 0 [fw=count]
predict new_pom_c if treat == 1  
sum death if treat ==1 [fw=count]    /* observed y for tx */
local pom_t = r(mean)

sum new_pom_c [fw=count]             /* predicted y for tx if NOT on Tx */
local new_pom_c = r(mean)      
di "ATET="`pom_t' - `new_pom_c'


*****************************************************************
*binary outcome (standardization)
*distribution of confounder = the same for Tx and control groups
*****************************************************************

clear
input id death	sex	treat	count
1	0	0	0	4100
2	1	0	0	200
3	0	0	1	950
4	1	0	1	80
5	0	1	0	2700
6	1	1	0	300
7	0	1	1	2700
8	1	1	1	300
end

*death as the outcome.
*sex as the potential confounder.
teffects ra (death sex, logit) (treat) [fweight=count], pom
teffects ra (death sex, logit) (treat) [fweight=count], ate

**********************************
*Binary outcomes
***********************************

*use "${data}\cattaneo2.dta", clear
use "${Git_data}/cattaneo2.dta", clear
teffects ra (lbweight mage medu mrace, logit) (mbsmoke)
teffects ra (lbweight mage medu mrace, logit) (mbsmoke), atet

qui: logit lbweight mage medu mrace if mbsmoke ==1 
predict pom_t 
qui: logit lbweight mage medu mrace if mbsmoke ==0 
predict pom_c 
sum pom_t
local pom_t = r(mean)
sum pom_c
local pom_c = r(mean)
di "ATE="`pom_t' - `pom_c'

*atet
qui: logit lbweight mage medu mrace if mbsmoke ==0 
predict new_pom_c if mbsmoke == 1  
sum lbweight if mbsmoke ==1     /* observed y for tx */
local pom_t = r(mean)
sum new_pom_c          /* predicted y for tx if NOT on Tx */
local new_pom_c = r(mean)      
di "ATET="`pom_t' - `new_pom_c'


*What is causal inference?

/*
The estimand: what do we want to know? What are we trying to estimate? 
We have to connect our causal models to statistical procedures. For statistical models 
to produce scientific insight, they require additional scientific (causal) models. 
The reasons for a statistical analysis are not found in the data themselves, but rather in the causes 
of the data. The causes of the data cannot be extracted from the data alone. 
How the sample differs from the population: the right way to adjust for sampling bias is to understand what caused it. 
Causal inference is:

(i) More than association between variables
(ii) Is prediction of intervention (knowing a cause means being able to predict the consequences of an intervention)
(iii) Is imputation of missing observations (a counterfactual imagining) 
(knowing a cause means being able to construct unobserved counterfactual outcomes)

*Causal assumptions

What assumptions are necessary to estimate causal effects from observed data?
How do we link observed data to potential outcomes and so estimate average causal effects?

Identifiability of causal effects requires making some untestable assumptions (causal assumptions): 
assumptions about the observed data (outcome; treatment; pre-treatment covariates)

SUTVA: no interference (units do not interfere with each other): treatment assignment 
of one unit does not affect the outcome of another unit (no spillover/contagion). Only one version of treatment.

Consistency: potential outcomes equal to observed outcomes conditional on treatment status and X.

Ignorability: no unmeasured confounders: treatment is randomly assigned given the confounders X. Meeting this assumption requires 
conditioning on confounders. Treatment assignment becomes ignorable: given X, biases in treatment assignment are no longer an issue. 
Among people with the same X we can think of treatment as being randomly assigned. Confounding relates to ignorability: within 
the same X: we compare the Tx groups within homogenous populations. Within the same X units are not systematically different 
with respect to treatment assignment. Tx is ignorable: the Tx groups do not differ on pre-treatment covariates. 
(i) use DAGs to identify the correct X (minimum set to achieve ignorability: backdoor path criterion); 
(ii) use matching/IPTW to control for X and estimate causal effects; 
(iii) conduct sensitivity analyses. If a variable affects outcome but not treatment assignment than it is a risk factor, not a confounder. 
Confounders affect both treatment assignment and outcome.

Positivity: for every set of X (pre-treatment covariates) treatment assignment is not deterministic 
(tx options available at all values of X: not everyone receiving Tx if X above a certain level). 
Cross-sectional studies: selection bias as treatment groups may be systematically different with respect to X.
Incident user design: restrict the treated population to those newly initiating treatment: the causal effect is the effect of initiating treatment.
Active comparator: compare those who intiate Tx and those who initiate some other Tx. But this is a narrow causal question.
Incident user design with active comparator: compare newly initiated Tx versus newly initiated other Tx at time 0. 
No-one had the treatments prior to time 0. Confounders are measured prior to study.
*/

erase "${outputs}\scores.dta"
erase "${outputs}\score.dta"
erase "${outputs}\outcome.dta"
erase "${outputs}\bweight.dta"

forvalues i = 1/74 {
erase "${outputs}\Temp`i'.dta"
}

************************
***** FINISHED *********
************************



