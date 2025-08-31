/*
Regression adjustment estimator.
ATE:
(1) regress y on covariates (Tx=0) and obtain yhat for everyone
(2) regress y on covariates (Tx=1) and obtain yhat for everyone
(3) compute the difference (1 versus 0: ate is the mean
ATET:
(1) regress y on covariates (Tx=0) and obtain yhat for Tx=1: counterfactual
(2) mean y for (Tx=1): observed 
(3) compute the difference in these 2 means
*/

clear all
global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Causal Inference/Treatment effects/"
global data "${projdir}/data"
global outputs "${projdir}/outputs"
pwd
display c(current_date)

clear
set seed 6795
set obs 1000
gen id=_n
gen treat = uniform() < .2
gen outcome= runiformint(1,10)
gen x1= runiformint(1,10)
gen x2= runiformint(1,10)
teffects ra (outcome x1 x2) (treat), ate nolog
teffects ra (outcome x1 x2) (treat), atet nolog

qui: regress outcome x1 x2 if treat==0
predict pom_c
qui: regress outcome x1 x2 if treat==1
predict pom_t
qui:sum pom_t
local pom_t = r(mean)    
qui:sum pom_c
local pom_c = r(mean)
di "ATE="`pom_t' - `pom_c' // ATE.

*ATET.
*For those on Tx: compare observed vs counterfactual. 
regress outcome x1 x2 if treat==0
*(i) obtain coefficients from model on the control.
*(ii) use those coefficients with observed X for the treated.
predict yhat if treat == 1
  
* compare POMs
qui:sum outcome if treat ==1     /* observed y for tx */
local pom_t = r(mean)
qui:sum yhat                     /* predicted y for tx if NOT on Tx */
local pom_c = r(mean)      
di "ATET="`pom_t' - `pom_c'

di "Finished"




