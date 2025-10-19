*http://vanderwalresearch.com/blog/13-rtutorial-wtchr	

/*
numerator: confounding variable.
denominator: propensity score model

numerator: stabilisation factors (baseline confounders).
denominator: stabilisation factors + (time-varying) confounders.

The numerator contains the probability of the OBSERVED exposure level (Tx)
given observed values of stabilization factors (usually a set of baseline covariates).
These probabilities are estimated using the model regressing Tx on the terms in the numerator.
numerator: baseline covariates -> Treatment

The denominator contains the probability of the observed exposure level
given the observed values of a set of confounders, as well as the stabilisation factors in the numerator.
These probabilities are estimated using the model regressing Tx on the terms in the denominator.
denominator: baseline covariates + time-varying -> Treatment
*/


global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/01. IPTW/"
global data "${projdir}/data"
global outputs "${projdir}/outputs/Temp"

*N:Drive.
global projdir_a "N:/Learning/Longitudinal/01. IPTW/"
global data_a "${projdir_a}/data"
global outputs_a "${projdir_a}/outputs/Temp"


*Cross-sectional data: treatment/exposure balanced with respect to one confounder

clear 
set obs 2000
set seed 432905
gen y = runiformint(20,40)
gen Tx = uniform() < .5
gen confvar = runiformint(1,3)
teffects ipw (y) (Tx confvar, logit), ate nolog

*Calculate by hand.
logit Tx confvar             // model for the treatment
predict pscore
generate ipw=0
replace ipw=1/(1-pscore) if (Tx==0)
replace ipw=1/pscore if (Tx==1)
tab confvar Tx, col
tab confvar Tx [aw=ipw], col 

svyset [pweight=ipw]
svy:mean y,over(Tx) coeflegend
*ATE
lincom _b[c.y@1.Tx] - _b[c.y@0bn.Tx]

keep y Tx confvar
save "${data_a}/ipw_Ex1a_R.dta", replace


***************************
*add another confounder
***************************

clear 
set obs 2000
set seed 432905
gen y= runiformint(20,40)
gen age= runiformint(20,30)
gen Tx = uniform() < .5
gen confvar= runiformint(1,3)
teffects ipw (y) (Tx confvar age, logit), ate nolog 

*Calculate by hand.
logit Tx confvar c.age
predict pscore
generate ipw=0
replace ipw=1/(1-pscore) if (Tx==0)
replace ipw=1/pscore if (Tx==1)
tab confvar Tx, col
tab confvar Tx [aw=ipw], col 

svyset [pweight=ipw]
svy:mean y,over(Tx) coeflegend
*ATE
lincom _b[c.y@1.Tx] - _b[c.y@0bn.Tx]

keep y Tx confvar age
save "${data_a}/ipw_Ex1b_R.dta", replace







