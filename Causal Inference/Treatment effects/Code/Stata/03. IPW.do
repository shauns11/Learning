****************************
** IPW estimators
****************************

clear all
pwd
display c(current_date)
global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Causal Inference/Treatment effects/"
global data "${projdir}/data"
global outputs "${projdir}/outputs"
set seed 6795
set obs 1000
gen id=_n
gen treat = uniform() < .2
gen male = uniform() < .5
gen outcome= runiformint(1,10)
gen x= runiformint(1,10)
teffects ipw (outcome) (treat male x, logit), ate nolog
teffects ipw (outcome) (treat male x, logit), atet nolog

*Calculate by hand.
logit treat x male
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

di "Finished"





