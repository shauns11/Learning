global progsinstall "xthybrid xtmixed_corr"
        foreach y of global progsinstall  {
                ssc install "`y'"
        }
adopath + "C:/Users/rmjdshc/OneDrive - University College London/Learning/Software/Stata/ado files"
adopath + "C:/Users/rmjdshc/OneDrive - University College London/StataMP17/"
adopath + "C:/Users/rmjdsjmhc/OneDrive - University College London/StataMP18/"


global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/02. Continuous outcomes/"
global data "${projdir}/data"
global outputs "${projdir}/outputs/Temp"

**************************************
*Mixed models for continuous outcomes.
**************************************

*************.
*Example 1.
*************.

clear
input id	wp1	wp2	wm1	wm2
1	494	490	512	525
2	395	397	430	415
3	516	512	520	508
4	434	401	428	444
5	476	470	500	500
6	557	611	600	625
7	413	415	364	460
8	442	431	380	390
9	650	638	658	642
10	433	429	445	432
11	417	420	432	420
12	656	633	626	605
13	267	275	260	227
14	478	492	477	467
15	178	165	259	268
16	423	372	350	370
17	427	421	451	443
end

keep id wm* 
reshape long wm, i(id) j(occasion)
rename occasion time
recode time (1=0) (2=1)
gen x =  runiformint(1,10)

*Random intercept: Fixed effects: interaction (slope/rate-of-change varies by x) 
mixed wm c.x c.time c.x#c.time || id: time, mle  stddeviations  nolog
margins, at(time=(0 1) x=(0 1 2 3 4 5 6))
marginsplot, noci

*Drop the interaction: parallel slopes; random slope for time.
mixed wm c.x c.time  || id: time, mle  stddeviations  nolog
margins, at(time=(0 1) x=(0 1 2 3 4 5 6))
marginsplot, noci

mixed wm c.x c.time  || id:, mle  stddeviations  nolog
margins, at(time=(0 1) x=(0 1 2 3 4 5 6))
marginsplot, noci

*************.
*Example 2.
*************.


*Github location.
global Git_projdir "https://github.com/shauns11/Learning/raw/refs/heads/main/Longitudinal/Continuous%20outcomes"
global Git_data "${Git_projdir}/Data/"
*github.
use "${Git_data}/pefr.dta", clear
*use "${data}/pefr.dta", clear

/*
Random intercept model:
Within-subject dependence is due to between-subject heterogeneity.
Split the error into two uncorrelated components:
1. Specific to each subject: but constant across time: representing individual differences 
due to personal characteristics not included as variables in the model (level 2 error)
2. Specific to each subject and time (level 1 error)
*/

keep id wm* 
reshape long wm, i(id) j(occasion)
summ wm   /* the overall sample mean */
xtset id occasion

*mixed model using xtreg
xtreg wm, mle nolog  

/*
EB prediction for residuals: rely on normality assumption for random intercepts. 
EB prediction performs better with small cluster sizes due to shrinkage. 
The total residual is (random effect) + level-1 residual.
The level-1 residuals represent deviation from person-specific trajectories at each time-point.
*/

mixed wm || id:, mle  stddeviations  nolog
estat icc
*post-estimation
predict yhat, xb
predict b*, reffects 
predict b_SE*, reses
predict e, residuals
label var yhat "Predicted value (pop.average)"
label var b1 "EB random intercept"
label var b_SE1 "SE of random intercept"
label var e "level1 residual"

*Range in the person-specific intercepts:
local u0SD = 107.0464
local b0 = 453.9118
di `b0' - (1.96*`u0SD')
di `b0' + (1.96*`u0SD')

*******************************************
*caterpillar plot of level2-random effects
*******************************************
preserve
collapse (mean) u0 = b1 (mean) u0se = b_SE1,by(id)
sort u0
gen u0rank = _n
serrbar u0 u0se u0rank, scale(1.96) yline(0) xtitle("") ytitle("")
restore

*Estimation of cons/intercept with balanced data.
summ wm   
mixed wm || id:, mle  nolog   /* balanced data */


************************************************
*Fixed versus random effects models
*Random effects models & omitted variable bias
************************************************

/*
Random effects (level-2 residuals) are uncorrelated with the level-1 residuals. 
These account for unobserved heterogeneity between persons.
Between-cluster variability represents cluster-level random (unexplained) processes that affect the outcome.
We may wish to explain between-cluster variability (via level-2 covariates). 
We generalise to the population of clusters. RE allows to generalize the inferences beyond the sample used in the model.

The rationale behind random effects model is that, unlike the fixed effects model,
the variation across entities is assumed to be random and 
uncorrelated with the predictor or independent variables included in the model.

The crucial distinction between fixed and random effects is whether the unobserved
individual effect embodies elements that are correlated with the regressors in the
model (FE yes; RE no), not whether these effects are stochastic or not.
If you have reason to believe that differences across entities have some influence
on your dependent variable then you should use random effects.
Random effects assume that the entity's error term is not correlated with the
predictors which allows for time-invariant variables to play a role as explanatory
variables. In random-effects you need to specify those individual characteristics that may or
may not influence the predictor variables. The problem with this is that some variables may not be available 
therefore leading to omitted variable bias in the model

*Fixed-effects models

Cluster-level covariates cannot be included: no attempt to explain between-cluster variability. 
Inference is limited to the clusters in the sample. 
Some estimators (FE) rely only on the **within-cluster** variability of covariates.
Use fixed-effects (FE) whenever you are **only** interested in analyzing the impact of
variables that vary over time. FE explore the relationship between predictor and outcome variables 
within an entity (person). Each entity has its own individual characteristics that
may or may not influence the predictor variables (e.g gender).

(1) When using FE we assume that something within the individual may impact or bias the
redictor or outcome variables and we need to control for this. This is the rationale behind
the assumption of the **correlation between entity's error term and predictor variables**. 
FE remove the effect of those time-invariant characteristics so we can assess the net effect of
the (within-person) predictors on the outcome variable.

(2) Another important assumption of the FE model is that those time-invariant characteristics
are unique to the individual and should not be correlated with other individual
characteristics. Each entity is different therefore the entity's error term and the constant
(which captures individual characteristics) should not be correlated with the others. If the
error terms are correlated, then FE is not suitable since inferences may not be correct and
you need to model that relationship (probably using random-effects), this is the main
rationale for the Hausman test.

If the unobserved variable does not change over time, then any changes in
the dependent variable must be due to influences other than these fixed characteristics.
FE will not work well with data for which within-cluster variation is minimal or for slow
changing variables over time.

The fixed-effects model controls for all time-invariant differences between the individuals, so the estimated
coefficients of the fixed-effects models cannot be biased because of omitted time-invariant characteristics. 
One side effect of the features of fixed-effects models is that they cannot be used to investigate time-invariant causes of the dependent variables. 
Technically, time-invariant characteristics of the individuals are perfectly collinear with the person [or
entity] dummies. Substantively, fixed-effects models are designed to study the causes of changes 
within a person. A time-invariant characteristic cannot cause such a change, because it is constant for each person.
In the FE model these time-invariant variables are absorbed by the intercept. 
*/

*****************************************
*Variance component models with covariates
*****************************************

*************.
*Example 3.
*************.

*use "${data}/smoking.dta", clear
use "${Git_data}/smoking.dta", clear

*education as a categorical variable.
generate educ=(hsgrad*1) + (somecoll*2) + (collgrad*3)
rename momid id
rename idx birth
sort id birth        /* births occur within mothers */
egen first = tag(id) 
xtset id birth
qui: xtsum birwt smoke black

*births = 8604; mothers = 3978

*level-2 variable (black).
*% of births were born to black mothers (birth as unit of analysis)
tab1 black  /* 617 out of 8604 births */

*% of mothers were black (mother as unit of analysis)
tab1 black if first==1 /* 284 out of 3978 mothers */

*Number of births per mother.
by id: egen n_births = count(birwt)
tab n_births if first==1

******************************
*Mixed/random-effects model
*****************************
/*
Lack of correlation between covariates and level-1 residual: level-1 exogeneity
Lack of correlation between covariates and random intercept: level-2 exogeneity
Differences across level-2 units are assumed to be uncorrelated with the regressors.

Violations = "level-1 endogeneity"; "level-2 endogeneity". 
Consistency of regression coefficients requires lack of correlation between covariates 
and the level-2 random effects / level-1 residuals (requires exogeneity).
*/

*These fit the same model.
qui: xtreg birwt smoke male, mle
mixed birwt smoke male || id:, mle
estat icc

*Perform a joint significance test (kessner2 kessner3) via LR test.
qui: mixed birwt smoke male mage i.educ married black kessner2 kessner3 novisit pretri2 pretri3 || id:, mle
estimates store m1
qui: mixed birwt smoke male mage i.educ married black novisit pretri2 pretri3 || id:, mle
estimates store m2
lrtest m1 m2

*Predicted means and CIs.
qui: mixed birwt i.smoke male mage i.educ married black novisit pretri2 pretri3 || id:, mle
margins i.smoke#i.educ
qui: marginsplot, xdimension(educ)

/*
Between and within-person effects of level-1 covariates:
In the above, smoke is a level-1 variable: beta is -218.0grams. 
For births where the mother smoked, the estimated birth weight is 218g lower than for births where the mother did not smoke, 
holding all other variables constant. This estimate is neither purely a comparison:

Between children of the same mother (smoked during one pregnancy but not the other): within-person effect.
Between children of different mothers (where one mother smoked, but the other did not): between-person effect.

This is neither purely:

A between-mother comparison (because smoking status can change within persons)
A within-mother comparison (because smoking status does not change for all mothers).

In a standard mixed-effects model, the estimator (for a variable that can vary by both level-1 and level-2) 
is a weighted average of the within- and between-estimator.
Implicit assumption is that they are identical. Mixed effects models use both within- and between-person information.
The coefficients in a mixed-effects model include both the within-entity and between-entity effects.
*/

*Model 1.
*mixed model (level-1 covariate)
mixed birwt i.smoke || id:, mle

*Between-person effects
*Purely between-person effect (regression on group means). All covariates must vary within-persons.

*Model 2.
xtreg birwt i.smoke, be
*replicate by hand.
preserve
collapse (mean) birwt smoke,by(id)
regress birwt smoke
restore

*************************************************
*Within-person effects (Fixed-Effects model)
*************************************************

/*
Centre time-varying outcome and covariates around cluster means.
Covariates that do not vary within persons drop-out; this includes the random intercept (time-invariant).
Estimation is based **solely** on within-cluster variability.
All person-specific (level-2) effects are accommodated by the fixed intercepts.
The covariates in the model explain variation in outcome within-persons.

Level-2 covariates - whether observed or unobserved - are implicitly controlled for
because the person is held constant in the comparison (smoke vs not smoke within the same mother)
along with the other level-1 characteristics. Each person serves as their own control.
*/

*Model 3.
*Purely within-person effects.
xtreg birwt i.smoke, fe

************************************************************************
*Summary: a covariate/predictor that varies at both level-1 and level-2
************************************************************************

/*
Standard mixed: neither purely within or between.
Purely between: between-regression on group means.
Purely within: fixed effects.
*/

**************************************************************************
*Level-2 endogeneity and cluster-level confounding
*correlation: level-1 predictors and uj: due to missing level-2 variables
***************************************************************************

/*
For a mixed effects model:

For a level-1 variable (e.g. smoking status within mothers), the between-effect may differ from the within-effect
due to omitted level-2 variables that affect both group means and the random intercept (and hence the outcome).
Residual confounding: **over-estimate of the between-person effect**.
Cluster-level confounding (cluster-level omitted variable bias).
Cluster-level confounding is the correlation between the level-1 covariate (time-varying x) and the random intercept (uj).
The random intercept represents the effects of omitted (time-constant) level-2 covariates.
Violation of the level-2 exogeneity assumption: i.e. the problem of endogeneity.

The **within-estimate** is fine: each participant serves as their own
control: level-2 covariates (whether observed or unobserved) are implicitly controlled for
because the person is held constant in the comparison.

**********************************************************
*Allowing for different within- and between-person effects
*Hausman test and hybrid models
**********************************************************

We can identify the problem of omitted level-2 covariates by comparing BP- and WP-effects.
Cluster-level confounding (level-2 endogeneity problem). 
We can investigate and address level-2 endogeneity of level-1 covariates 
(i.e. correlation between level-1 (time-varying) covariates and the random intercept: uj).

***********************************************
*Hybrid model:estimate BP and WP separately
***********************************************

It is important to include both the cluster mean (BP) and the cluster-mean centred covariate (WP).
It relaxes the assumption that the BP and WP effects are the same for a covariate that varies at both level-1 and level-2.
If they are the same (BP=WP), then we are back to the standard mixed model (neither purely within or between). 

The WP term is uncorrelated with the random intercept (by definition). 
The within-person effects are not susceptible to cluster-level confounding. 
We are concerned with the estimates of BP effects 
(concerned with over-estimation of BP effects (of a level-1 covariate) if omit level-2 variables))

********************************
*Entering the level-1 (WP) variable
*********************************

(1): Centre the level-1 covariate around subject mean: then the coefficient for the cluster-mean is the BP effect.

(2) If not centred, the coefficient for the cluster-mean is the **difference** 
between the BP and WP effects (i.e contextual effect).
*/

*Separate BP and WP effects
by id: egen smokBP = mean(smoke)               /* level-2 (cluster mean)         */
gen smokWP = (smoke) - (smokBP)               /* level-1 (cluster-mean centred) */

xtreg birwt smokBP smokWP, mle
mixed birwt smokBP smokWP || id:, mle
*test null hypothesis that between- and within-effects are the same
lincom smokBP - smokWP                 

**Replicate using xthybrid
xthybrid birwt smoke, clusterid(id) family(gaussian) link(identity)

****Hybrid model**
*Repeat for all level-1 covariates.
*Include the cluster means of all level-1 covariates.
*Achieves consistent estimation of WP effects.
*But level-2 (time-invariant) covariates in the model may still be correlated with 
*the random intercept (due to omitted variables).

egen maleBP = mean(male), by(id)
egen mageBP = mean(mage), by(id)
egen kessner2BP = mean(kessner2), by(id)
egen kessner3BP = mean(kessner3), by(id)
egen novisitBP = mean(novisit), by(id)
egen pretri2BP = mean(pretri2), by(id)
egen pretri3BP = mean(pretri3), by(id)

*Estimation (no centring of level-1 variables). If not centred, 
*the coefficient for the cluster-mean is the **difference** between 
*the between- and within-person effect.

xtreg birwt smoke smokBP male maleBP mage mageBP i.educ married black ///
kessner2 kessner2BP kessner3 kessner3BP novisit novisitBP pretri2 pretri2BP pretri3 pretri3BP,mle
*test the null hypothesis of no difference between the BP and WP effects.
*standard mixed: assumes no difference
*hybrid: allows for differences
testparm smokBP maleBP mageBP kessner2BP kessner3BP novisitBP pretri2BP pretri3BP

************************************************************************
**Problems remain with the hybrid model
************************************************************************

/*
Tries to handle level-2 endogeneity of (time-varying) level-1 covariates 
Coefficients of level-2 (time-invariant) covariates are not consistently estimated.
Cannot handle level-2 endogeneity of level-2 covariates (correlation between level-2 covariates and uj).
It is not straightforward to check for level-1 endogeneity (correlation between covariates: level-1 or level-2 and eij). 
IVs are necessary to correct for level-1 endogeneity.

*************************
*Hausman endogeneity test
*************************

To decide between fixed or random effects you can run a Hausman test where the H0 is that the preferred model 
is random effects vs. fixed effects. It basically tests whether the uj are correlated with the regressors (endogeneity), 
the H0 (random effects) is they are not.

The joint H0 that the regression coefficients of the cluster means are zero.
ie. no difference between the BP- and WP-effects (level-1 variables not centered).

xtreg, mle = random effects (mixed model).
xtreg, re = FGLS.
*/

*Model 1 (fixed-effects): estimate the purely WP.
xtreg birwt smoke male mage i.educ married black kessner2 kessner3 novisit pretri2 pretri3,fe
est store m1

*Model 2: estimate the WP using FGLS.
xtreg birwt smoke male mage i.educ married black kessner2 kessner3 novisit pretri2 pretri3,re
est store m2

hausman m1 m2

/*
Interpretation: 

Reject the null hypothesis = model mis-specification.
BP effects of a level-1 covariate are not equal to WP effects (implicit assumption of mixed model is not correct).
A significant Hausman test is often taken to mean that the mixed model should be abandoned and so use a FE.
However, if level-1 covariates do have equal BP and WP effects then we obtain more precise estimates by 
exploiting both within- and between-person information (mixed rather than FE).
FE estimates are particularly imprecise if the covariates have little within-cluster variation.
If BP and WP differ by a small amount then it may still be advisable to use RE estimator
(some bias but smaller variance) than FE.

Terminology:

Contextual effect (level-1 variable): difference between BP and WP.
Compositional effect: WP effect.

*************************
**FE versus RE
*************************

RE can estimate the effects of level-2 (time-invariant) covariates (in contrast to FE).
However: consistent estimation requires both level-1 and level-2 exogeneity.
FE: controls for clusters: providing only WP effects (the level-1 covariates).
RE: estimating purely WP requires the hybrid approach.
*/

****************************************************
*Assigning values to random effects (intercepts)
****************************************************

*Back to the mixed-effects model:
mixed birwt smoke male mage i.educ married black kessner2 kessner3 novisit pretri2 pretri3 || id:, mle
predict a, reffects
predict resid1, rstandard    /* std level-1 residual */
predict a_se, reses
generate diag_se = sqrt(exp(2*[lns1_1_1]_cons) - a_se^2)
generate resid2 = a/diag_se  /* std level-2 residual */

histogram resid1, normal xtitle("Standardized level-1 residuals")
histogram resid2 if first==1, normal xtitle("Standardized level-2 residuals")

**Robust SEs do not rely on model being correctly specified.
mixed birwt smoke male mage i.educ married black kessner2 kessner3 novisit pretri2 pretri3 || id:, vce(robust) mle

******************************************
*Random-coefficient models
*xtreg can only fit random-intercept models.**
******************************************

******************************************************************************************
*Between-group variance in random slope models.
*The magnitude of between-group variation depends on X.
*Small example.
clear
local cons_var = 0.439
local slope_var = 0.021
local covar = 0.034
local x = 0
local x = 1.5
di `cons_var' + (2*`covar')*`x' + (`slope_var'*`x'^2)
*******************************************************************************************

*******************************
*gcse data: girl as predictor.
*******************************

*use "${data}/gcse.dta", clear
use "${Git_data}/gcse.dta", clear

preserve
*random coefficient for girl (binary x).
*estimate between-group variance when x=0 & x=1.
mixed gcse i.girl || school: girl, mle  covariance(unstructured) 
local cons_var = 14.8531
local slope_var = 0.0853893
local covar = 1.126186
local male = 0
local female = 1
di `cons_var' + (2*`covar')*`male' + (`slope_var'*`male'^2)        /* when binary x=0 */
di `cons_var' + (2*`covar')*`female' + (`slope_var'*`female'^2)    /* when binary x=1 */
restore

*use cross-level interaction terms in the fixed part of the model to see the effect on :
*(i) the estimate of the slope variance (between-group variation)
*(ii) the estimate of the intercept variance  

*test inclusion of random slope + covar.
preserve
mixed gcse i.girl || school:, mle  
est store a
mixed gcse i.girl || school: girl, mle  covariance(unstructured) 
est store b
*lrtest a b
*local a = r(chi2)
*di (0.7931/2)
*di 0.5*chi2tail(1,`a') + 0.5*chi2tail(2,`a')  
restore

*******************************
*gcse data: lrt as predictor.
*******************************

*use "${data}/gcse.dta", clear
use "${Git_data}/gcse.dta", clear
egen first = tag(school)       

*Students are nested within schools. X is lrt; Y is gcse. 
*Make sure that X=0 is a useful reference point (e.g. centre at the mean). 
*Define 0 as the initial time-point in a GCM.

*random-intercept only
mixed gcse lrt || school:, mle  
estat icc
estimates store m1

*random-intercept + random slope
mixed gcse lrt || school: lrt, covariance(unstructured) mle  
estat icc
estimates store m2
* correlation between random intercepts and slopes
estat recovariance, correlation               
*Test the slope variance (naive approach).
*Conservative approach.
lrtest m1 m2

*Construct 95% intervals for the level-2 intercepts and level-2 slopes.
*This is a range within which the random variable is expected to lie:
*(a different concept than the usual interpretation of random samples).

**Range in intercepts for level-2 units**
*95% range in school-intercepts (When x=0):
*LL = pop-average - (1.96*level2_cons)
*UL = pop-average + (1.96*level2_cons)

qui: mixed gcse lrt || school: lrt, covariance(unstructured) mle
local b0 = -0.115
local l2_cons = 3.007
di round(`b0' - (1.96*`l2_cons'),.01)
di round(`b0' + (1.96*`l2_cons'),.01)

**Range in slopes**
*95% range in school-slopes (When x=0):
*LL = pop-average - (1.96*level2_slope)
*UL = pop-average + (1.96*level2_slope)

qui: mixed gcse lrt || school: lrt, covariance(unstructured) mle
local b1 = 0.557
local l2_slope = 0.121
di round(`b1' - (1.96*`l2_slope'),.01)
di round(`b1' + (1.96*`l2_slope'),.01)

**Assigning values to the intercepts and slopes**
predict l2_slope l2_cons, reffects          /* intercept varname last */
predict yhat1, xb                         /* pop.average: does not include the random effects */
predict yhat2, fitted                     /* person-specific: includes the random effects */
predict resid1, rstandard                 /* level-1 residuals */

*Graph the random-effects (level-2: intercepts and slopes).
histogram l2_cons if first==1,normal xtitle(Predicted random intercepts)
histogram l2_slope if first==1,normal xtitle(Predicted random slopes)
*Graph the std level-1 residuals.
histogram resid1,normal xtitle(Predicted level-1 residuals)
drop l2_cons l2_slope yhat1 yhat2 

**Model visualisation**
*Two sets of slopes: 
*random-intercept only 
*random-intercept + random slope.

preserve
*random intercept only
qui: mixed gcse lrt || school:, mle                                      
predict yhat, fitted
sort school lrt
twoway (line yhat lrt,connect(ascending)),xtitle(lrt) ytitle(random-intercept model)
graph save "${outputs}/Figure1.gph", replace
drop yhat

*random intercept and random slope 
qui: mixed gcse lrt || school: lrt, covariance(unstructured) mle          
predict yhat, fitted
sort school lrt
twoway (line yhat lrt,connect(ascending)),xtitle(lrt) ytitle(random-slope model)
graph save "${outputs}/Figure2.gph", replace
drop yhat
graph combine "${outputs}/Figure1.gph" "${outputs}/Figure2.gph", ycommon xcommon
restore
erase "${outputs}/Figure1.gph" 
erase "${outputs}/Figure2.gph"

**cross-level interaction in fixed part of the model**
*Effect of level-1 covariate (lrt) varies by type of level-2 unit (schtype: boys school; girls school; mixed school).
mixed gcse c.lrt i.schgend c.lrt#i.schgend || school: lrt, covariance(unstructured) mle   

**Summary**
*Estimate a random slope only for a level-1 variable (varies within level-2 units): not for a level-2 variable.
*The level-1 variable needs to vary within sufficient level-2 units.
*But a random intercept for a level-2 variable can vary by a level-1 variable 
*(i.e. heteroscedastic random intercepts).

*Convergence ('difficult' option).
mixed gcse c.lrt || school: lrt, covariance(unstructured) mle difficult

***********************************
*Summary of different approaches
***********************************

/*
Random effects models** 
Subject-specific intercepts (uj) as random variables. Unobserved between-subject heterogeneity is 
represented by subject-specific effects (uj) that are randomly varying. 
Random intercepts are (unobserved) random variables that can be viewed as residuals. 
Random intercepts (level-2 residuals) represent effects of omitted time-constant variables.

Fixed effects models** 
Subject-specific intercepts as unknown parameters. Unobserved subject heterogeneity 
is represented by fixed subject-specific effects. Estimate average within-subject relationships 
between time-varying covariates and the outcome: every participant acts as their own control. 
Eliminate subject-level confounding and facilitate causal inference.
Fixed-effect models rely exclusively on within-subject variability. 
Intercepts are model parameters (estimated by including dummy variables for subjects). Intercepts represent effects of omitted time-constant variables.
Fixed Effect models relax the assumption that the covariates are uncorrelated with the subject-specific intercept (assumed by random effects).
i.e. relax the exogeneity assumption.
Fixed effects model: the estimated coefficients represent within-subject effects of the covariates
and are not susceptible to bias due to omitted level-2 covariates.

Marginal (population-average) models** 
Within-subject dependence is modeled by direct specification of the residual covariance structure across occasions. 
Focus on average trends while accounting for longitudinal dependence.
A covariance structure is directly specified for the **total** residuals: 
instead of including random effects in the model that **imply** a certain covariance structure.

Dynamic models** 
Response at a given occasion depends on previous or lagged responses.
*/

*use "${data}/wagepan.dta", clear
use "${Git_data}/wagepan.dta", clear

rename nr id
format lwage %5.3f
keep id lwage black hisp union married exper year educ
reshape wide lwage union married exper,i(id) j(year)
reshape long lwage union married exper,i(id) j(year)
list id year lwage in 1/8
xtset id year

*Outcome is log-wage.
*Three time-scales: 
*(i) experience; 
*(ii) year (number of years since 1980); 
*(iii) education (number of years beyond high-school).

generate educt = (educ-12)
generate yeart = (year-1980)
summ exper educt yeart

*Pooled OLS.
regress lwage black hisp union married exper yeart educt, vce(cluster id)
*faqhow-do-i-interpret-a-regression-model-when-some-variables-are-log-transformed
margins, at(educ==4) atmeans
margins, at(educ==5) atmeans
*Change in Y per 1-unit increase in education
*di exp(.0928443)-1
*.09729087
*By hand:
di exp(2.135004)   /* educ=5 */
di exp(2.04216)   /* educ=4 */
*B = (new-old)/old.
di (8.4570803 - 7.7072389)/7.7072389

**********************
*Correlated residuals
**********************

*The outcome over time is often correlated within subjects even after controlling for covariates.
*within-subject correlation matrix.

*Pooled OLs.
regress lwage black hisp union married exper yeart educt, vce(cluster id)
predict resid, residuals

preserve
keep id resid yeart
reshape wide resid,i(id) j(year)
tabstat resid*,statistics(sd) format(%3.2f)
correlate resid*, wrap
restore
drop resid

/*
Problem of pooled OLS.
Between-subject comparisons are susceptible to omitted-variable bias (unmeasured confounding)
due to time-constant (level-2) variables that are not included in the model.
Whereas, within-subject comparisons are free from such bias because subjects act as their own controls.
*/

*Conventional random-intercept model. 
*For consistent estimation: assumption of exogeneity: 
*ie: no correlation between the covariates in the models and residuals (level-1 or level-2).
mixed lwage black hisp union married exper yeart educt || id:, mle
est store m1
estat icc

*Concern about level-2 coefficients (e.g. overestimation of BP effects).
*Prone to subject-level confounding or bias due to omitted subject-level bias.
*icc=0.46 (within-subject correlation between the residuals).
*46% of the variance in log-wage that is **not explained** by the covariates is due to unobserved time-constant level-2 variables.

xtmixed lwage black hisp union married exper yeart educt || id:, mle
xtmixed_corr

*Random-intercept model: accommodate endogenous (time-varying) covariate.
*Correlation: covariate and random-intercept (level-2 residual).
*But still assume covariate is uncorrelated with level-1 residual.
*We do this by allowing for level-1 covariates to have different BP and WP effects (Hybrid model).

*Subject-means of the time-varying variables.
by id: egen unionBP = mean(union)        /* level-2 (cluster mean)         */
by id: egen marriedBP = mean(married)    /* level-2 (cluster mean)         */
by id: egen experBP = mean(exper)        /* level-2 (cluster mean)         */
*Calculate the WP variables.
generate unionWP = union-unionBP
generate marriedWP = married-marriedBP
generate experWP = exper-experBP

*All level-1 covariates are now treated as endogenous: but level-2 covariates are exogenous.
xtmixed lwage black hisp unionWP marriedWP experWP educt unionBP marriedBP experBP || id:, mle vce(robust)
*test the null hypothesis of no difference between the BP and WP effects.
test (unionWP=unionBP) (marriedWP=marriedBP) (experWP=experBP)
*Endogeneity of level-1 covariates is suggested if the BP and WP differ (assumed the same in random effects).
test (unionWP=unionBP)
test (marriedWP=marriedBP)
test (experWP=experBP)

*Replicate using xthybrid.
xthybrid lwage black hisp union married exper educt, clusterid(id) family(gaussian) link(identity)

/*
The hybrid model.
WP effects are consistently estimated as long as relevant level-1 covariates are not omitted.
This is true whether level-2 covariates are exogenous (do not correlate with residuals) or not (endogenous).
BP effects (level-2 covariates) are inconsistently estimated: even if they are exogenous.
*/

****************************
*Hausman and Taylor (1981)
****************************

/*
Consistent estimation of effects of endogenous time-varying and endogenous time-constant covariates.
We have 4 kinds of covariates:

(i) exogenous time-varying covariates (not correlated with random intercept).
(ii) endogenous time-varying covariates.
(iii) exogenous time-constant covariates (not correlated with random intercept).
(iv) endogenous time-constant covariates.

Necessary condition for identification:
as many exogenous time-varying covariates (1) as endogenous time-constant covariates (2).

(i) union = endogenous time-varying covariates (correlated with random intercept).
(ii) educt = endogenous time-constant covariates (correlated with random intercept).
(iii) married;exper: exogenous time-varying covariates.
(iv) black;hisp: exogenous time-constant covariates.
*/

xthtaylor lwage black hisp union married exper educt, endog(union educt)
*Estimates are highly dependent on **which covariates** are designated as endogenous.

***********
*Summary
***********

/*
A weakness of conventional random-effects models is the possibility of subject-level confounding. Omission of variables.
For level-1 (time-varying) covariates (eg, union), this problem can be eliminated by: 
(1) hybrid model: include subject means of time-varying covariates; or by
(2) fixed-effects approaches.

However, neither gives consistent estimates of coefficients for level-2 covariates 
(even if exogenous: i.e. not correlated with uj). 
The xthtaylor approach can be used to obtain consistent estimators of all parameters if we can correctly classify covariates as exogenous or endogenous.

Fixed effects: WP effects are not susceptible to bias due to omitted level-2 variables (level-2 endogeneity) that affects random-effects models. 
It does require sufficient within-subject variability.
*/

*level-1 covariates are union; married and exper.
xtreg lwage union married exper, fe
est store m1
*Same as WP here.
qui: xthybrid lwage union married exper, clusterid(id) family(gaussian) link(identity)

*Hausman test: Are the BP and WP effects equal (assumed by random effects)?
*No: suggests endogeneity (covariates correlated with uj).
*(i) use hybrid model: but inconsistent level-2 covariates (hence xthtaylor); or
*(ii) use fixed effects models.

*Step 1. Fit random-intercept model.
xtreg lwage black hisp union married exper educt, re
est store a
*Step 2. Fit FE model.
xtreg lwage union married exper, fe
est store b
hausman b a

**********************************
*Random slopes in a mixed model.
**********************************

*no random slope.
qui: mixed lwage black hisp union married exper yeart educt || id:, covariance(unstructured) mle
est store a
*random slope.
mixed lwage black hisp union married exper yeart educt || id: exper, covariance(unstructured) mle
est store b
lrtest a b

*95% range in slopes (When x=0)
*LL = pop-average - (1.96*l2_slope(SD))
*UL = pop-average _ (1.96*l2_slope(SD))
local b1 = 0.0418495
local l2_slope = sqrt(0.0029106)  
display round(`b1' - (1.96*`l2_slope'),.01)
display round(`b1' + (1.96*`l2_slope'),.01)

*****************************
*Missing data and drop-out
*****************************

/*
It is important to get the covariance structure right (in addition to mean structure) when there is missing data. 
ML: consistency (estimates approaching parameter values in large samples) is retained for correctly specified models 
as long as the missing data are missing at random (MAR). 
The Pr of being missing may only depend on the covariates or responses at previous occasions: 
but not on the responses we would have observed had they not been missing (MNAR).

When we say data are MAR, we mean that the missingness is to do with the person but it can be predicted from other information about the person. 
It is not specifically related to the missing information. 
When data are MNAR, the missingness is specifically related to what is missing.

RE: In a random-intercepts model, we have MNAR when missingness depends on the random intercept (uj).
FE: Missing is MAR, because missingness can be viewed as depending on subjects,
and dummy variables for subjects are included as covariates, making missingness covariate-dependent.

*********************************************
*Subject-specific vs marginal approaches
*********************************************

Multilevel (conditional or subject-specific) vs marginal (or population-averaged) models.

Multilevel**: we derive the marginal or population-averaged expectations of the responses 
(averaged over the random effects but conditional on the observed covariates).

Marginal**: we directly specify a model for the marginal expectations and the marginal covariance matrix.

The fixed part of the model is the same: pop-average relationship between covariates and outcome.

Multilevel model: we provide subject-specific relationships (NOT in a marginal model). 
The focus is on modeling such subject-specific relationships and how they vary around the population average 
(as summarised by the covariance matrix of the random effects). The covariance matrix of the total residuals is generally not of interest.

Marginal model: only interested in the (1) marginal relationship; and the (2) marginal residual covariance matrix.

*******************************************
*Covariance structures (marginal models)
*******************************************

The covariance structure of the marginal model.
Different structures for the residual covariance matrix.
The covariance matrix for residuals of the same subject.
It is also the covariance matrix of the responses (given the covariates).
The ordering and the timing and spacing of occasions is relevant when 
considering the covariance structure. Assume exogeneity assumption for total residuals.
It is important to specify a good covariance structure in the presence of missing data.
*/

use "${Git_data}/wagepan.dta", clear
*use "${data}/wagepan.dta", clear

rename nr id
format lwage %5.3f
keep id lwage black hisp union married exper year educ
reshape wide lwage union married exper,i(id) j(year)
reshape long lwage union married exper,i(id) j(year)
*list id year lwage in 1/8
xtset id year
*outcome is log-wage.
*3 time-scales: experience; year (#years since 1980); education (yrs beyond high-school).
generate educt = (educ-12)
generate yeart = (year-1980)
*for use in R.
save "${outputs}/wagepan_reduced.dta", replace


**************************************
**Marginal model 1:
**Unstructured covariance matrix
**************************************

*No assumptions about the total residuals: but constant across subjects.
*xtmixed for marginal models: use the noconstant option (omits the random intercept).

*simpler model.
xtmixed lwage black hisp || id:, nocons residuals(unstructured, t(yeart)) nogroup mle

*complex model.
xtset id yeart
xtgee lwage black hisp union married exper yeart educt, family(gaussian)  corr(unstructured)
estat wcorrelation

xtmixed lwage black hisp union married exper yeart educt || id:, nocons residuals(unstructured, t(yeart)) nogroup mle

*Limitations: cannot be used if timings between points vary between subjects.

********************************************************
**Marginal model 2:
**Random intercept: compound symmetry (exchangeable).
********************************************************

*A marginal model with a random intercept structure.
*ICC = 0.46
xtmixed lwage black hisp union married exper yeart educt || id:, nocons residuals(exchangeable) nogroup mle
*xtmixed_corr

xtgee lwage black hisp union married exper yeart educt, corr(exc) vce(robust)
estat wcorrelation, format(%4.3f)


********************************************************
**Marginal model 3:
**Random intercept and random coefficient
********************************************************

*A marginal model with a random-intercept and random-coefficient structure.

*random coefficient for the linear term.
xtmixed lwage black hisp union married exper yeart educt || id: yeart, cov(unstructured) nofetable nogroup mle
xtmixed_corr

*Other covariance structures for marginal models 
*(e.g. autogressive; exponential; MA; banded and Toeplitz).

******************************************************
*Hybrid specifications and complex marginal models.
******************************************************

******************************************************
*(1) Random effects and correlated level-1 residuals.
******************************************************

*More realistic covariance structures combine (i) a random-intercept model with (ii) a covariance structure for the level-1 residuals. 
*A hybrid specification: random-intercept model + first-order autoregressive correlation structure for level-1 residuals.
*The combination of random intercept and exponential covariance structures is good for non-constant time intervals.
xtmixed lwage black hisp union married exper yeart educt || id:, residuals(ar 1, t(yeart)) nofetable nogroup mle
xtmixed_corr

********************************************************
*(2) Heteroscedastic level-1 residuals over occasions.
*random slope for time.
********************************************************

*Combine a random-coefficient structure with heteroscedastic level-1 residuals.
xtmixed lwage black hisp union married exper yeart educt || id: yeart, cov(unstructured) res(independent, by(yeart)) nofetable nogroup mle
xtmixed_corr

***********************************************
**(3) Heteroscedastic level-1 residuals over groups.
***********************************************

*Allow the level-1 residuals to have different variances for different groups.
*A random-intercept model with level-1 residuals that have different variances for the 3 ethnic groups.

generate ethnic = (black*1) + (hisp*2)
xtmixed lwage i.ethnic union married exper yeart educt || id:, cov(unstructured) res(independent, by(ethnic)) nofetable nogroup mle
xtmixed_corr,at(id=13)  /* white */
xtmixed_corr,at(id=383)  /* black */
xtmixed_corr,at(id=1142)  /* hispanic */

***********************************************
**(4) Different covariance matrix over groups.
***********************************************

xtmixed lwage i.ethnic union married exper yeart educt || id:, nocons residuals(ar 1, t(yeart) by(ethnic)) nofetable nogroup mle


****************************************
*Comparing the fit of marginal models.
*AIC and BIC
****************************************

qui: xtmixed lwage black hisp union married exper yeart educt || id:, nocons residuals(exchangeable) nofetable nogroup mle
est store a
qui: xtmixed lwage black hisp union married exper yeart educt || id: yeart, cov(unstructured) nofetable nogroup mle
est store b
estimates stats a b

/*
For both criteria, the model that has the smallest value is preferred (but the best via AIC may not be the best via BIC). 
BIC selects more parsimonious models than AIC.
COMPARISONS MUST BE BASED ON THE SAME DATA AND THE SAME NUMBER OF OBSERVATIONS.
Be cautious: not designed for clustered data.

**********************************************************************
*Marginal (pop.average) models (GEE) and the fixed part of the model.
**********************************************************************

(1) If mean structure is specified correctly, estimates are consistent even if correlation structure is misspecified. 
If the probability of missing data depends on other responses, correct specification of the 
covariance structure is necessary for consistent estimation of regression parameters.

(2) Model-based SEs rely on correct specification of the covariance structure.

(3) Robust SEs can perform poorly in small samples.

(4a) Include all potentially relevant covariates & interactions when selecting covariance structure.
(4b) Keep the chosen covariance structure; then refine mean structure.

(5) Marginal models are susceptible to the subject-level (level-2) confounding.
Inconsistent estimation of parameters if relevant level-2 covariates are not included.

*****************************
*GEE (marginal/pop.average)
*****************************

-xtgee- produces population averaged effect estimates whereas -mixed- and its related -me- commands produce individual level effect estimates. 
However, for linear models, the population averaged and individual level effects being estimated are actually the same, so it should make little difference. GEE is rarely used for continuous responses. Of the above, GEE cannot implement random-coefficient structure. 
GEE: variances are all constrained to be equal.
*/

xtgee lwage black hisp union married exper yeart educt, corr(ar 1) vce(robust)
estat wcorrelation, format(%4.3f)
xtmixed lwage black hisp union married exper yeart educt || id:,nocons residuals(ar 1, t(yeart)) vce(robust)
xtgee lwage black hisp union married exper yeart educt, corr(exc) vce(robust)
estat wcorrelation, format(%4.3f)

***************************
*** Growth curve models.
***************************

*use "${data}/asian.dta", clear
use "${Git_data}/asian.dta", clear

recode gender (2=1) (1=0), generate(girl)
recode gender (2=0) (1=1), generate(boy)
generate agesq = age*age

/*
Both the shape and degree of variability.
Non-linearity: polynomials. Non-linearity in the fixed-part of the model 
(random slope for the **linear** term). 
There may be insufficient within-subject information to estimate variability in the squared term. 
In general, it is reasonable to allow only the lower-order terms of the polynomial to vary randomly between subjects.
*/

*random-intercept and random slope (age).
mixed weight i.girl c.age c.agesq || id: age, covariance(unstructured) mle
est store a
estat recovariance
scalar var1 = exp(2*[lns1_1_1]_cons)
scalar var2 = exp(2*[lns1_1_2]_cons)
scalar cov = tanh([atr1_1_1_2]_cons)*sqrt(var1*var2)
*Plot the mean trajectory.
local a = _b[_cons]
local b = _b[age]
local c = _b[agesq]
local d = _b[1.girl]
twoway (function Weight=`a' + (`b'*x) + (`c'*x^2),range(0.1 2.6)), xtitle(Age in years) ytitle(weight in kg) yscale(range(0 20)) ylab(0(5)20)

twoway (function Weight=`a' + (`b'*x) + (`c'*x^2) + (`d') ,range(0.1 2.6)), ///
xtitle(Age in years) ytitle(weight in kg) ///
yscale(range(0 20)) ylab(0(5)20)

*slopes and person-specific variability.
twoway (function Weight=`a' + (`b'*x) + (`c'*x^2),range(0.1 2.6) lwidth(medium)) ///
(function lower=`a' + (`b'*x) + (`c'*x^2) - 1.96*sqrt(var1 + 2*cov*x + var2*x^2), ///
range(0.1 2.6) lpattern(dash)) ///
(function upper=`a' + (`b'*x) + (`c'*x^2) + 1.96*sqrt(var1 + 2*cov*x + var2*x^2), ///
range(0.1 2.6) lpattern(dash)), ///
legend(order(1 "Mean" 2 "95% range")) xtitle(Age in years) ytitle(weight in kg) yscale(range(0 20)) ylab(0(5)20)

*Predicting person-specific trajectories (nb: fitted includes random effects).
predict yhat, fitted
twoway (scatter weight age) (line yhat age, sort) if girl==1,by(id, compact legend(off))

*cross-level interaction (sex*age) in the fixed part of the model
*level-1 covariate (age) vary by levels of level-2 covariate (sex)
mixed weight i.girl c.age c.agesq i.girl#c.age || id: age, covariance(unstructured) mle

********************************
*(i) Heteroskedasticity at level-1.
********************************

*Usual assumption of constant variance. 
*Now allow amount of level-1 residual variance to vary by gender.

mixed weight i.girl c.age c.agesq || id: age, covariance(unstructured) mle 
est store a
mixed weight i.girl c.age c.agesq || id: age, covariance(unstructured) mle residuals(independent, by(gender))
est store b
lrtest a b

*Null hypothesis is not on the boundary space. Do not divide by 2. 
*Do not reject null hypothesis: simpler model is ok: we can assume common level-1 variance(homoscedasticity for level-1 residuals).

*************************************
*(ii) Heteroskedasticity at level-2.
*************************************

*Allow the random intercepts and random slopes to vary by gender (4 terms).

*interaction terms (slope by gender).
generate age_boy = (age*boy)
generate age_girl = (age*girl)

mixed weight i.girl c.age c.agesq || id: age, covariance(unstructured) mle 
est store a
mixed weight i.girl c.age c.agesq ///
|| id: age_boy boy, noconstant cov(unstructured) ///
|| id: age_girl girl, noconstant cov(unstructured) mle
est store b
lrtest a b

*Not on the boundary space

****************************************
**Non-linearity: piecewise linear
****************************************

*Straight-line segments between the knots. Knots at ages 0.4; 0.9 and 1.5.
*Facilitate graphs by predictions at the specified ages.

drop yhat
set obs 204
replace age=0.4 in 199
replace age=0.4 in 200
replace age=0.9 in 201
replace age=0.9 in 202
replace age=1.5 in 203
replace age=1.5 in 204
replace girl=0 in 199/204
mkspline ages1 0.4 ages2 0.9 ages3 1.5 ages4 = age
mixed weight girl ages1 ages2 ages3 ages4 ||id: age, cov(unstructured) mle
 
*predictions for boys (use -xb- not -fitted-: mean trajectory, not person-specific)
predict yhat, xb
*predict yhat2, fitted
*list yhat in 199/204

estat recovariance
scalar var1 = exp(2*[lns1_1_1]_cons)
scalar var2 = exp(2*[lns1_1_2]_cons)
scalar cov = tanh([atr1_1_1_2]_cons)*sqrt(var1*var2)
generate lower = yhat - (1.96*sqrt(var1 + 2*cov*age + var2*age^2))
generate upper = yhat + (1.96*sqrt(var1 + 2*cov*age + var2*age^2))

twoway (line yhat age, sort lwidth(medium)) ///
(line lower age, sort lpattern(dash)) ///
(line upper age, sort lpattern(dash)) if girl==0, ///
legend(order(1 "Mean" 2 "95% range")) xtitle(Age in years) ///
ytitle(weight in kg) yscale(range(0 20)) ylab(0(5)20) xtick(0.4 0.9 1.5, grid)

***********
*Appendix. 
***********

*Estimation of b0 with unbalanced data.
*The estimate in the null model is a weighted mean of the cluster means.

clear
input id wm1 wm2
1	512	.
2	430	.
3	520	.
4	428	.
5	500	.
6	600	625
7	364	460
8	380	390
9	658	642
10	445	432
11	432	420
12	626	605
13	260	227
14	477	467
15	259	268
16	350	370
17	451	443
end

egen mean = rowmean(wm1 wm2)
egen n = rownonmiss(wm1 wm2)
qui: generate weight = 1/((11435.68)+(524.6418/n))
qui: generate num = (weight * mean)
qui: summ num
di r(sum)
qui: summ weight
di r(sum)
display "Beta in unbalanced case is " (0.65507614)/(0.00144387)

qui: reshape long wm, i(id) j(occasion)
*unbalanced data
mixed wm || id:, mle  nolog   

*****************************************************************************************
*The standard linear growth-curve is not identified with two balanced occasions.
*****************************************************************************************

clear
input id wm1 wm2
1	512	400
2	430	401
3	520	402
4	428	403
5	500	404
6	600	625
7	364	460
8	380	390
9	658	642
10	445	432
11	432	420
12	626	605
13	260	227
14	477	467
15	259	268
16	350	370
17	451	443
end

generate change=wm2-wm1
regress change

qui: reshape long wm, i(id) j(occasion)
mixed wm c.occasion || id: occasion,cov(unstr) mle 
mixed wm c.occasion || id:,cov(unstr) mle 


****************************************************************
*Appendix: different choices of time and origin, including:
*wave of assessment
*time in study
*chronological age
*time before/after an event
*****************************************************************

*Time-in-study.
clear
set obs 1000
set seed 55899065
gen id=_n
gen y1=runiformint(1,10)
gen y2=runiformint(1,10)
gen y3=runiformint(1,10)
gen y4=runiformint(1,10)
gen y5=runiformint(1,10)
gen y6=runiformint(1,10)

gen phdate1=mdy(1,15,2002)
gen phdate2=mdy(1,15,2003)
gen phdate3=mdy(1,15,2004)
gen phdate4=mdy(1,15,2005)
gen phdate5=mdy(1,15,2006)
gen phdate6=mdy(1,15,2007)

gen start_date = mdy(1,15,2002)

gen age1=runiformint(45,64)
gen age2 = age1 + runiformint(1,2)
gen age3 = age2 + runiformint(1,2)
gen age4 = age3 + runiformint(1,2)
gen age5 = age4 + runiformint(1,2)
gen age6 = age5 + runiformint(1,2)
summ age1

gen baseage= (age1 - 54.55)   /*age-at-baseline (centered) */
gen cohort=age1               /* age-at-baseline */
save "${outputs}/Time_in-study.dta", replace

use "${outputs}/Time_in-study.dta", clear


*Time-in-study
preserve
reshape long y, i(id) j(time)
sort id time
tab1 time
*Time-in-study=0 at 1st wave
replace time = time-1
mixed y time || id:,mle cov(unstr) 
*intercept = mean y at time=0.
est store a
*mixed y time || id: time,mle cov(unstr) 
*est store b
*lrtest a b
*local a = r(chi2)
*di 0.5*chi2tail(1,`a') + 0.5*chi2tail(2,`a')  
restore


*Time-as-observation time
preserve
reshape long phdate y, i(id) j(time)
*obs_time=0 at 1st wave
gen date = (phdate-start_date)/365.25
summ date if time==1
mixed y date || id:,mle cov(unstr) 
restore

*Time-as-chronological age
*Getting older: intercept = baseline age 
preserve
reshape long age y, i(id) j(time)
gen age55 = (age-54.559)
summ age55 if time==1
*0 = mean age at 1st wave
mixed y age55 || id:,mle cov(unstr) 
restore

*********************************
*age-cohort effects
*********************************

/*
Using chronological age measures the effect of age
We can separate the effects of "being old" at study entry (cohort) from the effects of "getting old"
adjust for cohort-age IN a particular period (e.g. at baseline)
Interpret the cohort coefficient as increase in Y AT BASELINE 
for every 1-yr increase in age-at-baseline (i.e. cohort)
*/

*age-cohort effects
preserve
reshape long age y, i(id) j(time)
gen age55 = (age-54.559)
mixed y age55 cohort || id:,mle cov(unstr) 
*trajectories for 2 people: 5 years apart at baseline. hold time-varying age constant.
*age 54 (baseline: cohort=0)
gen b54 = _b[_cons] + age55*_b[age55] + 0*_b[cohort]
gen b59 = _b[_cons] + age55*_b[age55] + 5*_b[cohort]
label var b54 "age 54 at baseline"
label var b59 "age 59 at baseline"
*5*_b[cohort] = the difference in intercept (for 5 yr increase in age-at-baseline): 
*holding time-varying age constant.
di _b[_cons] + 0*_b[age55] + 0*_b[cohort]
di _b[_cons] + 0*_b[age55] + 5*_b[cohort]
twoway (line b54 age55) (line b59 age55), xtitle("Chronological age")
restore

*********************************************************
*age-period model
*time as chronological age: controlling for period effects
**********************************************************

*both are time-varying: increase in time and increase in age not necessarily the same.

reshape long age y, i(id) j(time)
gen age55 = (age-54.559)
replace time = time-1
mixed y age55 time || id:,mle cov(unstr) 

*so at each value of age: compare the weights across the different time points
*weight decreases (linearly) from t=1 to t=10

gen b0 = _b[_cons] + age55*_b[age55]
gen b1 = _b[_cons] + age55*_b[age55] + 1*_b[time]
gen b2 = _b[_cons] + age55*_b[age55] + 2*_b[time]
gen b3 = _b[_cons] + age55*_b[age55] + 3*_b[time]
gen b4 = _b[_cons] + age55*_b[age55] + 4*_b[time]
gen b5 = _b[_cons] + age55*_b[age55] + 5*_b[time]
label var b0 "baseline"
label var b1 "wave 2"
label var b2 "wave 3"
label var b3 "wave 4"
label var b4 "wave 5"
label var b5 "wave 6"

twoway (line b0 age55) (line b1 age55) (line b2 age55) (line b3 age55) ///
(line b4 age55) (line b5 age55), xtitle("Chronological age")

*y increases by 0.018 for every 1 unit increase in time (e.g. comparing same person at t=1 and t=2)
*y varies with age for each value of time (e.g. at t=2: y varies by chronological age)
*linear association between increase in time-in-study and y.

********************************************
*include time as a categorical variable
*****************************************

mixed y age55 i.time || id:,mle cov(unstr) 

**********************************************
*rate of change vary by age at baseline
**********************************************

mixed y c.baseage c.time c.baseage#c.time || id:,mle cov(unstr) 

di "Mixed models for continuous outcomes finished"

*********************
*FINISHED.
*********************



















 

