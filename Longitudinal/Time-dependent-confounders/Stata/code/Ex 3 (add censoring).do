/*
IPTW is routinely used in Marginal Structural Modelling.
MSM are particularly useful when accounting for time-varying confounding, formally defined
as confounding induced by outcome risk factors that are affected by previous treatment and which effect
future treatment.

Treatment (t-1) -> Time-invariant confounder (t) ->  Treatment (t+1)

Censoring:

In this setting, IPTW calculated at multiple time points throughout the follow-up period
are commonly combined with inverse probability of censoring weights to address time-varying confounding
and selection bias introduced by informative censoring in a single model.

(i) Model Tx at time t on (i) covariates at time t and (ii) covariate history.
(ii) Model Cens at time t on covariates at time t-1. 
Censoring precedes outcome at time t. If censoring occurs at t, we do not observe treatment at t.
*/

*******************
*Two examples here.
*pscore1: Num Tx
*pscore2: Denom Tx
*pscore3: Num Cens
*pscore4: Denom Cens
*******************

global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/01. IPTW/"
global data "${projdir}/data"
global outputs "${projdir}/outputs/Temp"
*import excel "${data}/Treatment_cens.xlsx",sheet("Sheet1") firstrow clear
*save "C:\Users\rmjdshc\OneDrive - University College London\Learning\Longitudinal\01. IPTW\data\ipw_example3.dta"



************************************************************
*** Example 1 ************************************************
*** No-one on Tx at baseline; No-one censored at baseline
***********************************************************

clear 
set obs 100
set seed 4990041
gen id=_n
gen sex = uniform() < .5
gen treat0=0
gen treat1=0
gen treat2=0
replace treat1=1 if id==10|id==11|id==21|id==31|id==45|id==46|id==47|id==48
replace treat2=1 if id==32|id==33|id==42|id==52|id==62|id==63|id==64|id==65
gen y0=runiformint(1,10)
gen y1=runiformint(1,10)
gen y2=runiformint(1,10)
gen x0=runiformint(1,10)
gen x1=runiformint(1,10)
gen x2=runiformint(1,10)
gen cens0=0
gen cens1=0
gen cens2=0
replace cens1=1 if id==21|id==31|id==41|id==51|id==61|id==71|id==81|id==91
replace cens2=1 if id==22|id==32|id==42|id==52|id==62|id==72|id==82|id==92

**************************************************************************
*Set missing values for censored cases.
*For a specific time t: censoring=1; time-varying variables at t are therefore missing.
*Censoring is prior to covariate assessment.
*treat: time-invariant: sex x_0; time-variant: x
*cens: time-invariant: sex x_0; time-variant: LagX (cannot use x)
**************************************************************************

*censoring is monotonic.
replace x1=. if cens1==1
replace treat1=. if cens1==1
replace y1=. if cens1==1
replace x2=. if (cens1==1|cens2==1)
replace treat2=. if (cens1==1|cens2==1)
replace y2=. if (cens1==1|cens2==1)

*Records for modelling Tx.
qui:tab1 treat0
scalar a = r(N)
qui:tab1 treat1 if (treat0==0) & inlist(treat1,0,1)
scalar b = r(N)
qui:tab1 treat2 if (treat0==0) & (treat1==0) & inlist(treat2,0,1)
scalar c = r(N)
di a+b+c

*Records for modelling cens.
qui: tab1 cens0
scalar a = r(N)
qui: tab1 cens1 if (cens0==0) & inlist(cens1,0,1)  
scalar b = r(N)
qui: tab1 cens2 if (cens0==0) & (cens1==0) & inlist(cens2,0,1)  
scalar c = r(N)
di a+b+c

*how many records of outcome
qui: summ y0
scalar a = r(N)
qui: summ y1 
scalar b = r(N)
qui: summ y2
scalar c = r(N)
di a+b+c

*Baseline values for use in modelling as time-invariant.
gen x_0 = x0
gen treat_0 = treat0

*data in long form.
reshape long treat x y cens,i(id) j(time)

*generate lagged Tx and lagged covariates
by id (time), sort: generate LagX = x[_n-1]
by id (time), sort: generate LagTx = treat[_n-1]

*replace first lagged value with baseline
by id (time), sort: replace LagX = x_0 if _n==1
by id (time), sort: replace LagTx = treat_0 if _n==1

***************
*** Tx
***************

by id, sort: egen first_Tx = min(cond(treat == 1, time, .))
gen drop_tx = time > first_Tx
*Records for the Tx model.
gen t=0
by id: replace t=1 if (time==0)
by id: replace t=1 if (time==1) & inlist(treat[2],0,1) 
by id: replace t=1 if (time==2) & inlist(treat[3],0,1) 
replace t=. if (drop_tx==1)
tab1 t

*******************************************************************
*model Tx at time t on covariates at time t and covariate history.
*covariates at time t are prior to treatment assignment at time t
*******************************************************************

preserve
summ x x_0 if t==1
restore

*Pooled logistic regression of Tx.
*Numerator = time-invariant.
logit treat sex x_0 if (t==1) 
predict pscore1
gen a = e(sample)                                
qui: predict numTx if e(sample)
qui: replace numTx = 1 if (time > first_Tx)                                                                     
qui: replace numTx = (numTx*treat) + ((1-numTx)*(1-treat)) if a==1                         
sort id time
by id: replace numTx=numTx*numTx[_n-1] if _n!=1
drop a

*Denominator (time-varying plus time-invariant). x at time t; and baseline.
logit treat sex x_0 x if (t==1) 
predict pscore2  
gen a = e(sample)                       
qui: predict denomTx if e(sample)
qui: replace denomTx = 1 if (time > first_Tx)                                  
qui: replace denomTx = (denomTx*treat) + ((1-denomTx)*(1-treat)) if a==1    
sort id time
by id: replace denomTx=denomTx*denomTx[_n-1] if _n!=1
drop a

gen iptw = numTx/denomTx
summ iptw

********************************
***Censoring: all person-months
********************************

by id, sort: egen first_C = min(cond(cens == 1, time, .))
gen drop_c = time > first_C

*Records for the censoring model.
gen c=0
by id: replace c=1 if (time==0)
by id: replace c=1 if (time==1) & (cens[1]==0) & inlist(cens[2],0,1) 
by id: replace c=1 if (time==2) & (cens[2]==0) & inlist(cens[3],0,1) 
replace c=. if (drop_c==1)
tab1 c

*************************************************
*model Cens at time t on covariates at time t-1.
*no predictors at time t.
*************************************************

preserve
summ LagX LagTx if c==1
restore

*Denominator (time-invariant + time-varying).
*but no censoring at time 0.
logit cens sex x_0 LagX if (time==1|time==2)
predict pscore4
gen a = e(sample)
predict pcens if e(sample)
*Step 1.
replace pcens=1-pcens
replace pcens=1 if time==0     /*no-one censored at baseline */
*Step 2.
sort id time
by id: replace pcens=pcens*pcens[_n-1] if a==1
*Step 3.
generate censdenom = pcens

*Numerator (time-invariant + time-varying).
drop pcens a
logit cens sex x_0 if (time==1|time==2)
predict pscore3
gen a = e(sample)
predict pcens if e(sample)
*Step 1.
replace pcens=1-pcens
*Step 2.
replace pcens=1 if time==0  /*no-one censored at baseline */
*Step 3.
sort id time
by id: replace pcens=pcens*pcens[_n-1] if a==1
*Step 4.
generate censnum = pcens
*Step 5.
gen ipcw=(censnum/censdenom)
summ ipcw

*multiply the weights together.
gen ipw=iptw*ipcw

*do not put time-dependent confounder in the model.
glm y treat [pw=ipw], cluster(id)
*Est = -2.499186.

********************************
**** Example 2
*time-dependent confounder = x
********************************

*Github location.
global Git_projdir "https://github.com/shauns11/Learning/raw/refs/heads/main/Longitudinal/Time-dependent-confounders/Stata/"
global Git_data "${Git_projdir}/data/"
use "${Git_data}/ipw_example3.dta", clear

*change 99 in Excel sheet to missing.
mvdecode outcome* dropout* treat* x*,mv(99)

*x is the confounder: so Tx and control groups unbalanced with respect to X.
*Data in wide form.

*Pooled records for Tx.
qui:tab1 treat1
scalar a=r(N)
qui:tab1 treat2 if (treat1==0)
scalar b=r(N)
qui:tab1 treat3 if (treat1==0 & treat2==0)
scalar c=r(N)
di a+b+c

*Pooled records for Censoring.
*Everybody took part at Wave 1 (N=181)
qui:tab1 dropout2
scalar b=r(N)
qui:tab1 dropout3 if (dropout2==0)
scalar c=r(N)
di b+c

reshape long outcome x treat dropout, i(id) j(time)
sort id time      
egen tag = tag(id) 

*wave when Tx=1.
egen first_Tx = min(cond(treat == 1, time, .)), by(id)

*tab this at id level (e.g. 60 Tx=1 at wave 1)
tab1 first_Tx  if tag==1


*iptw
*Denominator (time-varying confounder).

generate dropTx=0
replace dropTx=1 if time > first_Tx

*pooled LR of Tx ~ X at each wave (N=144)
xi: logistic treat x if (time <= first_Tx) | (first_Tx==.)  
predict pscore2
*Step 1.
predict pTx if e(sample)
qui:tab1 pTx if x==0    /*0.51*/
qui:tab1 pTx if x==1    /*0.64*/
*Step 2.
replace pTx=1 if (time > first_Tx)  
*Step 3 (iptw).
replace pTx=pTx*treat + ((1-pTx)*(1-treat)) if dropTx!=1 

*If X=0 & T=0.
di 0.51*0+(1-(0.51))*(1-0)  /*1-p*/
*If X=0 & T=1.
di 0.51*1+(1-(0.51))*(1-1)  /*p*/
*If X=1 & T=0.
di 0.64*0+(1-(0.64))*(1-0)  /*1-p*/ 
*If X=1 & T=1.
di 0.64*1+(1-(0.64))*(1-1)  /*p*/

*Step 4 (cumulative).
sort id time
by id: replace pTx=pTx*pTx[_n-1] if _n!=1
*Step 5.
generate Txdenom=pTx


******************************************
*Numerator (no time-invariant covariates).
******************************************

drop pTx
xi: logistic treat if (time<=first_Tx) | (first_Tx==.)
predict pscore1
predict pTx if e(sample)
*Step 1.
replace pTx=1 if (time>first_Tx)
*Step 2.
replace pTx=pTx*treat + ((1-pTx)*(1-treat)) if dropTx!=1

*T=0.
di 0.58*0+(1-0.58)*(1-0)   /*1-p*/
*T=1.
di 0.58*1+(1-0.58)*(1-1)   /*p*/

*Step 3.
sort id time
by id: replace pTx=pTx*pTx[_n-1] if _n!=1
*Step 4.
generate Txnum=pTx

*stable weight
gen iptw=Txnum/Txdenom

*=============================================.
*Censoring (nobody dropped out at wave 1).
*dropout based on covariates at earlier wave.
*==============================================.

by id, sort: egen first_C = min(cond(dropout == 1, time, .))
gen drop_c = time > first_C

*lagged covariate and treatment history to model censoring at time t.
by id (time), sort: generate LagX = x[_n-1]
by id (time), sort: generate LagTx = treat[_n-1]
order dropout LagX LagTx

*Denominator.
xi: logistic dropout LagX if (time==2|time==3)
predict pscore4
predict pcens if e(sample)
qui:tab pcens if LagX==0   /* 0.1975 */
qui:tab pcens if LagX==1   /* 0.22 */

*Step 1.
replace pcens=1-pcens
replace pcens=1 if time==1

*Step 2.
sort id time
by id: replace pcens=pcens*pcens[_n-1] if _n!=1
*Step 3.
generate censdenom = pcens

*Numerator (no covariates).
drop pcens
xi: logistic dropout if (time==2|time==3)
predict pscore3
predict pcens if e(sample)
qui:tab1 pcens     /*0.21*/
*Step 1.
replace pcens=1-pcens
*Step 2.
replace pcens=1 if time==1
*Step 3.
sort id time
by id: replace pcens=pcens*pcens[_n-1] if _n!=1
*Step 4.
generate censnum = pcens
*Step 5.
gen ipcw=censnum/censdenom

*ipw (Tx*censoring).
gen ipw=(iptw)*(ipcw)

*msm.
glm outcome treat [pw=ipw], family(gaussian) link(identity) cluster(id)
*Est = -.6300941 



















