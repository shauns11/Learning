**********************************************************
***Stabilised IPTW & MSM.
***Time-varying exposure/treat and time-varying confounders
** Two examples.
**********************************************************

*See Excel: 
*"C:\Users\rmjdshc\OneDrive - University College London\Learning\Causal Inference\IPTW\code\Stata\Stabilised weights and MSM\03 Time-varying treatment.xlsx

global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/01. IPTW/"
global data "${projdir}/data"
global outputs "${projdir}/outputs/Temp"





****************************************************************************************************
*Example 1.
*Time-invariant: sex.
*Time-varying: x.
*n=20; with two waves: so 40 values on the outcome variable (no censoring)
*but only 34 records used to model exposure: probability set to 1 after first exposure
***************************************************************************************************.

*n=20
*Tx=1 (6 at baseline).
*Of n=14 not on Tx at baseline, 2 move to Tx at t=2.
*Pooled records for modelling Pr(Tx)=20+14=34.
*2 people with Pr(Tx)=1 carried over.

clear
input id treat0 treat1 sex
1 0 1 1
2 0 0 2
3 0 0 1
4 0 0 2
5 0 0 1
6 0 0 2
7 0 0 1
8 1 1 2
9 1 1 1
10 1 0 2
11 0 1 1
12 0 0 2
13 0 0 1
14 0 0 2
15 0 0 1
16 0 0 2
17 0 0 1
18 1 1 2
19 1 1 1
20 1 0 2
end
set seed 499004

gen outcome0= runiformint(1,10)
gen outcome1= runiformint(1,10)
gen x0= runiformint(1,10)
gen x1= runiformint(1,10)
reshape long outcome treat x,i(id) j(time)

tab id if (treat==1 & time==0)

*when Tx first occurs (variable only recorded for those on Tx at any time: missing when treat==0)
by id, sort: egen first_Tx = min(cond(treat==1, time, .))
gen first_tx = time==first_Tx
gen drop_tx = time > first_Tx
count
label variable first_Tx "For all rows: shows time when exposure first occurs"
label variable first_tx "binary: 1 if time = time when exposure first occurs"
label variable drop_tx "binary: 1 if time = after the time when exposure first occurs"

*Count records for the pooled Tx model.
gen t=0
by id: replace t=1 if (time==0) & inlist(treat[1],0,1) 
by id: replace t=1 if (time==1) & (treat[1]==0) & inlist(treat[2],0,1) 
replace t=. if (drop_tx==1)
tab1 t

***********************************
*Pooled logistic regression of Tx.
***********************************

*Numerator = time-constant (model treatment on baseline covariates).
logit treat sex if (t==1) 
predict pscore1
gen a = e(sample)                                
qui: predict numTx if e(sample)
qui: replace numTx = 1 if (time > first_Tx)                                                                     
qui: replace numTx = (numTx*treat) + ((1-numTx)*(1-treat)) if a==1                         
sort id time
by id: replace numTx=numTx*numTx[_n-1] if _n!=1
drop a

*Denominator (time-varying plus time-constant).
logit treat sex x if (t==1)   
predict pscore2
gen a = e(sample)                       
qui: predict denomTx if e(sample)
qui: replace denomTx = 1 if (time > first_Tx)                                  
qui: replace denomTx = (denomTx*treat) + ((1-denomTx)*(1-treat)) if a==1    
sort id time
by id: replace denomTx=denomTx*denomTx[_n-1] if _n!=1

gen iptw = numTx/denomTx
summ iptw

*********************************************************
*Do not adjust for time-varying confounders in the msm.
*But I can use time-varying time.
*********************************************************

regress outcome treat time sex [pw=iptw], cluster(id)
*ATE=-1.904188


***********************************
*Example 2.
*Numerator: Time-invariant: male + age.
*Denominator: Time-variant: male + age + v.
***********************************

*Github location.
global Git_projdir "https://github.com/shauns11/Learning/raw/refs/heads/main/Longitudinal/Time-dependent-confounders/Stata/"
global Git_data "${Git_projdir}/data/"
use "${Git_data}/ipw_example2.dta", clear

*Data in long form; outcome is y; treat is exposure.
*time-varying confounder is v.
*time-invariant: age and sex.

*Model for Tx.
*first occasion of treatment.
by id, sort: egen first_Tx = min(cond(treat == 1, time, .))
gen first_tx = time == first_Tx
gen drop_tx = time > first_Tx
tab1 drop_tx
label variable first_Tx "For all rows: shows time when exposure first occurs"
label variable first_tx "binary: 1 if time = time when exposure first occurs"
label variable drop_tx "binary: 1 if time = after the time when exposure first occurs"

*Records for the Tx model.
gen t=0
by id: replace t=1 if (time==1) & inlist(treat[1],0,1) 
by id: replace t=1 if (time==2) & (treat[1]==0) & inlist(treat[2],0,1) 
by id: replace t=1 if (time==3) & (treat[1]==0) & (treat[2]==0) & inlist(treat[3],0,1) 
replace t=. if (drop_tx==1)
tab1 t

*Pooled logistic regression of Tx.
*Numerator (time-invariant -> treatment).
logit treat male age if (t==1)  
predict pscore1
gen a = e(sample)                               
predict num if e(sample)
replace num=1 if time > first_Tx                                                                     
replace num = (num*treat) + ((1-num)*(1-treat)) if a==1                         
sort id time
by id: replace num=num*num[_n-1] if _n!=1
drop a

*Denominator (time-varying plus time-invariant -> treatment).
logit treat v male age  if (t==1)  
predict pscore2
gen a = e(sample)                       
predict denom if e(sample)
replace denom=1 if time > first_Tx                                  
replace denom = (denom*treat) + ((1-denom)*(1-treat)) if a==1    
sort id time
by id: replace denom=denom*denom[_n-1] if _n!=1

gen iptw = num/denom
summ iptw

*xtgee y treat time male age [pw=ipw], family(gaussian) corr(ar1)

*do not include the time-varying confounder (v)
regress y treat time male age [pw=iptw], cluster(id)
*Est = -.0703493



