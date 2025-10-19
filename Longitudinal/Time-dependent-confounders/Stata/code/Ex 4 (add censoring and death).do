*****************************************************
******Separate models for Tx, Cens and mortality
*****************************************************

clear

global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/01. IPTW"
global data "${projdir}/data"
global outputs "${projdir}/outputs/Temp"

*N:Drive.
global projdir_a "N:/Learning/Longitudinal/01. IPTW/"
global data_a "${projdir_a}/data"
global outputs_a "${projdir_a}/outputs/Temp"


*import excel "${data}\Treatment cens and death.xlsx",sheet("Data") firstrow clear 
*save "C:\Users\rmjdshc\OneDrive - University College London\Learning\Longitudinal\01. IPTW\data\ipw_example4.dta"
use "${data_a}/ipw_example4.dta", clear

*Github location.
*global Git_projdir "https://github.com/shauns11/Learning/raw/refs/heads/main/Longitudinal/Time-dependent-confounders/Stata/"
*global Git_data "${Git_projdir}/data/"
*use "${Git_data}/ipw_example4.dta", clear

*data is in wide form

*change 99 to missing.
mvdecode start* time* death* cens* treat* y* cov*,mv(99)

*all data is missing after death
*once censored, do not observe treatment
preserve
keep if cens0==1
tab1 treat1 treat2
restore

preserve
keep if cens1==1
tab1 treat2
restore

*Number of records to model Tx (n=268)
qui:tab1 treat0
scalar a = r(N)
qui:tab1 treat1 if (treat0==0)
scalar b = r(N)
qui:tab1 treat2 if (treat0==0 & treat1==0)
scalar c = r(N)
di a+b+c

*Number of records for model Cx (n=410)
qui:tab1 cens0
scalar a = r(N)
qui:tab1 cens1 if (cens0==0)
scalar b = r(N)
qui:tab1 cens2 if (cens0==0 & cens1==0)
scalar c = r(N)
di a+b+c

*Number of records for model death (n=524)
qui:tab1 death0
scalar a = r(N)
qui:tab1 death1 if (death0==0)
scalar b = r(N)
qui:tab1 death2 if (death0==0 & death1==0)
scalar c = r(N)
di a+b+c

reshape long start time treat cov cens death y, i(id) j(occ)
keep id occ treat cens death start cov sex y
*N=200 (3 records each).
*https://www.stata.com/support/faqs/data-management/first-and-last-occurrences/

by id, sort: egen first_Tx = min(cond(treat == 1, occ, .))
gen first_tx = occ == first_Tx
gen drop_tx = occ > first_Tx

by id, sort: egen whenC = min(cond(cens == 1, occ, .))
gen first_c = occ == whenC
gen drop_c = occ > whenC

by id, sort: egen whenD = min(cond(death == 1, occ, .))
gen first_d = occ == whenD
gen drop_d = occ > whenD


*N=534 with Tx weights (268 model; 266 with PTx=1)
count if inlist(treat,0,1)|(drop_tx==1)
*N=524 with cens weights (410 model; 114 with PCx=1)
count if inlist(cens,0,1)|(drop_c==1)
*N=600 with death weights (524 model; 76 with Pdx=1)
count if inlist(death,0,1)|(drop_d==1)


*Records for the Tx model.
gen t=0
by id: replace t=1 if (occ==0)
by id: replace t=1 if (occ==1) & (treat[1]==0) & inlist(treat[2],0,1) 
by id: replace t=1 if (occ==2) & (treat[1]==0) & (treat[2]==0) & inlist(treat[3],0,1) 
replace t=. if (drop_tx==1)
tab1 t

*Records for the censoring model.
gen c=0
by id: replace c=1 if (occ==0)
by id: replace c=1 if (occ==1) & (cens[1]==0) & inlist(cens[2],0,1) 
by id: replace c=1 if (occ==2) & (cens[1]==0) & (cens[2]==0) & inlist(cens[3],0,1) 
replace c=. if (drop_c==1)
tab1 c

*Records for the death model.
gen d=0
by id: replace d=1 if (occ==0)
by id: replace d=1 if (occ==1) & (death[1]==0) & inlist(death[2],0,1) 
by id: replace d=1 if (occ==2) & (death[1]==0) & (death[2]==0) & inlist(death[3],0,1) 
replace d=. if (drop_d==1)
tab1 d

***********************
*Censoring.
***********************

*Numerator (time-constant variables)
logit cens i.sex if (c==1)  
predict pscore1
gen a = e(sample)
predict num if e(sample)
replace num=1 if occ > whenC   
replace num = (num*cens) + ((1-num)*(1-cens)) if a==1                         
sort id occ
by id: replace num=num*num[_n-1] if _n!=1
drop a

*Denominator (time-invariant + time-varying)
*lagged values of time-varying confounder
logit cens i.sex c.cov if (c==1) 
predict pscore2
gen a = e(sample)                    
predict denom if e(sample)
replace denom=1 if occ > whenC                                    
replace denom = (denom*cens) + ((1-denom)*(1-cens)) if a==1 

sort id occ
by id: replace denom=denom*denom[_n-1] if _n!=1
gen ipcw = num/denom
drop num denom a


**********************************
*Tx.
**********************************

*Numerator.
logit treat i.sex if (t==1) 
predict pscore3
gen a = e(sample)  
predict num if e(sample)
replace num=1 if occ > first_Tx                                                                     
replace num = (num*treat) + ((1-num)*(1-treat)) if a==1                         
sort id occ
by id: replace num=num*num[_n-1] if _n!=1
drop a

*Denominator.
logit treat i.sex c.cov  if (t==1) 
predict pscore4
gen a = e(sample)                          
predict denom if e(sample)
replace denom=1 if occ > first_Tx                                 
replace denom = (denom*treat) + ((1-denom)*(1-treat)) if a==1 
sort id occ
by id: replace denom=denom*denom[_n-1] if _n!=1
gen iptw = num/denom
summ iptw
drop num denom a

**********************
*death.
***********************

*Numerator
logit death i.sex if (d==1) 
predict pscore5
gen a = e(sample)                                
predict num if e(sample)
replace num=1 if occ > whenD                                                                     
replace num = (num*death) + ((1-num)*(1-death)) if a==1                        
sort id occ
by id: replace num=num*num[_n-1] if _n!=1
drop a

*Denominator
logit death i.sex c.cov  if (d==1) 
predict pscore6
gen a = e(sample)                          
predict denom if e(sample)
replace denom=1 if occ > whenD                                  
replace denom = (denom*death) + ((1-denom)*(1-death))  if a==1                            

sort id occ
by id: replace denom=denom*denom[_n-1] if _n!=1
gen ipdw = num/denom
summ ipdw
drop num denom a

gen ipw=iptw*ipcw*ipdw
*glm y treat sex cov [pw=ipw], cluster(id)
*N=410 (y observed for those who were not censored).

glm y treat [pw=ipw], cluster(id)
*treat = .2944133 


keep id start occ treat y sex cov cens drop_tx drop_c drop_d death
save "${data_a}/ipw_Ex4_R.dta", replace











