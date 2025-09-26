global progsinstall "xtrccipw" 
        foreach y of global progsinstall  {
                capture ssc install "`y'"
        }
	
adopath + "C:/Users/rmjdshc/OneDrive - University College London/Learning/Software/Stata/ado files"
adopath + "C:/Users/rmjdshc/OneDrive - University College London/StataMP17/"
adopath + "C:/Users/rmjdsjmhc/OneDrive - University College London/StataMP18/"


*Dropout is monotonic.
*Model dropout using lagged y and specified X.

clear
global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/06. Daza xtrccipw/"
global data "${projdir}/data"
global outputs "${projdir}/outputs/Temp"
*use "${data}/nlswork5_xtrccipw.dta", clear

*Github location.
global Git_projdir "https://github.com/shauns11/Learning/raw/refs/heads/main/Longitudinal/IPW/"
global Git_data "${Git_projdir}/Data/"

*github.
use "${Git_data}/nlswork5_xtrccipw.dta", clear

*Example 1:
*Excludes truncyear: 
*truncation being treated like dropout. 
*xtrccipw i.union, idvar(idcode) timevar(year) timeidxvar(yearidx) generate(ipw_full) ///
*linkfxn(logit) tdindepvars(ttl_exp) tiindepvars(i.collgrad) glmvars(age ln_wage birth_yr) glmfamily(binomial) 

*Example 2:
*xtrccipw i.union, idvar(idcode) timevar(year) timeidxvar(yearidx) generate(ipw_full) trtimevar(truncyear) linkfxn(logit) tdindepvars(ttl_exp) tiindepvars(i.collgrad) ///
*glmvars(age ln_wage birth_yr) glmfamily(binomial)
*Truncation can be considered as death.
*Those TRUNCATED are excluded from the model of drop-out
*model drop-out conditional on not being truncated.

*--------------------------------------.
*Example 2: truncation separately.
*---------------------------------------.

* 670.

count if inlist(union,0,1) & year==70
count if inlist(union,0,1) & year==71
count if inlist(union,0,1) & year==72
count if inlist(union,0,1) & year==73
count if inlist(union,0,1) & year==77
count if inlist(union,0,1) & year==78
count if inlist(union,0,1) & year==80

di 205+121+105+71+67+53+48

version 15
xtrccipw i.union, idvar(idcode) timevar(year) timeidxvar(yearidx) generate(ipw_full) trtimevar(truncyear) linkfxn(logit) tdindepvars(ttl_exp) tiindepvars(i.collgrad) ///
glmvars(age ln_wage birth_yr) glmfamily(binomial)
 
*replicate by hand.
* truncyear appears on all rows (i.e. is time-constant).

tab truncyear

*lagged outcome.
sort idcode year
by idcode: generate union1 = union[1]
by idcode: generate union2 = union[2]
by idcode: generate union3 = union[3]
by idcode: generate union4 = union[4]
by idcode: generate union5 = union[5]
by idcode: generate union6 = union[6]
by idcode: generate union7 = union[7]

label define censlbl 1 "not censored" 2 "censored" 3 "truncated"

*N=357 (118 censored: 80 truncated; 159 not censored).
preserve
keep if yearidx==1
gen outcome1=0
replace outcome1=1 if inlist(union1,0,1) & !inlist(truncyear,71)
replace outcome1=2 if union1==. & !inlist(truncyear,71)
replace outcome1=3 if (truncyear==71)
label values outcome1 censlbl
tab1 outcome1
restore

*N=159 (37 censored: 11 truncated; 111 not censored).
preserve
keep if yearidx==2
gen outcome2=.
replace outcome2=1 if inlist(union2,0,1) & !inlist(truncyear,71,72)
replace outcome2=2 if inlist(union1,0,1) & (union2==.) & !inlist(truncyear,71,72)
replace outcome2=3 if (truncyear==72)
label values outcome2 censlbl
tab1 outcome2
restore

*N=111 (6 censored: 16 truncated; 89 not censored).
preserve
keep if yearidx==3
gen outcome3=.
replace outcome3=1 if inlist(union3,0,1) & !inlist(truncyear,71,72,73)
replace outcome3=2 if inlist(union1,0,1) & inlist(union2,0,1) & (union3==.) & !inlist(truncyear,71,72,73)
replace outcome3=3 if (truncyear==73)
label values outcome3 censlbl
tab1 outcome3
restore

*N=89 (18 censored: 4 truncated; 67 not censored).
preserve
keep if yearidx==4
gen outcome4=.
replace outcome4=1 if inlist(union4,0,1) & !inlist(truncyear,71,72,73,77)
replace outcome4=2 if inlist(union1,0,1) & inlist(union2,0,1) & inlist(union3,0,1) & (union4==.) & !inlist(truncyear,71,72,73,77)
replace outcome4=3 if (truncyear==77)
label values outcome4 censlbl
tab1 outcome4
restore

*N=67 (0 censored: 8 truncated; 59 not censored).
preserve
keep if yearidx==5
gen outcome5=.
replace outcome5=1 if inlist(union5,0,1) & !inlist(truncyear,71,72,73,77,78)
replace outcome5=2 if inlist(union1,0,1) & inlist(union2,0,1) & inlist(union3,0,1) & inlist(union4,0,1) & (union5==.) & !inlist(truncyear,71,72,73,77,78)
replace outcome5=3 if (truncyear==78)
label values outcome5 censlbl
tab1 outcome5
tab1 union6 if outcome5==1, missing
restore

*N=59 (6 censored: 2 truncated; 51 not censored).
preserve
keep if yearidx==6
gen outcome6=.
replace outcome6=1 if inlist(union6,0,1) & !inlist(truncyear,71,72,73,77,78,80)
replace outcome6=2 if inlist(union1,0,1) & inlist(union2,0,1) & inlist(union3,0,1) & inlist(union4,0,1) & inlist(union5,0,1) & (union6==.) & !inlist(truncyear,71,72,73,77,78,80)
replace outcome6=3 if (truncyear==80)
label values outcome6 censlbl
tab1 outcome6
restore

*N=51 (3 censored: 2 truncated; 48 not censored).
preserve
keep if yearidx==7
gen outcome7=.
replace outcome7=1 if inlist(union7,0,1) & !inlist(truncyear,71,72,73,77,78,80)
replace outcome7=2 if inlist(union1,0,1) & inlist(union2,0,1) & inlist(union3,0,1) & inlist(union4,0,1) & inlist(union5,0,1) & inlist(union6,0,1)  & (union7==.) & !inlist(truncyear,71,72,73,77,78,80)
replace outcome7=3 if (truncyear==80)
label values outcome7 censlbl
tab1 outcome7
restore

*===================================================.
*Wave 1.
*N=357 (118 censored: 80 truncated; 159 not censored).
*N(dropout)=277
*=====================================================.

gen dropw1=1
replace dropw1=0 if (union1==. & yearidx==1)
tab dropw1 if year==70

*Model drop out.

logit dropw1 c.ttl_exp i.collgrad if yearidx==1                 /* 1 = NOT censored */
predict cens
generate w1wgt=1/cens
generate check_ipw=.
by idcode: replace check_ipw=w1wgt[1] if yearidx==1
summ check_ipw ipw_full if yearidx==1


*===================================================.
*Wave 2.
*N=159 (37 censored: 11 truncated; 111 not censored).
*===================================================.

*Drop-out at wave 2.

gen dropw2=1
replace dropw2=0 if (union2==. & yearidx==2)
tab dropw2 if yearidx==2

*Model drop out.

drop cens
logit dropw2 union1 c.ttl_exp i.collgrad if (yearidx==2   & !inlist(truncyear,71))              /* 1 = NOT censored */
predict cens
generate w2wgt=1/cens

by idcode: replace check_ipw=(w1wgt[1] * w2wgt[2]) if yearidx==2 & !inlist(truncyear,71)        
list check_ipw ipw_full if yearidx==2 in 1/40
summ check_ipw ipw_full if yearidx==2

*=============.
* Wave 3.
*N=111 (6 censored: 16 truncated; 89 not censored).
*================

gen dropw3=1
replace dropw3=0 if (union3==. & yearidx==3)
tab dropw3 if yearidx==3

*Model drop out.

drop cens
logit dropw3 union1 union2 c.ttl_exp i.collgrad if (yearidx==3  & !inlist(truncyear,71,72))                 /* 1 = NOT censored */
predict cens
generate w3wgt=1/cens

by idcode: replace check_ipw=(w1wgt[1] * w2wgt[2] * w3wgt[3]) if yearidx==3 & !inlist(truncyear,71,72) 
list check_ipw ipw_full if yearidx==3 in 1/40
summ check_ipw ipw_full if yearidx==3

*========.
* Wave 4.
*N=89 (18 censored: 4 truncated; 67 not censored).
*===============================================.

gen dropw4=1
replace dropw4=0 if (union4==. & yearidx==4)
tab dropw4 if yearidx==4

*Model drop out.

tab regerrorcode_xtrccipwRi if yearidx==4
 
* (71/89): numerator = not censored + truncated / (not censored + truncated + censored)

drop cens
logit dropw4 union1 union2 union3 c.ttl_exp i.collgrad if (yearidx==4   & !inlist(truncyear,71,72,73))               /* 1 = NOT censored */
predict cens
generate w4wgt=1/cens
replace w4wgt = 1 / (71/89) if (yearidx==4 & (regerrorcode_xtrccipwRi==3))    & !inlist(truncyear,71,72,73)        /* 10 cases */
by idcode: replace check_ipw=(w1wgt[1] * w2wgt[2] * w3wgt[3] * w4wgt[4]) if yearidx==4 & !inlist(truncyear,71,72,73) 

preserve
keep if (yearidx==4 & (regerrorcode_xtrccipwRi==0)) 
summ check_ipw ipw_full
restore

preserve
keep if (yearidx==4 & (regerrorcode_xtrccipwRi==3)) 
summ check_ipw ipw_full
restore

*=========.
* Wave 5.
*N=67 (0 censored: 8 truncated; 59 not censored).
*========

gen dropw5=1
replace dropw5=0 if (union5==. & yearidx==5)
tab dropw5 if yearidx==5

*Model drop out.

tab regerrorcode_xtrccipwRi if yearidx==5

drop cens
*logit dropw5 union1 union2 union3 union4 c.ttl_exp i.collgrad if (yearidx==5  & !inlist(truncyear,71,72,73,77))               /* 1 = NOT censored */

generate w5wgt=1/(67/67)
by idcode: replace check_ipw=(w1wgt[1] * w2wgt[2] * w3wgt[3] * w4wgt[4] * w5wgt[5]) if yearidx==5 & !inlist(truncyear,71,72,73,77) 

preserve
keep if (yearidx==5 & (regerrorcode_xtrccipwRi==1)) 
summ check_ipw ipw_full
restore

*========.
*Wave 6.
*N=59 (6 censored: 2 truncated; 51 not censored).
*========

gen dropw6=1
replace dropw6=0 if (union6==. & yearidx==6)
tab dropw6 if yearidx==6

*Model drop out.

tab regerrorcode_xtrccipwRi if yearidx==6

logit dropw6 union1 union2 union3 union4 union5 c.ttl_exp i.collgrad if (yearidx==6  & !inlist(truncyear,71,72,73,77,78))               /* 1 = NOT censored */
predict cens
generate w6wgt=1/cens
replace w6wgt = 1 / (53/59) if (yearidx==6 & (regerrorcode_xtrccipwRi==3))    & !inlist(truncyear,71,72,73,78)        /* 10 cases */
by idcode: replace check_ipw=(w1wgt[1] * w2wgt[2] * w3wgt[3] * w4wgt[4] * w5wgt[5] * w6wgt[6]) if yearidx==6 & !inlist(truncyear,71,72,73,77,78) 

preserve
keep if (yearidx==6 & (regerrorcode_xtrccipwRi==0)) 
summ check_ipw ipw_full
restore

preserve
keep if (yearidx==6 & (regerrorcode_xtrccipwRi==3)) 
summ check_ipw ipw_full
restore

*=======.
*Wave 7.
*N=51 (3 censored: 2 truncated; 48 not censored).
*======

gen dropw7=1
replace dropw7=0 if (union7==. & yearidx==7)
tab dropw7 if yearidx==7

*Model drop out (51 cases).

summ ipw_full if yearidx==6
summ ipw_full if yearidx==7
tab regerrorcode_xtrccipwRi if yearidx==7

drop cens
logit dropw7 union1 union2 union3 union4 union5 union6 c.ttl_exp i.collgrad if (yearidx==7  & !inlist(truncyear,71,72,73,77,78,79))               /* 1 = NOT censored */
predict cens
generate w7wgt=1/cens
replace w7wgt = 1 / (48/51) if (yearidx==7 & (regerrorcode_xtrccipwRi==3))    & !inlist(truncyear,71,72,73,78,79,80)        /* 10 cases */
by idcode: replace check_ipw=(w1wgt[1] * w2wgt[2] * w3wgt[3] * w4wgt[4] * w5wgt[5] * w6wgt[6] * w7wgt[7]) if yearidx==7 & !inlist(truncyear,71,72,73,77,78,79,80)
 
preserve
keep if (yearidx==7 & (regerrorcode_xtrccipwRi==0)) 
summ check_ipw ipw_full
restore

preserve
keep if (yearidx==7 & (regerrorcode_xtrccipwRi==3)) 
summ check_ipw ipw_full
restore

* MSM.
glm union c.age ln_wage birth_yr [pweight=check_ipw], family(binomial) vce(cluster idcode)

*xtrccipw i.union, idvar(idcode) timevar(year) timeidxvar(yearidx) generate(ipw_full5) linkfxn(logit) tdindepvars(ttl_exp) tiindepvars(i.collgrad) glmvars(age ln_wage birth_yr) glmfamily(binomial) 

