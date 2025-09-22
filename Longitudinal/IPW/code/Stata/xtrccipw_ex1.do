
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


*Github location.
global Git_projdir "https://github.com/shauns11/Learning/raw/refs/heads/main/Longitudinal/IPW/"
global Git_data "${Git_projdir}/Data/"
*github.
use "${Git_data}/nlswork5_xtrccipw.dta", clear

*use "${data}/nlswork5_xtrccipw.dta", clear

*--------------------------------------------------------------------------------------------------------------------------.
*Example 1 (truncation treated the same as drop-out).
*Excluding trtimevar(truncyear) from the xtrccipw call resulted in truncation being treated like dropout. 
*--------------------------------------------------------------------------------------------------------------------------.

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
xtrccipw i.union, idvar(idcode) timevar(year) timeidxvar(yearidx) generate(ipw_full) ///
linkfxn(logit) tdindepvars(ttl_exp) tiindepvars(i.collgrad) glmvars(age ln_wage birth_yr) glmfamily(binomial) 

gen asample=e(sample)
summ ipw_full       /* 979 records with a weight */
tab1 asample        /* 670 in the final model: di 205+121+105+71+67+53+48 */

*Replicate by hand.

* lagged outcome.

sort idcode year
by idcode: generate union1 = union[1]
by idcode: generate union2 = union[2]
by idcode: generate union3 = union[3]
by idcode: generate union4 = union[4]
by idcode: generate union5 = union[5]
by idcode: generate union6 = union[6]
by idcode: generate union7 = union[7]

label define censlbl 1 "not censored" 2 "censored"

*N=357 (152 censored; N=205 not censored).
preserve
keep if yearidx==1
gen outcome1=0
replace outcome1=1 if inlist(union1,0,1)
replace outcome1=2 if union1==.
label values outcome1 censlbl
tab1 outcome1
restore

*N=205 carried forward (84 censored; N=121 not censored).
preserve
keep if yearidx==2 & inlist(union1,0,1)
gen outcome2=0
replace outcome2=1 if inlist(union2,0,1)
replace outcome2=2 if (union2==.)
label values outcome2 censlbl
tab1 outcome2
restore

*N=121 carried forward (16 censored; N=105 not censored).
preserve
keep if yearidx==3 & inlist(union1,0,1) & inlist(union2,0,1)
gen outcome3=0
replace outcome3=1 if inlist(union3,0,1)
replace outcome3=2 if (union3==.)
label values outcome3 censlbl
tab1 outcome3
restore

*N=105 carried forward (34 censored;N=71 not censored).
preserve
keep if yearidx==4 & inlist(union1,0,1) & inlist(union2,0,1) & inlist(union3,0,1)
gen outcome4=0
replace outcome4=1 if inlist(union4,0,1)
replace outcome4=2 if (union4==.)
label values outcome4 censlbl
tab1 outcome4
restore

*N=71 carried forward (4 censored; N=67 not censored).
preserve
keep if yearidx==5 & inlist(union1,0,1) & inlist(union2,0,1) & inlist(union3,0,1) & inlist(union4,0,1)
gen outcome5=0
replace outcome5=1 if inlist(union5,0,1)
replace outcome5=2 if (union5==.)
label values outcome5 censlbl
tab1 outcome5
restore


*N=67 carried forward (14 censored; N=53 not censored).
preserve
keep if yearidx==6 & inlist(union1,0,1) & inlist(union2,0,1) & inlist(union3,0,1) & inlist(union4,0,1) & inlist(union5,0,1)
gen outcome6=0
replace outcome6=1 if inlist(union6,0,1)
replace outcome6=2 if (union6==.)
label values outcome6 censlbl
tab1 outcome6
restore

*N=53 carried forward (5 censored; N=48 not censored).

preserve
keep if yearidx==7 & inlist(union1,0,1) & inlist(union2,0,1) & inlist(union3,0,1) & inlist(union4,0,1) & inlist(union5,0,1) & inlist(union6,0,1)
gen outcome7=0
replace outcome7=1 if inlist(union7,0,1)
replace outcome7=2 if (union7==.)
label values outcome7 censlbl
tab1 outcome7
restore


*========.
*Wave 1.
*========.

gen dropw1=1
replace dropw1=0 if (union1==. & yearidx==1)
tab dropw1 if year==70

*Model drop out.
*N=357: censored (152); not censored (205).

logit dropw1 c.ttl_exp i.collgrad if yearidx==1      /* 1 = NOT censored */
predict cens
generate w1wgt=1/cens

*========.
*Wave 2.
*========.

*Drop-out at wave 2.

gen dropw2=1
replace dropw2=0 if (union2==. & yearidx==2)
tab dropw2 if yearidx==2

*Model drop out.
*N=205: censored (84); not censored (121).

drop cens
logit dropw2 union1 c.ttl_exp i.collgrad if yearidx==2                 /* 1 = NOT censored */
predict cens
generate w2wgt=1/cens

generate check_ipw=.
by idcode: replace check_ipw=w1wgt[1] if yearidx==1
by idcode: replace check_ipw=(w1wgt[1] * w2wgt[2]) if yearidx==2
list check_ipw ipw_full if yearidx==2 in 1/40

*========.
*Wave 3.
*========.

gen dropw3=1
replace dropw3=0 if (union3==. & yearidx==3)
tab dropw3 if yearidx==3

*Model drop out.
*N=121: censored (16); not censored (105).

drop cens
logit dropw3 union1 union2 c.ttl_exp i.collgrad if yearidx==3                 /* 1 = NOT censored */
predict cens
generate w3wgt=1/cens

by idcode: replace check_ipw=(w1wgt[1] * w2wgt[2] * w3wgt[3]) if yearidx==3
list check_ipw ipw_full if yearidx==3 in 1/40

*=========.
*Wave 4.
*=========

gen dropw4=1
replace dropw4=0 if (union4==. & yearidx==4)
tab dropw4 if yearidx==4

*Model drop out.
*N=105: censored (34); not censored (71).

drop cens
logit dropw4 union1 union2 union3 c.ttl_exp i.collgrad if yearidx==4                 /* 1 = NOT censored */
predict cens
generate w4wgt=1/cens

by idcode: replace check_ipw=(w1wgt[1] * w2wgt[2] * w3wgt[3] * w4wgt[4]) if yearidx==4
list check_ipw ipw_full if yearidx==4 in 1/80

*=======.
*Wave 5.
*=======.

gen dropw5=1
replace dropw5=0 if (union5==. & yearidx==5)|(truncyear==77)
tab dropw5 if yearidx==5

*Model drop out.
*N=71: censored (4); not censored (67).

tab regerrorcode_xtrccipwRi if yearidx==5

drop cens
logit dropw5 union1 union2 union3 union4 c.ttl_exp i.collgrad union4 if yearidx==5                /* 1 = NOT censored */
predict cens
generate w5wgt=1/cens

list check_ipw ipw_full if yearidx==5 & (regerrorcode_xtrccipwRi==0)           /* correct predictions for 6 cases */
list check_ipw ipw_full if yearidx==5 & (regerrorcode_xtrccipwRi==3)          /* replace with mean */

* 71 had a weight at wave 4.
* of those 4 dropped out.

tab union if yearidx==5 & inlist(union1,0,1) & inlist(union2,0,1) & inlist(union3,0,1) & inlist(union4,0,1), missing
* 1 /  probability of not censored
* 1 / (67/71)

replace w5wgt = 1 / (67/71) if (yearidx==5 & (regerrorcode_xtrccipwRi==3))
by idcode: replace check_ipw=(w1wgt[1] * w2wgt[2] * w3wgt[3] * w4wgt[4] * w5wgt[5]) if yearidx==5
list check_ipw ipw_full if yearidx==5 & ipw_full!=.

*========.
*Wave 6.
*========.

tab union if yearidx==6 & inlist(union1,0,1) & inlist(union2,0,1) & inlist(union3,0,1) & inlist(union4,0,1) & inlist(union5,0,1), missing

*(53/67)

gen dropw6=1
replace dropw6=0 if (union6==. & yearidx==6)|(truncyear==78)
tab dropw6 if yearidx==6

*Model drop out.
*N=67: censored (14); not censored (53).

tab regerrorcode_xtrccipwRi if yearidx==6

drop cens
logit dropw6 union1 union2 union3 union4 union5 c.ttl_exp i.collgrad if yearidx==6                /* 1 = NOT censored */
predict cens
generate w6wgt=1/cens

summ w6wgt if (yearidx==6 & (regerrorcode_xtrccipwRi==3))
replace w6wgt = 1 / (53/67) if (yearidx==6 & (regerrorcode_xtrccipwRi==3))
by idcode: replace check_ipw=(w1wgt[1] * w2wgt[2] * w3wgt[3] * w4wgt[4] * w5wgt[5] * w6wgt[6]) if yearidx==6

list check_ipw ipw_full if yearidx==6 & (regerrorcode_xtrccipwRi==0)           /* correct predictions for 6 cases */
list check_ipw ipw_full if yearidx==6 & (regerrorcode_xtrccipwRi==3)          /* replace with mean */

*Wave 7.
 
tab union if yearidx==7 & inlist(union1,0,1) & inlist(union2,0,1) & inlist(union3,0,1) & inlist(union4,0,1) & inlist(union5,0,1) & inlist(union6,0,1), missing

*(48/53)

gen dropw7=1
replace dropw7=0 if (union7==. & yearidx==7)|(truncyear==79)
tab dropw7 if yearidx==7

*Model drop out.
*N=53: censored (4); not censored (48).

tab regerrorcode_xtrccipwRi if yearidx==7

drop cens
logit dropw7 union1 union2 union3 union4 union5 union6 c.ttl_exp i.collgrad if yearidx==7                /* 1 = NOT censored */
predict cens
generate w7wgt=1/cens
summ w7wgt if (yearidx==7 & (regerrorcode_xtrccipwRi==3))
replace w7wgt = 1 / (48/53) if (yearidx==7 & (regerrorcode_xtrccipwRi==3))
by idcode: replace check_ipw=(w1wgt[1] * w2wgt[2] * w3wgt[3] * w4wgt[4] * w5wgt[5] * w6wgt[6] * w7wgt[7]) if yearidx==7

list check_ipw ipw_full if yearidx==7 & (regerrorcode_xtrccipwRi==0)           /* correct predictions for 6 cases */
list check_ipw ipw_full if yearidx==7 & (regerrorcode_xtrccipwRi==3)          /* replace with mean */

* MSM on long data using time-varying ipw.

glm union c.age ln_wage birth_yr [pweight=check_ipw], family(binomial) vce(cluster idcode)








