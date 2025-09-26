************************************************************************************
************** xtrccipw ************************************************************
**** inverse-probability weights for longitudinal data with dropout or truncation
*inverse probability weighting
*weight to adjust for censoring and mortality
*************************************************************************************

clear 
set seed 174906
set obs 1000
gen id=_n
gen indsex = uniform() < .5
label define censlbl 0 "alive but censored" 1 "non-censored" 2 "died"

gen y1=runiformint(0,10)
gen y2=runiformint(0,10)
gen y3=runiformint(0,10)
gen y4=runiformint(0,10)
gen y5=runiformint(0,10)
gen y6=runiformint(0,10)
gen y7=runiformint(0,10)
gen y8=runiformint(0,10)
gen y9=runiformint(0,10)

gen deadw1=0
gen deadw2 = uniform() < .02
gen deadw3 = uniform() < .03 & deadw2==0
gen deadw4 = uniform() < .04 & (deadw2==0 & deadw3==0)
gen deadw5 = uniform() < .05 & (deadw2==0 & deadw3==0 & deadw4==0)
gen deadw6 = uniform() < .06 & (deadw2==0 & deadw3==0 & deadw4==0 & deadw5==0)
gen deadw7 = uniform() < .07 & (deadw2==0 & deadw3==0 & deadw4==0 & deadw5==0 & deadw6==0)
gen deadw8 = uniform() < .08 & (deadw2==0 & deadw3==0 & deadw4==0 & deadw5==0 & deadw6==0 & deadw7==0)
gen deadw9 = uniform() < .09 & (deadw2==0 & deadw3==0 & deadw4==0 & deadw5==0 & deadw6==0 & deadw7==0 & deadw8==0)

*censored observations
replace y1=. if inrange(id,1,10)
replace y2=. if inrange(id,101,110)
replace y3=. if inrange(id,201,210)
replace y4=. if inrange(id,301,310)
replace y5=. if inrange(id,401,410)
replace y6=. if inrange(id,501,510)
replace y7=. if inrange(id,601,610)
replace y8=. if inrange(id,701,710)
replace y9=. if inrange(id,801,810)

replace y2=. if deadw2==1
replace y3=. if (deadw2==1|deadw3==1)
replace y4=. if (deadw2==1|deadw3==1|deadw4==1)
replace y5=. if (deadw2==1|deadw3==1|deadw4==1|deadw5==1)
replace y6=. if (deadw2==1|deadw3==1|deadw4==1|deadw5==1|deadw6==1)
replace y7=. if (deadw2==1|deadw3==1|deadw4==1|deadw5==1|deadw6==1|deadw7==1)
replace y8=. if (deadw2==1|deadw3==1|deadw4==1|deadw5==1|deadw6==1|deadw7==1|deadw8==1)
replace y9=. if (deadw2==1|deadw3==1|deadw4==1|deadw5==1|deadw6==1|deadw7==1|deadw8==1|deadw9==1)


**********************************************
*this is done on wide rather than long data.
*In examples 2 & 3, data is in long form
**********************************************

*7395 observations.
di 990 + 962 + 931 + 882 + 832 + 780 + 733 + 677 + 608 


*************************
*Outcome at Wave 1
*censoring but not deaths
**************************

gen CensW1=.
replace CensW1=0 if (y1==.)  /* 10 */
replace CensW1=1 if (y1!=.)  /* 990  */
tab1 CensW1

******************************************
*Outcome at Wave 2
*conditional on taking part at previous wave
*********************************************

preserve
*take forward from W1 (n=990)
keep if y1!=.
count if y2!=.               /* non-censored */
count if y2==. & deadw2==0   /* censored */
count if deadw2==1           /* died before W2 */
restore

gen CensW2=.
replace CensW2=0 if (y1!=. & y2==.) & (deadw2==0)               /*censored: 10*/
replace CensW2=1 if (y1!=. & y2!=.)                             /*not censored: 962*/
replace CensW2=2 if (deadw2==1)                                 /*truncated: 18*/
tab1 CensW2

******************************************
*Outcome at Wave 3
*********************************************

preserve
*take forward from W2 (n=962)
keep if y1!=. & y2!=.
summ y3 if deadw3==1
count if y3!=.               /* non-censored */
count if y3==. & (deadw3==0)   /* censored */
count if deadw3==1           /* died before W3 */
restore

gen CensW3=.
replace CensW3=0 if (y1!=. & y2!=. & y3==.) & (deadw3==0)                  /*censored: 10*/
replace CensW3=1 if (y1!=. & y2!=. & y3!=.)                                /*not censored: 931*/
replace CensW3=2 if (y1!=. & y2!=. & deadw3==1)                            /*truncated: 21*/
tab1 CensW3

******************************************
*Outcome at Wave 4
*********************************************

preserve
*take forward from W3 (n=931)
keep if y1!=. & y2!=. & y3!=.
summ y4 if deadw4==1
count if y4!=.               /* non-censored */
count if y4==. & (deadw4==0)   /* censored */
count if deadw4==1           /* died before W4 */
restore

gen CensW4=.
replace CensW4=0 if (y1!=. & y2!=. & y3!=. & y4==.) & (deadw4==0)                          /*censored: 10*/
replace CensW4=1 if (y1!=. & y2!=. & y3!=. & y4!=.)                                         /*not censored: 882*/
replace CensW4=2 if (y1!=. & y2!=. & y3!=. & deadw4==1)                                     /*truncated: 39*/
tab1 CensW4


******************************************
*Outcome at Wave 5
*********************************************

preserve
*take forward from W4 (n=882)
keep if y1!=. & y2!=. & y3!=. & y4!=.
summ y5 if deadw5==1
count if y5!=.               /* non-censored */
count if y5==. & (deadw5==0)   /* censored */
count if deadw5==1           /* died before W4 */
restore

gen CensW5=.
replace CensW5=0 if (y1!=. & y2!=. & y3!=. & y4!=. & y5==.) & (deadw5==0)                   /*censored: 8*/
replace CensW5=1 if (y1!=. & y2!=. & y3!=. & y4!=. & y5!=.)                                 /*not censored: 832*/
replace CensW5=2 if (y1!=. & y2!=. & y3!=. & y4!=. & deadw5==1)                             /*truncated: 42*/
tab1 CensW5


******************************************
*Outcome at Wave 6
*********************************************

preserve
*take forward from W5 (n=832)
keep if y1!=. & y2!=. & y3!=. & y4!=. & y5!=.
summ y6 if deadw6==1
count if y6!=.               /* non-censored */
count if y6==. & (deadw6==0)   /* censored */
count if deadw6==1           /* died before W4 */
restore

gen CensW6=.
replace CensW6=0 if (y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6==.) & (deadw6==0)                                  /*censored: 7*/
replace CensW6=1 if (y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6!=.)                                              /*not censored: 780*/
replace CensW6=2 if (y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & deadw6==1)                                         /*truncated 45*/
tab1 CensW6


******************************************
*Outcome at Wave 7
*********************************************

preserve
*take forward from W6 (n=780)
keep if y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6!=.
summ y7 if deadw7==1
count if y7!=.               /* non-censored */
count if y7==. & (deadw7==0)   /* censored */
count if deadw7==1           /* died before W4 */
restore

gen CensW7=.
replace CensW7=0 if (y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6!=. & y7==.) & (deadw7==0)      /*censored: 8*/
replace CensW7=1 if (y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6!=. & y7!=.)                    /*not censored: 733*/
replace CensW7=2 if (y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6!=. & deadw7==1)                /*truncated: 39*/
tab1 CensW7

******************************************
*Outcome at Wave 8
*********************************************

preserve
*take forward from W7 (n=733)
keep if y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6!=. & y7!=.
summ y8 if deadw8==1
count if y8!=.               /* non-censored */
count if y8==. & (deadw8==0)   /* censored */
count if deadw8==1           /* died before W4 */
restore

gen CensW8=.
replace CensW8=0 if (y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6!=. & y7!=. & y8==.) & (deadw8==0)           /*censored 8*/
replace CensW8=1 if (y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6!=. & y7!=. & y8!=.)                         /*not censored 677*/
replace CensW8=2 if (y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6!=. & y7!=. & deadw8==1)                    /*truncated 48*/
tab1 CensW8

******************************************
*Outcome at Wave 9
*********************************************

preserve
*take forward from W8 (n=677)
keep if y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6!=. & y7!=. & y8!=.
summ y9 if deadw9==1
count if y9!=.               /* non-censored */
count if y9==. & (deadw9==0)   /* censored */
count if deadw9==1           /* died before W4 */
restore

gen CensW9=.
replace CensW9=0 if (y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6!=. & y7!=. & y8!=. & y9==.) & (deadw9==0)       /*censored 8*/
replace CensW9=1 if (y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6!=. & y7!=. & y8!=. & y9!=.)                     /*not censored 608*/
replace CensW9=2 if (y1!=. & y2!=. & y3!=. & y4!=. & y5!=. & y6!=. & y7!=. & y8!=. & deadw9==1)                 /*truncated 61*/
tab1 CensW9

tab1 Cens*

***********************************************************
*truncation event is death
*make sure year is consistent with wave: 1 = baseline
*******************************************************

gen truncyr=.
replace truncyr=1 if CensW1==2    /* no-one died before W1 */
replace truncyr=2 if CensW2==2    /* died before W2 */
replace truncyr=3 if CensW3==2    /* died before W3 */
replace truncyr=4 if CensW4==2    /* died before W4 */
replace truncyr=5 if CensW5==2    /* died before W5 */
replace truncyr=6 if CensW6==2    /* died before W6 */
replace truncyr=7 if CensW7==2    /* died before W7 */
replace truncyr=8 if CensW8==2    /* died before W8 */
replace truncyr=9 if CensW9==2    /* died before W9 */

**********************
*Logistic regressions
**********************

keep id y* indsex truncyr CensW1 CensW2 CensW3 CensW4 CensW5 CensW6 CensW7 CensW8 CensW9

reshape long y,i(id) j(wave) 

sort id wave
by id: generate y_a = y[1]
by id: generate y_b = y[2]
by id: generate y_c = y[3]
by id: generate y_d = y[4]
by id: generate y_e = y[5]
by id: generate y_f = y[6]
by id: generate y_g = y[7]
by id: generate y_h = y[8]
by id: generate y_i = y[8]

*Wave 1.
qui:logit CensW1 i.indsex if wave==1 & inlist(CensW1,0,1)
di e(N)
predict a if e(sample)
gen ipw1=(1/a) if CensW1==1
drop a

*Wave 2.
qui:logit CensW2 i.indsex y_a if wave==2 & inlist(CensW2,0,1)
di e(N)
predict a if e(sample)
gen ipw2=(1/a) if CensW2==1
drop a

*Wave 3.
qui:logit CensW3 i.indsex y_a y_b if wave==3 & inlist(CensW3,0,1)
di e(N)
predict a if e(sample)
gen ipw3=(1/a) if CensW3==1
drop a

*Wave 4.
qui:logit CensW4 i.indsex y_a y_b y_c if wave==4 & inlist(CensW4,0,1)
di e(N)
predict a if e(sample)
gen ipw4=(1/a) if CensW4==1
drop a

*Wave 5.
qui:logit CensW5 i.indsex y_a y_b y_c y_d if wave==5 & inlist(CensW5,0,1)
di e(N)
predict a if e(sample)
gen ipw5=(1/a) if CensW5==1
drop a

*Wave 6.
qui:logit CensW6 i.indsex y_a y_b y_c y_d y_e if wave==6 & inlist(CensW6,0,1)
di e(N)
predict a if e(sample)
gen ipw6=(1/a) if CensW6==1
drop a

*Wave 7.
qui:logit CensW7 i.indsex y_a y_b y_c y_d y_e y_f if wave==7 & inlist(CensW7,0,1)
di e(N)
predict a if e(sample)
gen ipw7=(1/a) if CensW7==1
drop a

*Wave 8.
qui:logit CensW8 i.indsex y_a y_b y_c y_d y_e y_f y_g if wave==8 & inlist(CensW8,0,1)
di e(N)
predict a if e(sample)
gen ipw8=(1/a) if CensW8==1
drop a

*Wave 9.
qui:logit CensW9 i.indsex y_a y_b y_c y_d y_e y_f y_g y_h if wave==9 & inlist(CensW9,0,1)
di e(N)
predict a if e(sample)
gen ipw9=(1/a) if CensW9==1
drop a

gen ipw=.
by id: replace ipw=ipw1[1] if wave==1
by id: replace ipw=(ipw1[1]*ipw2[2]) if wave==2
by id: replace ipw=(ipw1[1]*ipw2[2]*ipw3[3]) if wave==3
by id: replace ipw=(ipw1[1]*ipw2[2]*ipw3[3]*ipw4[4]) if wave==4
by id: replace ipw=(ipw1[1]*ipw2[2]*ipw3[3]*ipw4[4]*ipw5[5]) if wave==5
by id: replace ipw=(ipw1[1]*ipw2[2]*ipw3[3]*ipw4[4]*ipw5[5]*ipw6[6]) if wave==6
by id: replace ipw=(ipw1[1]*ipw2[2]*ipw3[3]*ipw4[4]*ipw5[5]*ipw6[6]*ipw7[7]) if wave==7
by id: replace ipw=(ipw1[1]*ipw2[2]*ipw3[3]*ipw4[4]*ipw5[5]*ipw6[6]*ipw7[7]*ipw8[8]) if wave==8
by id: replace ipw=(ipw1[1]*ipw2[2]*ipw3[3]*ipw4[4]*ipw5[5]*ipw6[6]*ipw7[7]*ipw8[8]*ipw9[9]) if wave==9

di 990+962+931+882+832+780+733+677+608
*7395

glm y i.indsex [pweight=ipw],family(gaussian) vce(cluster id)

************************************************
*Stata programme
************************************************

version 16
xtrccipw y,idvar(id) timevar(wave) timeidxvar(wave) gen(ipw_a) linkfxn(logit) ///
trtimevar(truncyr) tiindepvars(i.indsex) ///
glmvars(i.indsex) glmfamily(gaussian)

keep if e(sample)
summ ipw ipw_a














































































































































