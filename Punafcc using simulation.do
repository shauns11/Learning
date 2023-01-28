*******************************************************************
Modifiable risk factors for 9-year mortality in older English and
Brazilian adults: The ELSA and SIGa-Bagé ageing cohorts
Scientific Reports (2020) 10:4375 
*******************************************************************

clear
set seed 177894
set obs 1000

gen month1 = runiformint(1,12)
gen year1 = 2002
gen month2 = runiformint(1,12)
gen year2 = 2004
gen dead = uniform() < .45
gen deadm = runiformint(1,12)
gen deady = 2007

gen id=_n
gen sex = uniform() < .5
gen agew1 = runiformint(50,74)
gen wealth = runiformint(1,3)
gen smoke = uniform() < .1
gen obese = uniform() < .3
gen htn = uniform() < .2

*data in wide form

gen STARTdayW1=15
gen STARTmthW1=month1
gen STARTyrW1=year1
gen start=mdy(STARTmthW1,STARTdayW1,STARTyrW1)

gen ENDday=15
gen ENDmth=4
replace ENDmth=deadm if dead==1
gen ENDyr=2018
replace ENDyr=deady if dead==1
gen end=mdy(ENDmth,ENDday,ENDyr)

generate time = (end-start)/365.25

format start end %td

**************************************************
****** Change the end date in stset **************
****** maximum follow-up of 10 years *************
**************************************************
 
*stset end,failure(dead) enter(time start) exit(time mdy(1,15,2012)) scale(365.25)

*use origin to start at time 0.
stset end,failure(dead) origin(time start) exit(time mdy(1,15,2012)) scale(365.25)

gen diff = _t - _t0
summ diff

strate sex, per(1000)
stcox sex
sts test sex,logrank
sts graph, by(sex) adjustfor(agew1)

********************************************************
****** Model 1 (age- and sex-adjusted) *****************
********************************************************

stcox obese i.sex c.agew1,hr vce(robust)
punafcc,at(obese==0) eform vce(unconditional)

********************************************************
****** Model 2 (combined RF) *****************
********************************************************

stcox obese i.htn i.sex c.agew1,hr vce(robust)
punafcc,at(obese==0 htn==0) eform vce(unconditional)

********************************************************
****** Model 3 (wealth adj) *****************
********************************************************

stcox obese i.htn i.sex c.agew1 i.wealth,hr vce(robust)
punafcc,at(obese==0 htn==0) eform vce(unconditional)


********************************************************
*SES gradient in mortality
********************************************************

stcox ib1.wealth i.sex,nohr vce(robust) coeflegend

predict S0, basesurv // 
gen S1=S0^exp(_b[2.wealth])
gen S2=S0^exp(_b[3.wealth])
label var S0 "Richest"
label var S1 "Middle"
label var S2 "Poorest"

twoway (scatter S0  _t if _t<8.5, c(l) lpattern(dot) ms(none) sort)  /// 
(scatter S2 _t if _t<8.5 , c(l) lpattern(dash) ms(none) sort) ///
(scatter S1 _t if _t<8.5 , c(l) lpattern(solid) ms(none) sort), legend(cols(1)) title("")  ///
ylabel(0.5 0.75 1.00,ang(hor)) xtitle("Years") legend(pos(7) ring(0)) ytitle("Survival probability") /// 
scheme("") legend(width(30)) legend(symxsize(10))  legend(rowgap(*-1))




























