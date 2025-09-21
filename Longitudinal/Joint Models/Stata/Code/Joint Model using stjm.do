*Synthetic data for running a joint model using stjm.

global progsinstall "stjm" 
        foreach y of global progsinstall  {
                capture ssc install "`y'"
        }
global progsinstall "merlin" 
        foreach y of global progsinstall  {
                capture ssc install "`y'"
        }		
		
adopath + "C:/Users/rmjdshc/OneDrive - University College London/Learning/Software/Stata/ado files"
adopath + "C:/Users/rmjdshc/OneDrive - University College London/StataMP17/"
adopath + "C:/Users/rmjdsjmhc/OneDrive - University College London/StataMP18/"

clear
set obs 1000
set seed 56803580
gen id=_n
gen sex = uniform() < .5

*Wave 1.
*generate w1 interview date.
generate w1int = runiform(mdy(1,1,2002), mdy(12,31,2002))
format w1int %td
generate y1 = floor(runiform() * 21)


*between wave1 and wave 2 there were 20 deaths
generate rand = runiform()
sort rand
generate deathw1w2 = _n <= 20
sort id
* 1. Create a variable to hold the new date
generate dod = .
* 2. For flagged observations, generate a random date in the following year (2003)
replace dod = runiform(mdy(1,1,2003), mdy(12,31,2003)) if deathw1w2 == 1
format dod %td
drop rand

*Wave 2.
*980 remaining at wave 2.
generate y2 = y1 + rnormal(0, 3) if deathw1w2==0
* 2. Clip values to the 0–20 range and round to integers
replace y2 = min(max(y2, 0), 20) if deathw1w2==0
replace y2 = round(y2) if deathw1w2==0
generate w2int = .
replace w2int = runiform(mdy(1,1,2003), mdy(12,31,2003)) if deathw1w2 == 0
format w2int %td

*between wave2 and wave 3 there were 30 deaths
generate rand = runiform() if deathw1w2==0
sort rand
generate deathw2w3 = _n <= 30
sort id
replace dod = runiform(mdy(1,1,2004), mdy(12,31,2004)) if deathw2w3 == 1
format dod %td
drop rand


*Wave 3.
*950 remaining at wave 3.
generate y3 = y2 + rnormal(0, 3) if (deathw1w2==0 & deathw2w3 == 0)
replace y3 = min(max(y3, 0), 20) if (deathw1w2==0 & deathw2w3 == 0)
replace y3 = round(y3) if (deathw1w2==0 & deathw2w3 == 0)
generate w3int = .
replace w3int = runiform(mdy(1,1,2004), mdy(12,31,2004)) if (deathw1w2==0 & deathw2w3 == 0)
format w3int %td

*between wave3 and wave 4 there were 70 deaths
generate rand = runiform() if (deathw1w2==0 & deathw2w3 == 0)
sort rand
generate deathw3w4 = _n <= 70
sort id
replace dod = runiform(mdy(1,1,2005), mdy(12,31,2005)) if deathw3w4==1
format dod %td
drop rand


*Wave 4.
*880 remaining at wave 4.
generate y4 = y3 + rnormal(0, 3) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0)
replace y4 = min(max(y4, 0), 20) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0)
replace y4 = round(y4) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0)
generate w4int = .
replace w4int = runiform(mdy(1,1,2005), mdy(12,31,2005)) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0)
format w4int %td

*between wave4 and wave 5 there were 100 deaths
generate rand = runiform() if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0)
sort rand
generate deathw4w5 = _n <= 100
sort id
replace dod = runiform(mdy(1,1,2006), mdy(12,31,2006)) if deathw4w5==1
format dod %td
drop rand


*Wave 5.
*780 remaining at wave 5.
generate y5 = y4 + rnormal(0, 3) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0)
replace y5 = min(max(y5, 0), 20) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0)
replace y5 = round(y5) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0)
generate w5int = .
replace w5int = runiform(mdy(1,1,2006), mdy(12,31,2006)) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0)
format w5int %td

*between wave5 and wave 6 there were 140 deaths
generate rand = runiform() if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0)
sort rand
generate deathw5w6 = _n <= 140
sort id
replace dod = runiform(mdy(1,1,2007), mdy(12,31,2007)) if deathw5w6==1
format dod %td
drop rand


*Wave 6.
*640 remaining at wave 6.
generate y6 = y5 + rnormal(0, 3) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0)
replace y6 = min(max(y6, 0), 20) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0)
replace y6 = round(y6) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0)
generate w6int = .
replace w6int = runiform(mdy(1,1,2007), mdy(12,31,2007)) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0)
format w6int %td

*between wave6 and wave 7 there were 180 deaths
generate rand = runiform() if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0)
sort rand
generate deathw6w7 = _n <= 180
sort id
replace dod = runiform(mdy(1,1,2008), mdy(12,31,2008)) if deathw6w7==1
format dod %td
drop rand


*Wave 7.
*460 remaining at wave 7.
generate y7 = y6 + rnormal(0, 3) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0 & deathw6w7 == 0)
replace y7 = min(max(y7, 0), 20) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0 & deathw6w7 == 0)
replace y7 = round(y7) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0 & deathw6w7 == 0)
generate w7int = .
replace w7int = runiform(mdy(1,1,2008), mdy(12,31,2008)) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0 & deathw6w7 == 0)
format w7int %td

*between wave7 and wave 8 there were 200 deaths
generate rand = runiform() if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0 & deathw6w7 == 0)
sort rand
generate deathw7w8 = _n <= 200
sort id
replace dod = runiform(mdy(1,1,2009), mdy(12,31,2009)) if deathw7w8==1
format dod %td
drop rand

*Wave 8.
*260 remaining at wave 7.
generate y8 = y7 + rnormal(0, 3) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0 & deathw6w7 == 0 & deathw7w8 == 0)
replace y8 = min(max(y8, 0), 20) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0 & deathw6w7 == 0 & deathw7w8 == 0)
replace y8 = round(y8) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0 & deathw6w7 == 0 & deathw7w8 == 0)
generate w8int = .
replace w8int = runiform(mdy(1,1,2009), mdy(12,31,2009)) if (deathw1w2==0 & deathw2w3 == 0 & deathw3w4 == 0 & deathw4w5 == 0 & deathw5w6 == 0 & deathw6w7 == 0 & deathw7w8 == 0)
format w8int %td

summ y* 
pwcorr y*


rename (y1 y2 y3 y4 y5 y6 y7 y8) (y0 y1 y2 y3 y4 y5 y6 y7)
rename (w1int w2int w3int w4int w5int w6int w7int w8int) (intdate0 intdate1 intdate2 intdate3 intdate4 intdate5 intdate6 intdate7)

***********************************************
*restrict to analytical sample before reshape
***********************************************

generate asample0 = inrange(y0,0,20)
generate asample1 = inrange(y1,0,20)
generate asample2 = inrange(y2,0,20)
generate asample3 = inrange(y3,0,20)
generate asample4 = inrange(y4,0,20)
generate asample5 = inrange(y5,0,20)
generate asample6 = inrange(y6,0,20)
generate asample7 = inrange(y7,0,20)

tab1 asample*
di 1000+980+950+880+780+640+460+260
*5950
gen dead = !missing(dod)
tab1 dead  // 740 deaths

clonevar w0int = intdate0

keep id sex intdate* y* dead dod w0int asample*

reshape long intdate y asample, i(id) j(wave)
keep if asample==1
count
sort id wave

egen first = tag(id) 
tab1 dead if first==1     /* 740 deaths */
drop first


*Dataset contains (i) interview dates; (ii) binary variable for death; (iii) date-of-death.
*For each record the participant was alive.

*===========================================================.
*Analysis time = (interview date - baseline interview date).
*time at _n=1: W2 interview date (exit) - W1 interview date (w0int)
*time at _n=2: W3 interview date (exit) - W1 interview date (w0int)
*===========================================================.

*Beginning = w0int.
*Exit for each record = interview date of the next record.

by id: gen exit_date = intdate[_n+1]     /* W2int_m moved to _n==1 */        
format exit_date %td 

list id y w0int intdate exit_date wave dead in 1/7


*================================================================================================================.
*Censoring date MUST be on last record if dead==0.
*set to last interview + 30 days
*================================================================================================================.

by id: replace exit_date = intdate+30 if (_n==_N) & dead==0 

*==============================================================.
*Dead=1 must be only on the last record: exit as time as death.
*==============================================================.

by id: replace dead=0 if (_n!=_N) & dead==1
by id: replace exit_date=dod if (_n==_N) & dead==1


*=====================.
*stset
*======================

drop if intdate==.

generate time = (exit_date-w0int)/365.25 
stset time, id(id) failure(dead==1) 

stjm y sex, panel(id) ///
timeinteraction(sex) ///
survm(weibull) rfp(1) ///
survcov(sex) nonadapt










































