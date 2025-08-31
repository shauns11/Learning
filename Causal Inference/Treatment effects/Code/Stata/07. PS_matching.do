*************************************
**PSM 
**************************************

clear all
global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Causal Inference/Treatment effects/"
global data "${projdir}/data"
global outputs "${projdir}/outputs/Temp"

set seed 67952
set obs 1000
gen id=_n
gen treat = uniform() < .4
gen outcome= runiformint(1,10)
gen x1= runiformint(1,10)
gen x2= runiformint(1,10)
teffects psmatch (outcome) (treat x1 x2,logit),gen(match)
teffects psmatch (outcome) (treat x1 x2,logit),atet
teffects psmatch (outcome) (treat x1 x2,logit),ate

local nmatch = e(k_nnmax) 

preserve
keep id outcome
sort id
save "${outputs}/outcome.dta",replace
restore

*max number of matches
local nmatch = e(k_nnmax)

forvalues i = 1(1)`nmatch' {
preserve
keep match`i'
gen id = match`i'
merge m:1 id using "${outputs}/outcome.dta", noreport
keep if _merge==3
rename outcome m_outcome`i'
keep match`i' m_outcome`i'
sort match`i' 
save "${outputs}/Temp`i'.dta",replace
restore
}

forvalues i = 1(1)`nmatch' {
sort match`i'	
merge m:m match`i' using "${outputs}/Temp`i'.dta", nogen noreport
}

egen m_outcome = rowmean(m_outcome1-m_outcome`nmatch')

*ATE = (Tx - control).
generate diff=0
replace diff = (m_outcome - outcome) if treat==0        
replace diff = (outcome - m_outcome) if treat==1       
qui: summ diff
di "ATE=" r(mean)
qui: summ diff if treat==1
di "ATET=" r(mean)


*erase intermediate files.
erase "${outputs}/outcome.dta"
forvalues i = 1/11 {
erase "${outputs}/Temp`i'.dta"
}

di "Finished"



