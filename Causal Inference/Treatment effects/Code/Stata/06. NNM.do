**************************************
********* Nearest Neighbour Matching 
**************************************


clear all
global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Causal Inference/Treatment effects/"
global data "${projdir}/data"
global outputs "${projdir}/outputs/Temp"

*Github location.
global Git_projdir "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
global Git_data "${Git_projdir}/Data/"

*use "${data}/cattaneo2.dta", clear
*github.
use "${Git_data}/cattaneo2.dta", clear

gen id=_n
teffects nnmatch (bweight mmarried mage fage medu prenatal1) (mbsmoke), ///
ematch(mmarried prenatal1) nneighbor(1)  generate(mid)
teffects nnmatch (bweight mmarried mage fage medu prenatal1) (mbsmoke), ///
ematch(mmarried prenatal1) nneighbor(1) atet
di e(k_nnmax) 
local nmatch = e(k_nnmax) 

*link matches.
preserve
keep id bweight
sort id
save "${outputs}/bweight.dta",replace
restore

forvalues i = 1(1)`nmatch' {
preserve
keep mid`i'
gen id = mid`i'
merge m:1 id using "${outputs}/bweight.dta", noreport
keep if _merge==3
rename bweight mbweight`i'
keep mid`i' mbweight`i'
sort mid`i' 
save "${outputs}/Temp`i'.dta",replace
restore
}

forvalues i = 1(1)`nmatch' {
sort mid`i'	
merge m:m mid`i' using "${outputs}/Temp`i'.dta", nogen noreport
}

egen mbweight = rowmean(mbweight1-mbweight16)
*ATE = (Tx - control).
generate diff=0
replace diff = (mbweight - bweight) if mbsmoke==0        
replace diff = (bweight - mbweight) if mbsmoke==1       
qui: summ diff
di "ATE=" r(mean)
qui: summ diff if mbsmoke==1
di "ATET=" r(mean)

*erase intermediate files.

erase "${outputs}/bweight.dta"
forvalues i = 1/`nmatch' {
erase "${outputs}/Temp`i'.dta"
}

di "Finished"














