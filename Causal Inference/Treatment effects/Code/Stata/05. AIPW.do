*******************
*AIPW 
*******************

clear all
global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Causal Inference/Treatment effects/"
global data "${projdir}/data"
global outputs "${projdir}/outputs"

*Github location.
global Git_projdir "https://github.com/shauns11/Learning/raw/refs/heads/main/Causal%20Inference/Treatment%20effects/"
global Git_data "${Git_projdir}/Data/"

*use "${data}/cattaneo2.dta", clear
*github.
use "${Git_data}/cattaneo2.dta", clear
rename mbsmoke treat
teffects aipw (bweight mage prenatal1 mmarried fbaby)                 ///
                (treat mmarried c.mage fbaby medu, logit)    ///
                , pomeans aequations nolog

				teffects aipw (bweight mage prenatal1 mmarried fbaby)                 ///
                (treat mmarried c.mage fbaby medu, logit)    ///
                , ate nolog
*teffects overlap, nolog	

qui: logit treat mmarried c.mage fbaby medu
predict pscore if e(sample)
*Need two separate weights
gen double ipw0 = 0.treat/(1-pscore)
gen double ipw1 = 1.treat/pscore 

*POM (treatment)
*No weights in outcome model 
qui: reg bweight mage prenatal1 mmarried fbaby if treat == 1
predict double pom1
*Use weight to correct prediction (note observed outcome here)
replace pom1 = pom1 + ipw1*(bweight - pom1)

*POM (control)
qui: reg bweight mage prenatal1 mmarried fbaby if treat == 0
predict double pom0
replace pom0 = pom0 + ipw0*(bweight - pom0)
qui: sum pom0
local pom_c = r(mean)
qui: sum pom1
local pom_t = r(mean)
di "AIPW="`pom_t' - `pom_c'	

di "Finished"













