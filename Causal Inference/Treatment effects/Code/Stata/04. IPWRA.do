*********************
*IPWRA
*********************

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

qui: teffects ipwra (bweight fage mage mmarried prenatal1) ///
(mbsmoke fage mage mmarried fbaby,logit), pomeans aequations nolog
teffects ipwra (bweight fage mage mmarried prenatal1) ///
(mbsmoke fage mage mmarried fbaby,logit), ate aequations nolog

qui: logit mbsmoke fage mage mmarried fbaby
predict pscore
generate ipw=0
replace ipw=1/(1-pscore) if mbsmoke==0
replace ipw=1/(pscore) if mbsmoke==1
qui: reg bweight fage mage mmarried prenatal1 if mbsmoke==1 [pweight=ipw]
predict double pom_t
qui: reg bweight fage mage mmarried prenatal1 if mbsmoke==0 [pweight=ipw]
predict double pom_c
qui: summ pom_t
local pom_t = r(mean)
qui: summ pom_c
local pom_c = r(mean)
di "ATE="`pom_t' - `pom_c'


di "Finished"





