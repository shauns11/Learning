****************************
*GEE for binary outcomes.
****************************

global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/01. Binary outcomes/"
global data "${projdir}/data"
global outputs "${projdir}/outputs/Temp"

*https://stats.oarc.ucla.edu/stata/dae/mixed-effects-logistic-regression/

*Grab the most recent version from the internet
clear
insheet using "https://stats.idre.ucla.edu/stat/data/hdp.csv", comma

*string variables to numeric.

foreach v of varlist familyhx smokinghx sex cancerstage school {
encode `v', gen(`v'2)
drop `v'
rename `v'2 `v'
}

keep did remission il6 crp cancerstage lengthofstay experience

desc did remission il6 crp cancerstage lengthofstay experience
*mixed-effects.
melogit remission il6 crp i.cancerstage lengthofstay experience || did: , or

*GEE.
xtset did
*Independent.
xtgee remission il6 crp i.cancerstage lengthofstay experience , family(binomial) link(logit) corr(independent) eform
*Exchangeable.
xtgee remission il6 crp i.cancerstage lengthofstay experience, family(binomial) link(logit) corr(exchangeable) eform
margins,at(cancerstage=(1 2 3 4))   // predicted probabilities
margins, dydx(cancerstage)          // difference in probabilities (versus reference group)

di ("GEE finished")


