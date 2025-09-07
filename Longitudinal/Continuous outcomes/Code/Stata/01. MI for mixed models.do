****************************************************************************************************************
*https://stats.idre.ucla.edu/stata/faq/how-can-i-perform-multiple-imputation-on-longitudinal-data-using-ice/
***************************************************************************************************************

global progsinstall "nmissing"

        foreach y of global progsinstall  {
                ssc install "`y'"
        }
adopath + "C:/Users/rmjdshc/OneDrive - University College London/Learning/Software/Stata/ado files"
adopath + "C:/Users/rmjdshc/OneDrive - University College London/StataMP17/"
adopath + "C:/Users/rmjdsjmhc/OneDrive - University College London/StataMP18/"



*data in long form.
*missing data is "."

global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/02. Mixed Models/"
global data "${projdir}/data"
global outputs "${projdir}/outputs/Temp"
*use "https://stats.oarc.ucla.edu/stat/stata/faq/mi_longi.dta",clear
*save "C:\Users\rmjdshc\OneDrive - University College London\Learning\Longitudinal\02. Mixed Models\data\mi_longi.dta"
*use "${data}/mi_longi.dta", clear

use "https://stats.oarc.ucla.edu/stat/stata/faq/mi_longi.dta",clear


sum female ses private read math
nmissing

mi set mlong
mi misstable summarize female private ses read math /* 4 vars with missing data: female does not have any */

*private & ses is time-constant.
*read and math are time-varying.
*time is in the dataset.
*so we now have: read1; read2; read3: math1; math2; math3.

mi reshape wide read math, i(id) j(time)

*After the data is mi set, Stata requires 3 additional commands to complete our analysis. 

*******************************
*mi register imputed
*******************************

*The first is "mi register imputed". This command identifies which variables 
*in the imputation model have missing information.

mi register imputed private ses read1 read2 read3 math1 math2 math3

*******************************
*mi impute changed
*******************************

*The second command is "mi impute chained" where the user specifies the imputation model 
*to be used and the number of imputed data sets to be created. 
*Within this command we can specify a particular distribution to impute each variable under. 
*The chosen imputation method is listed with parentheses directly preceding the variable(s) 
*to which this distribution applies. 

*Note that we also use rseed to set the seed for the random number generator, 
*this will enable you to reproduce the results of our imputation.
*On the mi impute chained command line we can use the add option to specify the number of imputations to be performed. 
*In this example we chose 10 imputations. 
*Variables on the left side of the equal sign have missing information, 
*while the right side is reserved for variables with no missing information and are therefore 
*solely considered “predictors” of missing values.

mi impute chained (logit) private ///
(ologit) ses (regress) read1 read2 read3 math1 math2 math3 = female,  add(2) rseed(4657)

*So now we have our multiply imputed data, but they are still in wide format, and we 
*will probably want them in long form to run the analyses. 
*We can again use mi reshape, to reshape the data back to long

mi reshape long read math, i(id) j(time)
sum female private ses read math
mi estimate: mixed math read  c.time || id: c.time, variance ml covariance(un) 
mi estimate: mixed math read  c.time || id: , variance ml covariance(un) 

*====================================.
*MI (mixed models).
*===================================.

clear
set obs 100
gen id=_n
generate sex=round(runiformint(0,1),1)
generate cflisen0=round(runiformint(0,10),1)
generate cflisen1=round(runiformint(0,10),1)
generate cflisen2=round(runiformint(0,10),1)
generate psall0=round(runiformint(0,4),1)
generate psall1=round(runiformint(0,4),1)
generate psall2=round(runiformint(0,4),1)
replace psall1=. if (id==14|id==18|id==22)
replace psall2=. if (id==4|id==8|id==12)
reshape long cflisen psall, i(id) j(occasion)

mi set mlong
mi misstable summarize psall
mi reshape wide cflisen psall, i(id) j(occasion)
mi register imputed psall1 psall2
mi impute chained (regress) psall1 psall2 = sex, force add(2) rseed(4216) orderasis augment noimputed
mi reshape long cflisen psall, i(id) j(occasion)
mi estimate: mixed cflisen c.psall || id:, covariance(un)

************************
*Separate estimates.
************************

mi convert flong, clear
tab1 _mi_m

*complete-case
preserve
keep if _mi_m==0
mixed cflisen c.psall || id:, covariance(un)
restore

preserve
keep if _mi_m==1
mixed cflisen c.psall || id:, covariance(un)
restore

preserve
keep if _mi_m==2
mixed cflisen c.psall || id:, covariance(un)
restore


mi estimate, saving(M1.ster, replace) esample(esample): mixed cflisen c.psall i.sex || id:, covariance(un)
mi test 1.sex





