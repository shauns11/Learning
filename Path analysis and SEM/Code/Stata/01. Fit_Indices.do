******************
*01. Fit Indices.
******************

global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Path Analysis and SEM/"
global data "${projdir}/data"
global outputs "${projdir}/outputs/Temp"

global projdir_a "C:/Users/rmjdshc/OneDrive - University College London/Learning/Path Analysis and SEM/"
global data_a "${projdir_a}/data"
global outputs_a "${projdir_a}/outputs/Temp"

*****************************
*Fit indices (SEM.)
*https://stats.oarc.ucla.edu/stata/faq/what-are-the-saturated-and-baseline-models-in-sem/
*****************************.

*e(df m) model degrees of freedom
*e(df b) baseline model degrees of freedom
*e(df s) saturated model degrees of freedom
*e(chi2 ms) test of target model against saturated model
*e(df ms) degrees of freedom for e(chi2 ms)
*e(p ms) p-value for e(chi2 ms)

clear
use https://stats.oarc.ucla.edu/stat/data/hsbdemo.dta, clear


*Model 1.
sembuilder "N:/Learning/Path Analysis and SEM/SEM diagrams/Fit indices.stsem"
sem (Acad -> math science socst) /// measurement model
(read -> Acad)(female -> read), ///
    mean(female) var(female) nolog
local modelLL = e(ll) 
local df_m = e(df_m)        //model df
local df_b = e(df_b)        //baseline df
local df_s = e(df_s)        //saturated df
local edf_m = e(df_m)       //model df
local edf_b = e(df_b)       //baseline df
local edf_s = e(df_s)        //saturated df
estat gof

*saturated model (only observed variables).
sem (<-read math science socst female)
local sLL = e(ll) 
local k = 5
di `k'*(`k'+1)/2 + `k'

*model versus saturated 
di -2*((`modelLL')-(`sLL'))
di `df_s' - `df_m'

*baseline model (no variables are related)
sem (<-read math science socst female),               ///
    covstr(read math science socst female, diagonal)
local bLL = e(ll) 

*baseline versus saturated 
di -2*((`bLL')-(`sLL'))
di `df_s' - `df_b'


*Model 2.

*different saturated model.
sem (Acad -> math science socst)(Acad<-read)(Acad<-female)(read<-female) ///
     (math<- female read)(socst<-female read),                           ///
     mean(female) var(female)

*different baseline model.
sem (Acad ->math@1 science@1 socst@1) (read<-female@0), ///
       var(Acad@0 female) mean(female)

*RMSEA (model vs saturated).
di ((`t1'-27)/(1625*27))^0.5

*CFI (baseline vs saturated).
di 1-((`t1'-27)/(`t2'-36))


*simulated data.

clear 
set obs 1000
set seed 432905
gen id=_n
gen exp = runiformint(16,44)
gen outcome = runiformint(20,30)
gen confounder = runiformint(20,30)
gen mediator = runiformint(10,20)
gen rf = runiformint(10,20)
sem (exp confounder -> mediator) ///
		(exp confounder mediator rf -> outcome) 
estat teffects, compact
*total effect = -.0044767 
estat gof,stats(all)

*********************
*model vs saturated.
*********************

*model 
sem (exp confounder -> mediator) (exp confounder mediator rf -> outcome) 
local m1 = e(ll) 
di e(df_m) // 10 parameters
di e(df_s) // 20 parameters
di e(chi2_ms) // test of target model against saturated model
di e(df_ms)   // 1 

*saturated model.
sem(<-exp confounder mediator rf outcome)
local m2 = e(ll) 
*e(df_m) =  20    // 20 parameters

*baseline model.
sem(<-exp confounder mediator rf outcome), ///
covstr(exp confounder mediator rf outcome, diagonal)
local m3 = e(ll) 
*e(df_m) =  10    // 10 parameters

*model versus saturated. 
di 2*((`m2')-(`m1'))
local t1= 2*((`m2')-(`m1'))
di `t1'

*baseline versus saturated.
di 2*((`m2')-(`m3'))
local t2=2*((`m2')-(`m3'))

*RMSEA (model vs saturated).
di ((`t1'-27)/(1625*27))^0.5

*CFI (baseline vs saturated).
di 1-((`t1'-27)/(`t2'-36))





























