====================================.
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

tab1 cflisen*
recode cflisen0 (0/4=0) (5/10=1)
recode cflisen1 (0/4=0) (5/10=1)
recode cflisen2 (0/4=0) (5/10=1)

reshape long cflisen psall, i(id) j(occasion)

mi set mlong
mi misstable summarize psall
mi reshape wide cflisen psall, i(id) j(occasion)
*register variables with missing.
mi register imputed psall1 psall2
mi impute chained (regress) psall1 psall2 = sex, force add(2) rseed(4216) orderasis augment noimputed
mi reshape long cflisen psall, i(id) j(occasion)

mi xtset id occasion
mi estimate, eform: xtgee cflisen c.psall, family(binomial) link(logit) corr(exch) vce(robust)
*melogit not supported by mi:estimate.























