ssc install estout


clear
set seed 12345
set obs 10000

*--------------------------------------------------
* 1. Create synthetic survey data
*--------------------------------------------------
gen id = _n

* Sex: 0 = female, 1 = male
gen sex = runiform() > 0.5
label define sexlbl 0 "Male" 1 "Female"
label values sex sexlbl

* Height (m) and weight (kg)
gen height = rnormal(1.65 + 0.10*sex, 0.08)
gen weight = rnormal(65 + 10*sex, 12)

* BMI
gen bmi = weight/(height^2)

* Survey design variables
gen psu     = ceil(_n/10)
gen strata  = mod(_n,5) + 1
gen weightp = runiform()*2 + 0.5

gen age = floor(runiform()*41) + 50
recode age (50/54 = 1) (55/59 = 2) (60/64 = 3) (65/69 = 4) (70/74 = 5) (75/79 = 6) (80/100 = 7), gen(agegrp)
tab1 agegrp

gen wealth = ceil(runiform()*5)
tab1 wealth

summ bmi
scalar a = r(max)

generate bmivg52=-2
replace bmivg52=1 if inrange(bmi,0,18.4999)
replace bmivg52=2 if inrange(bmi,18.500,24.9999)
replace bmivg52=3 if inrange(bmi,25.00,29.9999)
replace bmivg52=4 if inrange(bmi,30.00,39.9999)
replace bmivg52=5 if inrange(bmi,40.00,a)

gen overwt=-2
replace overwt=0 if inlist(bmivg52,1,2)
replace overwt=100 if inlist(bmivg52,3,4,5)

gen obese=-2
replace obese=0 if inlist(bmivg52,1,2,3)
replace obese=100 if inlist(bmivg52,4,5)

gen male=sex==0
gen female=sex==1

save "C:\Users\rmjdshc\OneDrive - University College London\Learning\Software\Stata\Making Documents\survey_data.dta", replace


***************
*BMI by sex
***************

use "https://github.com/shauns11/Learning/raw/refs/heads/main/survey_data.dta", clear

*--------------------------------------------------
* 2. Declare survey design
*--------------------------------------------------
svyset psu [pweight=weightp], strata(strata)

* Estimate mean BMI by sex
svy: mean bmi, over(sex)
* Extract coefficient vector
matrix b = e(b)'
matrix unwtd = e(_N)'
*create new matrix
matrix define A = b[1,1]\b[2,1]\unwtd[1,1]\unwtd[2,1]
matrix list A
*insert column to the left: need correct columns before applying border
matrix A = (J(rowsof(A), 1, .),A)
matrix list A

putdocx begin
putdocx table tableA = matrix(A), title("Table 1: Mean BMI by sex", bold halign(center)) note("Mean BMI by sex", italic halign(center)) border(start, nil) border(insideH, nil) border(insideV, nil) border(end, nil)
putdocx describe tableA  //


*format table.
putdocx table tableA(1,.), addrows(2,after)  // insert 2 rows after Title
putdocx table tableA(5,.), addrows(2,after)  // add 2 rows (after row 5); gap and "unwtd"
putdocx describe tableA  // 10*2


*format cells.
putdocx table tableA(4,1)= ("Men"), bold halign(left)
putdocx table tableA(5,1)= ("Women"), bold halign(left)
putdocx table tableA(7,1)= ("Unweighted N"), italic halign(left)
putdocx table tableA(8,1)= ("Men"), italic halign(left)
putdocx table tableA(9,1)= ("Women"), italic halign(left)
putdocx table tableA(2,2)= ("2024-24"), bold halign(center)
putdocx table tableA(3,2)= ("All"), bold halign(center)

*decimal places 
putdocx table tableA(4/5,2),nformat(%9.2f) halign(center)
putdocx table tableA(8/9,2),italic halign(center)

*borders
putdocx table tableA(1,.),border(bottom)
putdocx table tableA(10,.),border(top)
putdocx table tableA(2,2),border(bottom)
putdocx table tableA(3,2),border(bottom)

putdocx save "N:\Temp\Table 1.docx", replace


*************************
*Mean BMI by age and sex.
*************************

use "https://github.com/shauns11/Learning/raw/refs/heads/main/survey_data.dta", clear

*--------------------------------------------------
* 2. Declare survey design
*--------------------------------------------------
svyset psu [pweight=weightp], strata(strata)

svy: mean bmi, over(sex agegrp)
* Extract coefficient vector
matrix b1 = e(b)
matrix unwtd1 = e(_N)

svy: mean bmi, over(sex)
* Extract coefficient vector
matrix b2 = e(b)
matrix unwtd2 = e(_N)

*create new matrix
matrix B= b1[1,1..7],b2[1,1]\b1[1,8..14],b2[1,2]\unwtd1[1,1..7],unwtd2[1,1]\unwtd1[1,8..14],unwtd2[1,2]
matrix list B

*insert column to the left: need correct columns before applying border (4 * 9)
matrix B = (J(rowsof(B), 1, .),B)
matrix list B

putdocx begin
putdocx table tableB = matrix(B), title("Table 2: Mean BMI (kg/m-squared) by sex and age", bold halign(center)) note("Mean BMI by sex and age", italic halign(center)) border(start, nil) border(insideH, nil) border(insideV, nil) border(end, nil)
putdocx describe tableB  // (6 * 9) added title and note

*format table.
putdocx table tableB(1,.), addrows(2,after)  // insert 2 rows after Title
putdocx table tableB(5,.), addrows(2,after)  // add 2 rows (after row 5); gap and "unwtd"
putdocx describe tableB  // 10*9


*format cells.
putdocx table tableB(4,1)= ("Men"), bold halign(left)
putdocx table tableB(5,1)= ("Women"), bold halign(left)
putdocx table tableB(7,1)= ("Unweighted N"), italic halign(left)
putdocx table tableB(8,1)= ("Men"), italic halign(left)
putdocx table tableB(9,1)= ("Women"), italic halign(left)

putdocx table tableB(2,2)= ("Age in 2023-24"), bold halign(center)
putdocx table tableB(3,2)= ("50-54"), bold halign(center)
putdocx table tableB(3,3)= ("55-59"), bold halign(center)
putdocx table tableB(3,4)= ("60-64"), bold halign(center)
putdocx table tableB(3,5)= ("65-69"), bold halign(center)
putdocx table tableB(3,6)= ("70-74"), bold halign(center)
putdocx table tableB(3,7)= ("75-79"), bold halign(center)
putdocx table tableB(3,8)= ("80+"), bold halign(center)
putdocx table tableB(3,9)= ("All"), bold halign(center)


*decimal places 
putdocx table tableB(4/5,2/9),nformat(%9.1f) halign(center)
putdocx table tableB(8/9,2/9),italic halign(center)


*borders
putdocx table tableB(1,.),border(bottom)
putdocx table tableB(10,.),border(top)
putdocx table tableB(2,2/9),border(bottom)
putdocx table tableB(3,1/9),border(bottom)
putdocx table tableB(3,9),border(top, nil)

putdocx table tableB(2,2),colspan(7) halign(center)
putdocx table tableB(.,1),width(1)
             
putdocx save "N:\Temp\Table 2.docx", replace


*************************
*Mean BMI by wealth and sex.
*************************

use "https://github.com/shauns11/Learning/raw/refs/heads/main/survey_data.dta", clear

*--------------------------------------------------
* 2. Declare survey design
*--------------------------------------------------
svyset psu [pweight=weightp], strata(strata)

svy: mean bmi, over(sex wealth)
* Extract coefficient vector
matrix b = e(b)
matrix unwtd = e(_N)

*create new matrix
matrix B= b[1,1..5]\b[1,6..10]\unwtd[1,1..5]\unwtd[1,6..10]
matrix list B


*insert column to the left: need correct columns before applying border (4 * 6)
matrix B = (J(rowsof(B), 1, .),B)
matrix list B

putdocx begin
putdocx table tableB = matrix(B), title("Table 3: Mean BMI (kg/m-squared) by sex and wealth", bold halign(center)) note("Mean BMI by sex and wealth", italic halign(center)) border(start, nil) border(insideH, nil) border(insideV, nil) border(end, nil)
putdocx describe tableB  // (6 * 6) added title and note

*format table.
putdocx table tableB(1,.), addrows(2,after)  // insert 2 rows after Title
putdocx table tableB(5,.), addrows(2,after)  // add 2 rows (after row 5); gap and "unwtd"
putdocx describe tableB  // 10*6

*format cells.
putdocx table tableB(4,1)= ("Men"), bold halign(left)
putdocx table tableB(5,1)= ("Women"), bold halign(left)
putdocx table tableB(7,1)= ("Unweighted N"), italic halign(left)
putdocx table tableB(8,1)= ("Men"), italic halign(left)
putdocx table tableB(9,1)= ("Women"), italic halign(left)

putdocx table tableB(2,2)= ("wealth group in 2023-24"), bold halign(center)
putdocx table tableB(3,2)= ("Lowest"), bold halign(center)
putdocx table tableB(3,3)= ("2nd"), bold halign(center)
putdocx table tableB(3,4)= ("3rd"), bold halign(center)
putdocx table tableB(3,5)= ("4th"), bold halign(center)
putdocx table tableB(3,6)= ("Highest"), bold halign(center)

*decimal places 
putdocx table tableB(4/5,2/6),nformat(%9.1f) halign(center)
putdocx table tableB(8/9,2/6),italic halign(center)

*borders
putdocx table tableB(1,.),border(bottom)
putdocx table tableB(10,.),border(top)
putdocx table tableB(2,2/6),border(bottom)
putdocx table tableB(3,1/6),border(bottom)
putdocx table tableB(3,6),border(top, nil)


putdocx table tableB(2,2),colspan(5) halign(center)
putdocx table tableB(.,1),width(2)
             
putdocx save "N:\Temp\Table 3.docx", replace

********************
*put the tables in the same document.
********************

putdocx append "N:\Temp\Table 1.docx" "N:\Temp\Table 2.docx" "N:\Temp\Table 3.docx", saving("N:\Temp\Table 4.docx",replace)


erase "N:\Temp\Table 1.docx"
erase "N:\Temp\Table 2.docx"
erase "N:\Temp\Table 3.docx"
erase "N:\Temp\Table 4.docx"


*********************************************
*Full Table of BMI status by sex and age
*********************************************

use "https://github.com/shauns11/Learning/raw/refs/heads/main/survey_data.dta", clear

mvdecode bmivg52 overwt obese,mv(-2)

*--------------------------------------------------
* 2. Declare survey design
*--------------------------------------------------
svyset psu [pweight=weightp], strata(strata)

*Mean BMI by sex.
set cformat %9.3f
svy: mean bmi, over(sex) 
matrix A1 = e(b)
matrix A2 = r(table)[2,1..2]
matrix A3 = e(_N)
matrix A4 = e(_N_subp)

*Mean BMI by sex and age.
svy: mean bmi, over(sex agegrp)
matrix B1 = e(b)
matrix B2 = r(table)[2,1..14]
matrix B3 = e(_N)
matrix B4 = e(_N_subp)

*BMI categories by sex.
estpost svy: tabulate bmivg52 sex, col perc nomarg
matrix C1 = e(b)'
*BMI categories (male)
estpost svy,subpop(male): tabulate bmivg52 agegrp, col perc nomarg
*7 (age) columns with 5 rows 
matrix C2 = e(b)'
*BMI categories (female)
estpost svy,subpop(female): tabulate bmivg52 agegrp, col perc nomarg
*7 (age) columns with 5 rows 
matrix C3 = e(b)'

*% overwt+ by sex and age
svy: mean overwt, over(sex) 
matrix D1 = e(b)
svy: mean overwt, over(sex agegrp)
matrix D2 = e(b)

*% obese+ by sex and age
svy: mean obese, over(sex) 
matrix E1 = e(b)
svy: mean obese, over(sex agegrp)
matrix E2 = e(b)

*******************************************************************************************
*Put the estimates above into a matrix with 22 rows and 8 columns (7 agegroups; 1 total).
*[rows,columns]
* , indicates new column. / indicates new row.
*Men:
*(1) Mean BMI
*(2) SE
*(3..7) bmivg52
*(8) overwt
*(9) obese
*women:
*(10) Mean BMI
*(11) SE
*(12..16) bmivg52
*(17) overwt
*(18) obese
*base (unwtd)
*(19) men
*(20) women
*base (wtd)
*(21) men
*(22) women
**********************

matrix men = B1[1,1..7],A1[1,1] \ B2[1,1..7],A2[1,1] \          ///
C2[1..5,1],C2[6..10,1],C2[11..15,1],C2[16..20,1],C2[21..25,1],C2[26..30,1],C2[31..35,1],C1[1..5,1]  \ ///
D2[1,1..7],D1[1,1] \ ///
E2[1,1..7],E1[1,1] 

matrix women = B1[1,8..14],A1[1,2] \ B2[1,8..14],A2[1,2] \          ///
C3[1..5,1],C3[6..10,1],C3[11..15,1],C3[16..20,1],C3[21..25,1],C3[26..30,1],C3[31..35,1],C1[6..10,1]  \ ///
D2[1,8..14],D1[1,2] \ ///
E2[1,8..14],E2[1,2]

matrix base = B3[1,1..7],A3[1,1] \ B3[1,8..14],A3[1,2] \ B4[1,1..7],A4[1,1] \ B4[1,8..14],A4[1,2]

*full matrix we need.
mat A = men[1..9,1..8] \ women[1..9,1..8] \ base[1..4,1..8]

*insert column to the left: need correct columns before applying border
matrix A = (J(rowsof(A), 1, .),A)
matrix list A


putdocx begin
putdocx table tableA = matrix(A), title("Table 1: BMI by sex and age", bold halign(center)) note("BMI by sex and age", italic halign(center)) border(start, nil) border(insideH, nil) border(insideV, nil) border(end, nil)
putdocx describe tableA  //
*24 rows, 9 columns (1 row for title; 1 for note).

*add 4 rows (men; women; bases (unweighted); bases (wtd) before formatting.
putdocx table tableA(1,.), addrows(2,after)  
putdocx table tableA(12,.), addrows(1,after)  // take into account addition above
putdocx table tableA(22,.), addrows(1,after)  // take into account addition above
putdocx table tableA(25,.), addrows(1,after)  // take into account addition above

*decimal places 
putdocx table tableA(4,2/9),nformat(%9.1f) halign(center)        // mean BMI (men)
putdocx table tableA(5,2/9),nformat(%9.2f) halign(center)        // SE BMI (men)
putdocx table tableA(6/12,2/9),nformat(%9.0f) halign(center)        // BMI categories

putdocx table tableA(14,2/9),nformat(%9.1f) halign(center)        // mean BMI (men)
putdocx table tableA(15,2/9),nformat(%9.2f) halign(center)        // SE BMI (men)
putdocx table tableA(16/22,2/9),nformat(%9.0f) halign(center)        // BMI categories 

putdocx table tableA(24/25,2/9),nformat(%9.0f) halign(center)        // unwtd
putdocx table tableA(27/28,2/9),nformat(%9.0f) halign(center)        // wtd

*format cells.

putdocx table tableA(2,2)= ("50-54"), bold halign(center)
putdocx table tableA(2,3)= ("55-59"), bold halign(center)
putdocx table tableA(2,4)= ("60-64"), bold halign(center)
putdocx table tableA(2,5)= ("65-69"), bold halign(center)
putdocx table tableA(2,6)= ("70-74"), bold halign(center)
putdocx table tableA(2,7)= ("75-79"), bold halign(center)
putdocx table tableA(2,8)= ("80+"), bold halign(center)
putdocx table tableA(2,9)= ("All"), bold halign(center)


putdocx table tableA(3,1)= ("Men"), bold halign(left)
putdocx table tableA(4,1)= ("Mean BMI"), halign(left)
putdocx table tableA(5,1)= ("SE"), halign(left)
putdocx table tableA(6,1)= ("% underweight"), halign(left)
putdocx table tableA(7,1)= ("% normal"), halign(left)
putdocx table tableA(8,1)= ("% overweight"), halign(left)
putdocx table tableA(9,1)= ("% obese, excluding morbidly obese"), halign(left)
putdocx table tableA(10,1)= ("% morbidly obese"), halign(left)
putdocx table tableA(11,1)= ("Overweight, including obesity"), halign(left)
putdocx table tableA(12,1)= ("Obese"), halign(left)

putdocx table tableA(13,1)= ("Women"), bold halign(left)
putdocx table tableA(14,1)= ("Mean BMI"), halign(left)
putdocx table tableA(15,1)= ("SE"), halign(left)
putdocx table tableA(16,1)= ("% underweight"), halign(left)
putdocx table tableA(17,1)= ("% normal"), halign(left)
putdocx table tableA(18,1)= ("% overweight"), halign(left)
putdocx table tableA(19,1)= ("% obese, excluding morbidly obese"), halign(left)
putdocx table tableA(20,1)= ("% morbidly obese"), halign(left)
putdocx table tableA(21,1)= ("Overweight, including obesity"), halign(left)
putdocx table tableA(22,1)= ("Obese"), halign(left)

putdocx table tableA(23,1)= ("Unweighted bases"), italic halign(left)
putdocx table tableA(24,1)= ("Men"), halign(left)
putdocx table tableA(25,1)= ("Women"), halign(left)

putdocx table tableA(26,1)= ("Weighted bases"), italic halign(left)
putdocx table tableA(27,1)= ("Men"), halign(left)
putdocx table tableA(28,1)= ("Women"), halign(left)

putdocx table tableA(.,1), width(2%)

putdocx save "N:\Temp\Table 1.docx", replace
















































