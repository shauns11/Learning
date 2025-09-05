*************************************************.
**Mixed effects / multilevel logistic regression
**************************************************.

*https://www.stata.com/support/faqs/statistics/random-effects-versus-population-averaged/

global projdir "C:/Users/rmjdshc/OneDrive - University College London/Learning/Longitudinal/01. Binary outcomes/"
global data "${projdir}/data"
global outputs "${projdir}/outputs/Temp"

/*
A random intercept model predicts the probability of Y conditional on X, where the baseline probability is allowed to vary by level 2-units
A random slopes model allows the slopes of one or several predictor variables vary between level 2-units
In logistic regression, the level 1-variance depends on the predicted probability, which means there is heteroscedasticity by definition. 
It is not strictly possible therefore to quantify the proportion of level 1-variance explained by a predictor. 

Random intercept only

Very similar to the linear multilevel model. Differences are:
(1) we are predicting a probability (rather than the mean);
(2) we use the logit transformation;
(3) the logistic regression model is usually written without explicitly specifying an error term for level 1.

Null model
β0 (intercept): log odds of Y for someone with u0j = 0.
A Level-2 unit with u0j = 0 has about average **log odds** of Y, which is not the same as average probability.
The predicted value of the outcome for u0j = 0 cannot be interpreted as the population average.

Level-1 variance in a mixed effects LR

In contrast to linear multilevel model output, Stata gives no "Level 1 variance" for multilevel logistic regression.
This is because for binary outcomes, the level 1-variance depends on the probability π of the outcome.
Since π varies between individuals, there is no single value we can give the level 1-variance.
Pragmatically, the level 1-variance is often given an arbitrary fixed value (≈3.29) for every model, 
which means that it does not change even when important covariates are added to a model.
In multilevel logistic regression, we cannot make precise statements about percentages of variance reduction due to adding covariates (as we can in linear multilevel models). 
This is because in multilevel logistic regression the level 1-variance always stays fixed at 3.29.

*melogit Y x || L2 identifier :

β0 (intercept): log odds of Y for when X=0, in a 'typical' L2unit (with u0j = 0);
β1: difference in log odds of Y associated with a 1-unit difference in X, controlling for L2 unit;
𝝈𝒖𝟎𝟐 [var(_cons)]: (level-2 residual variance), after taking into account X.

The random intercept variance is not transformed in Stata output tables (remain on the log odds scale).


Random slopes in multilevel logistic regression

Whether the slope of a level 1-predictor variable depends on which level 2-unit it belongs to.

*melogit Y x1 x2 || L2: x1, covariance(unstructured)

A (non-zero) slope variance for x1 may indicate that the relationship between x1 and the probability of Y varies by L2 unit. 
In some L2 units, x1 matters more than in others. This is conceptually the same as an interaction (effect modification) between fixed effects (L2 by x1).

Testing multiple random effects parameters


Testing multiple random effects parameters via a likelihood ratio test (LRT) is not recommended. Generally the assumptions 
under which the LRT is valid are not met for such model comparisons.  
Rather than relying on statistical tests, it might be a good strategy to consider information criteria instead.
For example, to evaluate whether a random slope and the covariance between the random slope and intercept improves the model, 
we might look at information criteria such as AIC/BIC. 
*/

*Github location.
global Git_projdir "https://github.com/shauns11/Learning/raw/refs/heads/main/Longitudinal/Binary%20outcomes"
global Git_data "${Git_projdir}/Data/"
*github.
use "${Git_data}/Bangladesh.dta", clear
drop urban

*randomly generate an urban/rural variable which is the same across all observations in a district.
*Step 1: Generate one random value per group
gen double u = .
bysort district (district): replace u = runiform() if _n == 1
bysort district (district): replace u = u[1]
*Step 2: Convert to 0 or 1
gen urban = u < 0.5
tab1 urban

tab1 c_use /* Y */
tab1 urban /*L2-variable */

preserve
collapse (mean) rate = c_use ///
(mean) urban = urban, by(district)
graph box rate, over(urban) ///
ytitle("Contraceptive use rate") b1title("Urbanity")
table urban, statistic(mean rate) nototals
restore

*Empty model.
melogit c_use || district:, intmethod(laplace)

predict u0, reffects reses(u0se)
*level 2-residuals are on the log-odds scale (-∞ to +∞).

*Caterpillar plot
preserve
collapse (mean) u0 = u0 (mean) u0se = u0se, by(district)
sort u0
gen u0rank = _n
serrbar u0 u0se u0rank, scale(1.96) yline(0) ///
xtitle("Districts (ranked by residual)") /// 
ytitle("District-level residuals")
restore

*Add predictors.
summarize age 
melogit c_use age urban || district: , or
melogit c_use age urban || district: age, or cov(unstructured)
*Caterpillar plots of the random effects (intercepts and slopes)
predict r_slope r_int, reffects reses(slope_se int_se)


preserve
collapse (mean) r_int = r_int (mean) int_se = int_se (mean) ///
r_slope = r_slope (mean) slope_se = slope_se, by(district)
*plot for intercepts*/
sort r_int
gen intrank = _n
serrbar r_int int_se intrank, scale(1.96) yline(0) ///
xtitle("Districts (ranked by residual)") ytitle("District-level residuals")
*plot for slopes*/
sort r_slope
gen sloperank = _n
serrbar r_slope slope_se sloperank, scale(1.96) yline(0) ///
xtitle("District (ranked by slope)") ytitle("District-level age slopes")
restore

*Model comparison: with or without random slope

melogit c_use age urban || district: , or
estat ic
melogit c_use age urban || district: age, or cov(unstructured)
estat ic

*model assumptions


preserve
collapse (mean) r_slope = r_slope (mean) r_int = r_int, by(district)
hist r_int, normal frequency xtitle("Random intercepts")
hist r_slope, normal frequency xtitle("Random slopes")
restore

/*
Investigating logistic model calibration

Check whether model is well calibrated – that is, whether the model 
predicts the probability of Y=1 equally well for different groups of data. 

(1) Save the predicted values from a multilevel logistic regression; these are on the log-odds scale
(2) Transform the predicted log odds into predicted probabilities
(3) Divide the sample into deciles based on predicted probabilities
(4) For each decile group, calculate:
(i) 	the average predicted probability that Y=1.
(ii)	the observed proportion that Y=1.

Make a scatterplot of predicted probabilities versus observed proportions; 
if the model is well calibrated, the two values should be very similar for all groups
*/

melogit c_use age urban || district: age, or cov(unstructured)

predict yhat, fitted 
gen pred_prob = exp(yhat) / (1 + exp(yhat))
egen prob_dec = cut(pred_prob), group(10) label 
lab var prob_dec "Predicted probability decile"
table (prob_dec), statistic(frequency) statistic(mean pred_prob c_use) nototals
 
preserve
collapse (mean) observed = c_use (mean) predicted = pred_prob, by(prob_dec)
graph twoway (scatter observed predicted) (function y = x), xtitle("Predicted probability") ytitle("Observed proportion") legend(off)
restore


display ("Multilevel logit finished")











































