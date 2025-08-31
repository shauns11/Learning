/*
Lasso, short for Least Absolute Shrinkage and Selection Operator, is a statistical method used in machine learning and statistical modeling. 
It performs both variable selection and regularization to enhance the prediction accuracy and interpretability of the resulting statistical model. 
Lasso regression introduces an additional penalty term based on the absolute values of the coefficients, which encourages sparse 
solutions where some coefficients are forced to be exactly zero. This feature makes Lasso particularly useful for feature selection, 
as it can automatically identify and discard irrelevant or redundant variables.

Estimating treatment effects in the potential outcome framework is a powerful tool for evaluating the effectiveness of a treatment based on observational data. 
However, in the presence of high-dimensional data, researchers often face a dilemma about how to build the model. 
On one hand, we want to have deep insights by making good use of large amounts of data. 
On the other hand, the more complex the model is, the more difficult it is to fit such a model. 
We want to include more variables in the model, but the traditional estimation techniques cannot fit such models.

(i) To resolve this conflict, we need to use model selection techniques such as lasso to select the variables that matter. 
(ii) At the same time, we want our estimator to be robust to model selection mistakes. In other words, we want our estimation results to still be valid 
even if lasso omits some important variables or includes some extra variables.

*telasso: to estimate treatment effects with many control variables and be robust to model selection mistakes. 

Through an example that compares two types of lung transplants, we will illustrate the dilemma or conflict of including many variables 
in the treatment-effects estimation and show how to use telasso to reconcile this conflict.

Suppose we want to compare two types of lung transplants. 
Bilateral lung transplant (BLT) is usually associated with a higher death rate in the short-term after the operation 
but with a more significant improvement in life quality than the single lung transplant (SLT). 
As a result, for patients who need to decide between these two treatment options, knowing the effect of BLT (versus SLT) on quality of life is essential. 

The outcome (fev1p) is FEV1% measured one year after the operation. 
The treatment variable (transtype) indicates whether the treatment is BLT or SLT.
*/

use https://www.stata-press.com/data/r17/lung, clear

/*
In addition to our treatment and outcome variables, we have 29 variables that record characteristics of the patients and donors. 
To construct control variables, we want to use these 29 variables and the interactions among them. 
Distinguish between continuous and categorical variables. 
-vl- is a suite of commands that simplifies this process (manages variable lists). 
First, we use vl set to partition the variables into continuous and categorical variables automatically. 
The global macro $vlcategorical contains all the categorical variable names; $vlcontinuous contains all the continuous variable names.
*/

quietly vl set
display `"$vlcategorical"'
display `"$vlcontinuous"'

/*
Second, we use -vl create- to create customized variable lists. Specifically:
$cvars contains all the continuous variables except the outcome (fev1p)
$fvars contains all the categorical variables except the treatment (transtype). 
*/

vl create cvars = vlcontinuous - (fev1p)
vl create fvars = vlcategorical - (transtype)

/*
Finally, -vl sub- substitutes the global macro $allvars with the full second-order interaction between the continuous variables in $cvars and categorical variables in $fvars. 
We will use $allvars as the control variables for both the outcome model and the treatment model.
*/

vl sub allvars = c.cvars i.fvars c.cvars#i.fvars
display `"$allvars"'

*will not work.
capture noisily teffects aipw (fev1p $allvars) (transtype $allvars)


/*
teffects produces an error complaining that the overlap assumption has been violated. 
The overlap assumption means that each patient has a strictly positive probability of being treated or not treated. 
In other words, given any patient in the treatment group, the overlap assumption implies that we can find a similar patient in the control group. 
That is, there is an overlap between the treatment and control groups. In our example, including all of these controls violates the overlap assumption 
because some specific combination of values of the control variables appears in either the treatment group or the control group but not both. 
The more control variables there are, the more difficult it is to satisfy the overlap assumption.
The dilemma is that (i) including all the controls makes the model inestimable, but (ii) not including all of them renders our model too simple to approximate the reality.
*/

/*
use lasso to select variables in the treatment and outcome models and use the selected variables in the treatment-effects estimation.
We assume a linear outcome model and a logit treatment model. 
*/

telasso (fev1p $allvars) (transtype $allvars)

/*
it selects only 8 variables among the 454 control variables. So telasso selects only variables that matter.
More importantly, the estimator implemented in telasso is robust to the model selection mistakes made by lasso. 
Thus, the estimation results are still valid even if some important variables are not included or if some extra variables are included in them.
*/

*The estimation results can be interpreted as usual. 
*If all the patients were to choose BLT, the FEV1% is expected to be 38% higher than the 46% average expected if all patients were to choose an SLT.

telasso (fev1p $allvars) (transtype $allvars), pomeans
di (46.4938) + (37.51841)          // PO mean for BLT.
di (84.01221) - (46.4938)          // ATE (BLT vs SLT)


**Double machine learning**

/*
The estimates obtained above relied on a critical assumption of lasso, the sparsity assumption, which requires that 
only a small number of the potential covariates are in the "true" model. 
We can use a double machine learning technique to allow for more covariates in the true model. 
To do this, we add the xfold(5) option to split the sample into five groups and perform crossfitting, 
and we add the resample(3) option to repeat the cross-fitting procedure with three samples.
*/

set seed 12345671
telasso (fev1p $allvars) (transtype $allvars), xfolds(5) resample(3) nolog

*The estimated treatment effect is similar to the first telasso command reported, but the selected model included 16 controls (instead of 8). 
*The similarity of the estimates across the different specifications suggests that our first model did not violate the sparsity assumption.

di "Finished"






















