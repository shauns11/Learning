*(https://rpubs.com/mbounthavong/IPTW_MSM_Tutorial)

/*
*IPTW in longitudinal studies: time-dependent confounding in the setting of treatment-confounder feedback.

To adjust for confounding measured over time in the presence of treatment-confounder feedback, 
IPTW can be applied to appropriately estimate the parameters of a MSM. Weights are calculated at each time point as the
inverse probability of receiving an individual's exposure level, given an individual's previous exposure history, the previous values
of the time-dependent confounder and the baseline confounders.

Stabilised IPTW & MSM: Time-varying Tx and time-varying confounders (tvc)

Two approaches to handling confounders: (i) conditional and (ii) marginal approaches. 

Marginal approach (weighting):

The marginal approach uses weights to balance the confounders across the Tx groups. 
MSM for when exposures and confounders are time-varying: adjustment for these is critical for reducing bias. 
There are also time-dependent relationships between the confounders and exposures. Use IPTW fitted to a MSM.
The probability of treatment exposure varies across time, which requires application of time-varying weights.
IPTW are used to make the exposure at each time point independent of the confounders that occur beforehand 
and allow us to generate a causal interpretation between Tx and Y.

IPTW are weights assigned to each observation across time conditioned on previous exposure history, which are then multiplied
to generate a single weight for each subject. We model Tx status: previous exposure history is 
incorporated into the propensity score estimation, which is time-varying.


Standardised weights: 

(i) the time-varying confounders are in the denominator but not in the numerator;
(ii) time-invariant confounders are in both.

Numerator: the probability of the observed exposure at each time point conditioned on 
(i) the observed exposure history at the previous time-point, and 
(ii) the observed time-invariant confounders.

Denominator: the probability of the observed exposure at each time point conditioned on 
(i) the observed exposure history at the previous time point; 
(ii) the observed time-varying confounder history at the current time point and 
(iii) the time-invariant covariates.

NB: time-varying confounders at time t influence Tx decision at time t (confounders prior to Tx).

Some software cannot handle time-varying weights: a single weight needs to be estimated.
Created by multiplying all the standardised weights across the time points, which is then applied in a MSM. 
Assumptions are: 
(i) no unmeasured confounding; 
(ii) positivity; 
(iii) correct specification of the IPTW.

Estimate the propensity scores for each individual at each time point: the probability of receiving treatment.
Calculate the IPTW. These weights are used to adjust for confounding:









