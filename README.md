# coexistence IPM
This folder contains R code of a two-species integrated competition model in Nimble used to simulate data and fit the models to the simulated data

## coexistence_model.R

Simulate data using 4 sets of parameters and fit the data with IPM and 3 sets of priors
Change "parameterset" from 1 to 4 and "PRIOR"" ("EXP","LOGLOW", or "LOGHIGH")
One can also change sample sizes, which we did not do here.

Posterior samples are saved and used by the next script:

## output_coexistence.R
(change "parameterset" and "PRIOR" to get samples from the desired model)

Assess model convergence

Some 3 d plots to visualise density dependent effects

Compute invation criteria

Save summary results to be used by the next script:

## cross_scenarios_coexistence.R

Pool summary results from all parameter sets and prior sets.

Plot and save manuscript figures and tables