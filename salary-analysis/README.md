# Predicting academic job salary
Project report available in [HTML (recommended)](https://github.com/lt2710/Pet-projects/blob/master/salary-analysis/slide-salary.html) and [PDF](https://github.com/lt2710/Pet-projects/blob/master/salary-analysis/slide-salary.pdf).

In addition, feel free to check [RMD](https://github.com/lt2710/Pet-projects/blob/master/salary-analysis/make-slide-salary.Rmd) file to make the analysis and [raw R codes](https://github.com/lt2710/Pet-projects/blob/master/salary-analysis/slide-salary-codes.R)

## Summary
### Data preparation:
 - No dedup/imputation performed
 - Construct new feature to avoid multicolinearity between yrs.since.phd and yrs.service
 - 8:2 train/test split, 5-fold CV inside the training set for hyperparameter tuning
 - Dummy code all factors; Add 2nd order polynomials on yrs.service; Normalize numeric features
### Modeling:
 - Salary (logged): a general penalized regression achieving 0.08 RMSE on test set.
 - Binary salary indicator: a CART achieving 0.67 accuracy on test set.
### Enhancement:
 - Questions: causal relationship between gender and salary, interaction salary effect of gender on seniority, subgroup detection
 - Additional attributes: citations, PhD at top schools, current academic instition rank, working hours, marriage status
 - Sample: size >=45, oversampling high salary observations
