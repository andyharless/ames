So here's what I'm figuring to do:
1. Merge Ames data with OFHEO West North Central Housing Price Index (by year and month)
and then use log(SalesPrice/OFHEO)
as the dependent variable.  Also put log(OFHEO) on the right hand side.
As far as OLS coefficients go, this makes dividing
the dependent variable by OFHEO irrelevant, but it will force LASSO to pay attention.
2. Divide the official "training" data set into 3 parts: 60% for primary training,
20% for cross-validation, 20% for initial testing.
Do most subsequent exploratory analyses on primary training set.
3. Fit a simple OLS model with the most obvious variables along with neighborhood dummies.
Use the dummy coefficients to get a continuous neighborhood variable.
This is a way to avoid overfitting the dummies in subsequent regressions, but it
will also tend to give my "obvious variables" a foot in the door.
4. Decide which categorical variables will be represented by dummies and which will be
pseudo-continous, and transform them as necessary.
(Transformations to be done on the full data set, to avoid duplication of effort.)
5. Standardize variables and run a LASSO regression with everything but the kitchen sink.
(OK, if the dataset contains data on the kitchen sink, I'll include that too.)
Use performance on the cross-validation set to choose a LASSO parameter.
6. Repeat the process (including creation of a new continuous neighborhood variable)
on the primary training data plus cross-validation data, using the chosen LASSO parameter.
Test resulting model on initial testing set.  If it performs OK,
repeat the process on the whole official training set, predict on the official test set,
and submit the result as an initial Kaggle entry.
(But should I re-fit the LASSO parameter?  And if so to what?)
7. Go back and improve a bunch of stuff, e.g.
- Fix the psuedocontinuous variables, maybe by including over/under dummies
for each category and then deleting any that get the wrong sign.
- Think about the variables that LASSO liked and see if they really make sense.
Are there tweaks to the initial variable set that could cause the LASSO
to make smarter decisions?  Are some of the variables just stupid crap that shouldn't
even be there?
- Think about interactions, quadratic/polynomial terms, variable transformations, etc.
that might make sense given the
substantive character of the variables and their observed relationships.
8. Re-fit and re-submit predictions from the improved model.
9. Try other predictive techniques (RF, boosting, bagging, neural networks, etc.)
and maybe create an ensemble.
10. Whatever.
