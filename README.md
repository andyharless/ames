# ames
## Kaggle House Prices Competition

Others are welcome to clone/fork this repository or just copy my code. I request that you credit me where appropriate and inform me of outcomes from any resulting Kaggle submissions.

Due to randomness, bugfixes, interactive runs, etc., correspondence between repository scripts and Kaggle submissions is only approximate, and there are more submissions then there are scripts.

## Files:

- `house[`N`].R` = Primary script for main attempt N (blank=1)
- `house[`N`]_output.txt` = console output therefrom
- `house[`N`][`L`].R` = intermediate script L after main attempt N
- `train.csv` = Training data from Kaggle
- `test.csv` = Test data from Kaggle
- `data_description.txt` = Codebook from Kaggle
- `data_plan.xlsx` = Original plan for how to process variables
- `ofheowncnsa` = [FHFA (fka OFHEO)](https://www.fhfa.gov/DataTools/Downloads/pages/house-price-index.aspx) House Price Index data used in my analysis
- `plan.md` = Original plan for this analysis

## Results

| Version | Description | RMSE
----------|-------------|-----
| house.R | OLS for feature engnineering, GLMnet fitting model | 0.132
| house2.R | Ensemble including nonlinear models | 0.131
| house3.R | Ensemble with more feature engineering | 0.127
| house4.R | Ensemble of linear models only | 0.122

Leaderboard rank 731/2389, as of 2017-04-23 18:53 GMT

## Salient Features of My Approach
1. Force macro variable (OFHEO WNC house price index) to be included (by using it to normalize the target variable before including it as a predictor, thus preventing algorithms from excluding it, as they might have if it were *only* a predictor).
2. Use logarithms for continuous variables that can't go to zero (e.g. square feet of living space).
3. Recode many-valued categorical variables as continuous by taking the coefficients from their dummies in an otherwise sparse OLS.
4. Use arbitrary pseudocontinuous variables for ordered factors (at least as a first cut).

Here's [my original plan](plan.md) FWIW.

## Excuses After My First Submission
OK, so I submitted [my first Kaggle entry](https://www.kaggle.com/aharless).  Placed in the bottom half, but I don't feel too bad about that, because
- It was my very first entry
- I intentionally limited myself to linear models on the first try, even after seeing that others had had more success with tree models
- I didn't make any attempts to tune fitting algorithms, just chose the one with the best off-the-shelf results
- I didn't bother to use an ensemble of models or fitting methods, just chose my favorite from the linear options
- I cut corners in the interest of getting it done: e.g. treat ordered data as continuous without any attempt to find the right ratios
- I'm working on a single 2013-vintage MacBook Pro, can't compete with massive parallelism, and had to discard some fitting options because they're too slow
- The most successful models in this competition don't do dramatically better than typical entries
