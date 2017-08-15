# ames
## Kaggle House Prices Competition

This is the repository for [my entries](https://www.kaggle.com/aharless) in the [Kaggle House Price Competition](https://www.kaggle.com/c/house-prices-advanced-regression-techniques).

Others are welcome to clone/fork this repository or just copy my code. I request that you credit me where appropriate and inform me of outcomes from any resulting Kaggle submissions.

Due to randomness, bugfixes, interactive runs, etc., correspondence between repository scripts and Kaggle submissions is only approximate, and there are more submissions than there are scripts.  Also, the code in my related Kaggle kernels, some of which was used to produce submitted entries, may or may not correspond to what is here.  I have had to hobble some of the kernel scripts due to performance issues, so typically the best entries have been from code run on my own computer.


## Files:

- `house[`N`].R` = Primary script for main attempt N (blank=1)
- `house[`N`]_output.txt` = console output therefrom
- `house[`N`][`L`].R` = intermediate script L after main attempt N
- `house[`N`][`L`].Rmd` = R markdown version of R script
- `house[`N`][`L`].md` = markdown version of R markdown script
- `house[`N`][`L`].nb.html` = rendered version of R markdown script
- `house[`N`][`L`].ipynb` = Jupyter notebook version of R script
- `train.csv` = Training data from Kaggle
- `test.csv` = Test data from Kaggle
- `data_description.txt` = Codebook from Kaggle
- `data_plan.xlsx` = Original plan for how to process variables
- `ofheowncnsa` = [FHFA (fka OFHEO)](https://www.fhfa.gov/DataTools/Downloads/pages/house-price-index.aspx) House Price Index data used in my analysis
- `plan.md` = Original plan for this analysis
- `choudhary.ipynb` = Amit Choudhary's analysis in Python, with my annotations.

I haven't included output CSV files in this repository, since that would proabably violate contest rules.  You can approximate them by runinng the scripts, but I offer no guarantees as to how they will run on your system or how close the output will be to my actual submissions.

## Results

| Version | Description | RMSE
----------|-------------|-----
| `house.R` | OLS for feature engnineering, GLMnet fitting model | 0.132
| `house2.R` | Ensemble including nonlinear models | 0.131
| `house3.R` | Ensemble with more feature engineering | 0.127
| `house4.R` | Ensemble of linear models only | 0.122
| `house5.R` | Average with simple SVM prediction | 0.117
| `house6.R` | Drop outliers and change SVM parameter | 0.116
| `house7.R` | Average house6 result with Choudhary model | 0.112

Subsequently (2017-06-21) I took a weighted average of my output and the output from Xin-xing Chen_hust's ["have_a_try_2" Kaggle kernel](https://www.kaggle.com/cxxacxx/have-a-try-2) (which uses a combination of Lasso, XGBoost, and ElasticNet).  That script is written in Python, so could not be easily incorporated into my R code, so I just used Excel to take the weighted average (weight .8 for my house7 and .2 for have-a-try-2, just an offhand guess at reasonable weights).

And after that (2017-08-15) I tried averaging in Oleg Panichev's [Ensemble of 4 models](https://www.kaggle.com/opanichev/ensemble-of-4-models-with-cv-lb-0-11489), which didn't help, even though it had a RMSE of 0.115 on its own.  I kept reducing the weight, but a 2% weighting still gave a worse RMSE, I gave up.  Apparently the information in Oleg's ensemble is redundant to what is already in mine.

But then (still 2017-08-15) I averaged in 15% of Serigne's [Stacked Regressions](https://www.kaggle.com/serigne/stacked-regressions-top-4-on-leaderboard) with my June 21 submission (an 80/20 mix of a 50/50 mix of a 50/50 mix with my original ensemble), which produced a slight improvement.  At this point my ensemble of ensembles of ensembles of ensembles is undoubtedly overfitting the public leaderboard, but I guess we'll never know how much.  If I were going to do a final submission for a competition that was ending, I would raise the weight of my original ensemble, since it's the only component that hasn't been chosen for having already demonstrated good performance on the public leaderboard.  In my latest submission, my original ensemble has a weight of 0.17 (85% of 80% of 50% of 50%), which may be too small even to optimize the public leaderboard score, so maybe I will play with the weights in the future.

Leaderboard rank 8/1599, as of 2017-08-15 15:51 GMT

## Salient Features of My Approach
1. Force macro variable (OFHEO WNC house price index) to be included (by using it to normalize the target variable before including it as a predictor, thus preventing algorithms from excluding it, as they might have if it were *only* a predictor).
2. Use logarithms for continuous variables that can't go to zero (e.g. square feet of living space).
3. Recode many-valued categorical variables as continuous by taking the coefficients from their dummies in an otherwise sparse OLS.
4. ~Use arbitrary pseudocontinuous variables for ordered factors (at least as a first cut).~ Make ordered variables continuous by taking the coefficients from their dummies in an OLS (only using those dummy coefficients that turn out to be in the right order).

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
