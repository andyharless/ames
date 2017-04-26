# House Price Prediciton with Linear Ensemble and SVM

Set this variable appropriately before running the rest:

```r
running_as_kaggle_kernel = FALSE
```

This approach uses a simple 50/50 logarithmic average of my linear predictor
and mtyxwp's simple support vector machine (copied from "svm_simple" kernel).
My linear predictor does quite a bit of pre-processing using ordinary least squares
as a tool and then feeds the resulting model into an ensemble of fitting methods,
with weights for combining the methods determined from the fit on a validation set.

Start by loading libraries.

```r
library(plyr)
library(caret)
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```r
library(Metrics)
library(parallel)
library(doParallel)
```

```
## Loading required package: foreach
```

```
## Loading required package: iterators
```

```r
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(Amelia)
```

```
## Loading required package: Rcpp
```

```
## ## 
## ## Amelia II: Multiple Imputation
## ## (Version 1.7.4, built: 2015-12-05)
## ## Copyright (C) 2005-2017 James Honaker, Gary King and Matthew Blackwell
## ## Refer to http://gking.harvard.edu/amelia/ for more information
## ##
```

```r
library(mice)
library(lattice)
library(rpart)
library(xgboost)
library(e1071)
```

Here are the linear fitting methods.  Many of these will be excluded from the final
predictions, if they don't seem to help with performance on the validation set.
And one will be excluded for performance reasons if running on Kaggle.


```r
modelnames = c("lars2",     # Least Angle Regression
               "cubist",    # Cubist Regression Tree
               "glmboost",  # Boosted Generalized Linear Model
               "glmnet",    # Generalized linear model via penalized maximum likelihood
               "lasso",     # Least absolute shrinkage & selection operator (L1 penalty)
               "bayesglm",  # Bayesian Generalized Linear Model
               "ridge",     # Ridge Regression (L2 penalty)
               "xgbLinear", # eXtreme Gradient Boosting, linear method
               "nnls",      # Non-Negative Least Squares
               "icr",       # Independent Component Regression
               "gbm")       # Stochastic Gradient Boosting

too_slow <- c("xgbLinear")
```

Read the data files, and make any environment-specific adjustments.

```r
if (running_as_kaggle_kernel) {
  trainfile <- "../input/train.csv"
  testfile <- "../input/test.csv"
  keep = ! (modelnames %in% too_slow)
  modelnames <- modelnames[ keep ]
} else {
  trainfile <- "train.csv"
  testfile <- "test.csv"
}
```


## Functions

For factors that have an explicit or implicit order (e.g. Poor, Fair, Good, etc.), I am going to recode them as integers at first, so the order doesn't get lost.  Also,
when I call this function, I will be careful to choose the ordering that is expected
(based on intuition, but usually obvious) to be positively associated with sale price.
You'll see why later.

```r
# Function to recode levels to numeric in specified order and add ".n" to name
recode <- function( df, var, lev ) { 
  to <- as.character( 0:(length(lev)-1) )
  newvar <- as.numeric( as.character( mapvalues(df[[var]], from=lev, to=to) ) )
  newname <- paste0(var,".n")
  df <- cbind( df, newvar )
  names(df)[ncol(df)] <- newname
  df[var] <- NULL
  df
}
```

Function for initial data cleaning.  (Much of this could be done more elegantly
and consistently, but I confess I didn't start out this project by making sure 
the data were clean, as I should have.)

```r
cleanData <- function( df ) {
  # Convert pseudo-numeric "type of dwelling" identifier to a factor
  df$MSSubClass <- as.factor( df$MSSubClass )

  # Deal with numeric variables that have missing values
  df$LotFrontage = as.character( df$LotFrontage )
  df$HasLotFrontage = ifelse( df$LotFrontage=="NA", 0, 1 )
  df$LotFrontage = ifelse( df$LotFrontage=="NA", "0", df$LotFrontage ) 
  df$LotFrontage = as.numeric( df$LotFrontage )

  df$MasVnrArea = as.character( df$MasVnrArea )
  df$HasMasVnr = ifelse( df$MasVnrArea=="NA", 0, 1 )
  df$MasVnrArea = ifelse( df$MasVnrArea=="NA", "0", df$MasVnrArea ) 
  df$MasVnrArea = as.numeric( df$MasVnrArea )

  df$GarageYrBlt = as.character( df$GarageYrBlt )
  df$HasGarageYr = ifelse( df$GarageYrBlt=="NA", 0, 1 )
  df$GarageYrBlt = ifelse( df$GarageYrBlt=="NA", "0", df$GarageYrBlt ) 
  df$GarageYrBlt = as.numeric( df$GarageYrBlt )

  # Dummy for "has basement"
  df$HasBasement = ifelse( df$BsmtQual=="NA", 0, 1 )

  # Recode ordered factors as pseudo-continuous numerical variables
  qualcats  = c( "Po",  "Fa",  "TA",   "Gd",   "Ex" )
  qualcats2 = c( "NA",  qualcats )
  funcats   = c( "Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ" )
  basecats  = c( "NA",  "Unf", "LwQ",  "Rec",  "BLQ", "ALQ",  "GLQ"         )
  df <- recode( df, "ExterCond",    qualcats  )
  df <- recode( df, "ExterQual",    qualcats  )
  df <- recode( df, "HeatingQC",    qualcats  )
  df <- recode( df, "KitchenQual",  qualcats  )
  df <- recode( df, "BsmtCond",     qualcats2 )
  df <- recode( df, "FireplaceQu",  qualcats2 )
  df <- recode( df, "GarageQual",   qualcats2 )
  df <- recode( df, "GarageCond",   qualcats2 )
  df <- recode( df, "Functional",   funcats   )
  df <- recode( df, "BsmtFinType1", basecats  )
  df <- recode( df, "BsmtFinType2", basecats  )
  df <- recode( df, "PavedDrive",   c("N",   "P",      "Y"                     ) )                                         
  df <- recode( df, "Utilities",    c("ELO", "NoSeWa", "NoSewr", "AllPub"      ) )
  df <- recode( df, "LotShape",     c("IR3", "IR2",    "IR1",    "Reg"         ) )                                         
  df <- recode( df, "BsmtExposure", c("NA",  "No",     "Mn",     "Av",    "Gd" ) )
  df <- recode( df, "PoolQC",       c("NA",  "Fa",     "TA",     "Gd",    "Ex" ) )
  df <- recode( df, "GarageFinish", c("NA",  "Unf",    "RFn",    "Fin"         ) )

  # BsmtHeight needs special treatment, since it's really a categorized continuous variable
  from <- c("NA", "Po", "Fa", "TA", "Gd", "Ex"  ) 
  to   <- c("0",  "50", "75", "85", "95", "120" )                                          
  df$BsmtHeight <- as.numeric( mapvalues(df$BsmtQual, from=from, to=to) )
  df$BsmtQual <- NULL

  # Fix numeric variables that will get read as factors in test set
  df$BsmtFinSF1 = as.numeric( as.character( df$BsmtFinSF1 ) )
  df$BsmtFinSF1[is.na(df$BsmtFinSF1)] = mean(df$BsmtFinSF1, na.rm=TRUE)
  df$BsmtFinSF2 = as.numeric( as.character( df$BsmtFinSF2 ) )
  df$BsmtFinSF2[is.na(df$BsmtFinSF2)] = mean(df$BsmtFinSF2, na.rm=TRUE)
  df$BsmtUnfSF = as.numeric( as.character( df$BsmtUnfSF ) )
  df$BsmtUnfSF[is.na(df$BsmtUnfSF)] = mean(df$BsmtUnfSF, na.rm=TRUE)
  df$BsmtFullBath = as.numeric( as.character( df$BsmtFullBath ) )
  df$BsmtFullBath[is.na(df$BsmtFullBath)] = mean(df$BsmtFullBath, na.rm=TRUE)
  df$BsmtHalfBath = as.numeric( as.character( df$BsmtHalfBath ) )
  df$BsmtHalfBath[is.na(df$BsmtHalfBath)] = mean(df$BsmtHalfBath, na.rm=TRUE)
  df$GarageCars = as.numeric( as.character( df$GarageCars ) )
  df$GarageCars[is.na(df$GarageCars)] = mean(df$GarageCars, na.rm=TRUE)
  df$GarageArea = as.numeric( as.character( df$GarageArea ) )
  df$GarageArea[is.na(df$GarageArea)] = mean(df$GarageArea, na.rm=TRUE)
  
  # Fix missing values
  df$Utilities.n[is.na(df$Utilities.n)] = 3       # Modal value
  df$Functional.n[is.na(df$Functional.n)] = 7     # Modal value
  df$KitchenQual.n[is.na(df$KitchenQual.n)] = 3   # Modal value
  df$Electrical[df$Electrical=="NA"] = as.factor("SBrkr")

  # Take logarithms where appropriate
  df$X1stFlrSF = log( df$X1stFlrSF )
  names(df)[names(df)=="X1stFlrSF"] <- "Ln1stFlrSF"
  df$GrLivArea = log( df$GrLivArea )
  names(df)[names(df)=="GrLivArea"] <- "LnLivArea"
  df$OFHEO = log( df$OFHEO )
  names(df)[names(df)=="OFHEO"] <- "LnOFHEO"

  # Normalize dependant variable (if this is a training set)  
  if (!is.null(df$SalePrice)) {
    df$SalePrice = log( df$SalePrice ) - df$LnOFHEO 
    names(df)[names(df)=="SalePrice"] <- "RelPrice"
  }

  df
}
```
OK, Wait!  We interrupt this function to bring you a special announcement!
That last section does something non-standard.  It normalizes the target
variable by dividing it by a house price index.  The new target variable
is called "RelPrice" and will have to be un-normalized at the end.  More on this issue
when I get to "Analysis" section of this notebook.

Now on to the fancy stuff.  The following function takes a baseline OLS model,
adds a factor to it, and returns the factor's OLS coefficients, to be used later
to recode the factor as a continuous variable.  (Note that the factor enters into
the OLS as an exhaustive set of dummies -- one for every level present -- because
the constant term is removed.)

```r
# Function to get coefficients to be used to make factor continuous given baseline model
getCoeffs <- function( df, basemodel, factor ) {
  mod <- paste0( basemodel, "+", factor, "-1" )
  lm <- lm(formula=mod, data=df)
  fnames <- grep( factor, names(lm$coefficients), fixed=TRUE )
  lm$coefficients[fnames]
}
```

...and the next function takes those dummy coefficients and uses them to make
the factor into a continuous variable:

```r
# Function to make factor continuous (given dummy coefficients) and add "_r" to name
makeContinuous <- function( df, factor, coeffs ) {
  outvar <- 0*(1:nrow(df))
  fact <- df[[factor]]
  for ( n in levels(fact) ) {
     outvar[fact==n] <- coeffs[paste0(factor,n)]
  }   
  df <- cbind( df, outvar )  
  names(df)[ncol(df)] <- paste0( factor, "_r" )
  df[factor] <- NULL
  df
}
```

That's how I process many-valued unordered factors.  As for the ordered factors,
I started out by making them into integers, but the following set of functions
makes them more genuinely continuous, using a procedure somewhat similar to
what I did with the unordered factors.  These next functions also apply to variables
that were originally presented as integers (e.g. 10-point scale) but don't
represent phenomena where the ratios of different values are meaningful.

As the first step, I have a function that takes an integer variable and creates
an above/below dummy for each of its values.

```r
# Function to convert integer variable to above/below dummies for each of its values
dummify <- function( df, var ) { 
  v <- df[[var]]
  vals <- sort( unique(v) )
  n <- length(vals) - 1
  if (n==0) { return (NULL) }
  for (i in vals[1:n]) {
    newname <- paste0(var,".gt",as.character(i))
    newvar <- as.numeric(v > i)
    df <- cbind( df, newvar )
    names(df)[ncol(df)] <- newname
  }
  df
}
```

As the second step, I have a function that adds those dummies to the model.
But it then removes any dummies whose coefficients fit with the wrong sign.

```r
# Function to add to model any of the created dummies that have positive coefficients
addDummiesToModel <- function( df, var, mod ) {
  vars <- c()
  for (name in names(df)) {
    if ( grepl(paste0(var,".gt"), name, fixed=TRUE) ) {
      vars <- c(vars,name)
    }
  }
  newmod <- paste0( mod, " + ", paste(vars, collapse=" + ") )
  lmfull <- lm( formula=newmod, data=df )
  count = length(vars)
  for (var in vars) {
    if ( is.na( lmfull$coefficients[var] ) ) {
      print( paste0( "Variable ", var, " removed due to rank deficiency"))
      newmod <- gsub( paste0(" + ",var), "", newmod, fixed=TRUE )
      count = count - 1
    }
    else if ( lmfull$coefficients[var] < 0 ) {
      newmod <- gsub( paste0(" + ",var), "", newmod, fixed=TRUE )
      count = count - 1
    }
  }
  if (count==0) { return(NULL) }
  newmod
}
```

The next function runs the resulting model and extracts the dummy coefficients.

```r
# Function to get coefficients to be used to make ordered variable continuous
#   (given a model that includes any dummies found to have positive coefficients)
getOrderedCoeffs <- function( df, newmod, var ) {
  lm1 <- lm(formula=newmod, data=df)
  fnames <- grep( var, names(lm1$coefficients), fixed=TRUE )
  coeffs <- lm1$coefficients[fnames]
  names(coeffs) <- gsub( paste0(var,".gt"), "", names(coeffs), fixed=TRUE )
  coeffs
}
```

But it would get verbose having to run the last three functions separately, so I have
another function that runs them all and checks the results.

```r
# Function to make coefficients to be used to make ordered variable continuous
makeOrderedCoeffs <- function( df, mod, var ) {
  df <- dummify( df, var )
  if (is.null(df)) { 
    print( paste0("dummify returned NULL for ", var) )
    return(NULL) 
  }
  mod <- addDummiesToModel( df, var, mod )
  if (is.null(mod)) {
    print("addDummiesToModel returned NULL")
    return(NULL) 
  }
  coeffs <- getOrderedCoeffs( df, mod, var )
  if (is.null(coeffs)) {
    print("getOrderedCoeffs returned NULL")
    return(NULL) 
  }
  coeffs
}
```

And finally, the next function uses the resulting coefficients to make
the integer variable into a continuous one.  (This often has to be done separately
so that coefficients generated from training data can be applied to test data.)

```r
# Function to make ordered variable continuous (given coefficients) and add "_r" to name
makeOrderedContinuous <- function( df, var, coeffs ) {
  outvar <- 0*(1:nrow(df))
  v <- df[[var]]
  outvar <- 0
  for (n in names(coeffs)) {
    outvar <- outvar + ifelse( v>as.numeric(n), coeffs[n], 0 )
  }
  df <- cbind( df, outvar )  
  names(df)[ncol(df)] <- paste0( var, "_r" )
  df[var] <- NULL
  df
}
```

But even with the functions I've already written, the whole process can get verbose,
and it may have to be done several times, so I have the following function
that goes through a bunch of ordered variables and makes them all continuous.
Note that, for each variable, it uses as a base model one that includes all of
the previously processed variables but not the ones yet to be processed (with one
exception -- well, two, but the second one would have been included either way).
I could have included the yet-to-be-processed variables as pseudocontinuous
integers, but I chose not to.  Or I could have just used the original baseline
model for each variable.  (This issue also came up with the many-valued factors.)
It's not clear that what I chose was the best choice.  But it's also not clear
that it wasn't the best choice.

```r
# Function to make a bunch of ordered variables continuous using functions above
makeOrderedVariablesContinuous <- function(df, mod, varlist) {

  # Note: Ordered variables should be defined so that positive coefficients are expected

  # Start with a baseline model.  For each ordered variable (in sequence, so it matters
  #   how the variables are arranged in the variable list), create above/below dummies
  #   for each level.  Delete any dummies with wrong sign.  Use OLS coefficients on
  #   remaining dummies to define a continuous version of the ordered variable. 
  
  # "OverallQual" and "OverallCond" need special treatment because they are already
  #    in the baseline model so must be removed before adding corresponding dummies.
  already_in_baseline_model <- c("OverallQual", "OverallCond")
  
  orderedCoeffs <- list() # List that will contain coefficients for ordered variables
  varsToDrop <- c() # List that will contain variable names dropped because all wrong sign
  i <- 0 
  for ( var in varlist ) {
    if ( var %in% already_in_baseline_model ) {
      mod <- gsub( paste0("+ ", var), "", mod, fixed=TRUE ) # Delete from model
    } 
    co <- makeOrderedCoeffs( df, mod, var )
    if ( is.null(co) ) {
      varsToDrop <- c(varsToDrop, var)
      df[var] <- NULL
    }
    else {
      df <- makeOrderedContinuous( df, var, co )
      mod <- paste0( mod, " + ", var, "_r")
      i <- i + 1
      orderedCoeffs[[i]] <- co
      names(orderedCoeffs)[[i]] <- var
    }
  }
  output <- list( df, mod, orderedCoeffs, varsToDrop )
  names(output) <- c("df", "mod", "coeffs", "drop")
  output
}
```

OK, I'm finished with the "feature generation" functions, but once they've run
there will be more data cleaning to do (which, again, one could probably do
more elegantly and robustly than I did).

```r
# Function to do final data cleaning after variables have been processed into features
finalCleaning <- function( df ) {

  # Fix numeric variables with missing values in test and/or validation set
  df$MSSubClass_r[is.na(df$MSSubClass_r)] = mean(df$MSSubClass_r, na.rm=TRUE)
  df$Exterior1st_r[is.na(df$Exterior1st_r)] = mean(df$Exterior1st_r, na.rm=TRUE)
  df$Exterior2nd_r[is.na(df$Exterior2nd_r)] = mean(df$Exterior2nd_r, na.rm=TRUE)
  df$Condition2_r[is.na(df$Condition2_r)] = mean(df$Condition2_r, na.rm=TRUE)
  
  # Collapse sale condition categories
  salecon <- as.character(df$SaleCondition)
  df$SaleMisc <- ifelse( salecon=="Family" | salecon=="Partial", 1, 0 )
  df$SaleAbnormal <- ifelse( salecon=="Abnorml", 1, 0 )
  df$SaleCondition <- NULL

  # Collapse sale type categories
  st <- as.character(df$SaleType)
  con <- c("Con", "ConLw", "ConLI", "ConLD")
  wd <- c("WD", "CWD", "VWD")
  df$Contract <- ifelse( st %in% con, 1, 0 )
  df$WrntyDeed <- ifelse( st %in% wd, 1, 0 )
  df$NewSale <- ifelse( st=="New", 1, 0 )
  df$SaleType <- NULL

  # Only one kind of building type seems to be different
  df$SingleFam <- ifelse( as.character(df$BldgType)=="1Fam", 1, 0 )
  df$BldgType <- NULL

  # It matters if you have a garage, but this is captured by "HasGarageYear"
  # It also matters if it's a real garage or just a car port, so:
  df$CarPort <- ifelse( as.character(df$GarageType)=="CarPort", 1, 0 )
  df$GarageType <- NULL

  # Residential vs. nonresidential seems to be only relevant aspect of zoning
  zo <- as.character(df$MSZoning)
  res_zone <- c( "FV", "RH", "RL", "RP", "RM" )
  df$Residential <- ifelse( zo %in% res_zone, 1, 0 )
  df$MSZoning <- NULL

  # Get rid of RoofMatl. It is an overfit dummy for one case.
  # Earlier analysis showed all levels got OLS coefficients that were
  # very significantly different from zero but not different from one another.
  # "ClyTile" was the omitted category and was only one case.
  df$RoofMatl <- NULL

  # Get rid of MiscFeature. Per earlier analysis, it's a mess. Don't want to deal with it.
  df$MiscFeature <- NULL

  # Factors that earlier analyses didn't like and too much of a pain in the neck to keep
  df$Fence <- NULL
  df$RoofStyle <- NULL
  df$Heating <- NULL

  # I didn't see any residual seasonal pattern, so:
  df$MoSold <- NULL

  # These nonlinearitiesn seem to matter
  df$LotFrontage2 <- df$LotFrontage^2
  df$SinceRemod <- df$YrSold - df$YearRemodAdd
  df$SinceRemod2 <- df$SinceRemod^2
  df$YrSold <- NULL
  df$YearRemodAdd <- NULL
  df$BsmtFinSF1sq <- df$BsmtFinSF1^2

  # The following turn out to be redundant. But may want to bring them back later.
  df$TotalBsmtSF <- NULL
  df$HasMasVnr <- NULL
  df$KitchenAbvGr_r <- NULL
  df$GarageCond.n_r <- NULL
  
  df
}
```

Phew!  So much for data processing functions.  Now on to the model fitting.

The following function fits the models using the "train" function from the "caret" package.

```r
# Function to fit formula "fo" using several fitting methods "models"
fitModels <- function( df, fo, models, runParallel, seed ) {

  if (runParallel) {
    # Set up for multiple cores
    cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
    registerDoParallel(cluster)
    }

  # Fit the models
  modelfits = list()
  for (m in models) {
    print ( paste("Training model:", m) )
    set.seed(seed)
    fit <- train( as.formula(fo), data=df, method=m )
    modelfits = c(modelfits, list(fit))
  }

  if (runParallel) {
    # Go back to sequential processing
    stopCluster(cluster)
    registerDoSEQ()
  }
  
  names(modelfits) <- models

modelfits 
}
```

And the following function uses those fits to make predictions.

```r
# Function to make predictions given several model fits
makePredictions <- function( df, modelfits, basepred, is_test ) {

  modelnames <- names(modelfits)
  rmses <- list()
  predicted <- list()
  for (fi in modelfits) {
    writeLines ( paste("\n\n\nPredicting for model:", fi[[1]]) )
    p <- predict(fi, newdata=df)
    if (!is.null(basepred)) {
      p[is.na(p)] <- basepred[is.na(p)]
      }
    predicted <- c(predicted, list(p))
    if (!is_test) {
      rmses <- c(rmses, rmse(df$RelPrice, p))
    }
  }
  names(predicted) <- modelnames
  if (!is_test) {
    names(rmses) <- modelnames
    print( rmses )
  }
  
  predicted
}
```

Finally, the following function uses the predictions (presumably from a cross-validation
set predicted with a model fitted on a training set) to create an ensemble of fitting
procedures.  It starts out by including all the procedures that have been used
to fit the models.  To generate the first candidate for a new set of predictions,
it runs a no-constant OLS with coefficients constrained to sum to one.
Then, one by one, it eliminates procedures with negative coefficients, until
it ends up with an OLS where all coefficients are positive.  That OLS then generates
the weights for the ensemble.  (I guess this is what machine learning people call
"stacking" an OLS on top of the other models.)

```r
# Function to choose an ensemble of models and weight them
chooseEnsemble <- function( df, predicted ) {

  preddf <- cbind( as.data.frame(predicted), df$RelPrice )
  colnames(preddf) <- c(modelnames, "actual")
  bestcoef = 0
  i = 1

  while ( bestcoef <= 0 ) {

    # Run full equation
    predeq <- lm(actual~.-1, data=preddf)
    summary( predeq )
    cof <- predeq$coefficients
    bestmod <- names( cof[order(cof,decreasing=TRUE)] )[i]
    i = i + 1

    # Force coefficients to sum to 1
    preddf2 <- preddf
    predmod <- "actual~-1"
    for (n in names(preddf2)) {
      preddf2[n] <- preddf2[n] - preddf[bestmod]
      if (!(n=="actual"||n==bestmod)) {
        predmod = paste0(predmod, "+", n)
      }
    }

    # Keep dropping variables until all coefficients are positive
    eq <- lm( predmod, data=preddf2 )
    while( min(eq$coefficients) < 0 ) {
      dropv <- names( which.min(eq$coefficients) )
      predmod <- gsub( paste0("+",dropv), "", predmod, fixed=TRUE)
      eq <- lm(predmod, data=preddf2)
    }

    # Calculate missing coefficient
    bestcoef = 1 - sum(eq$coefficients)
    names(bestcoef) <- bestmod
    weights <- c(eq$coefficients, bestcoef)
  }
  
  weights
}
```

We're almost done with functions.  The following is a function that I copied from
mtyxwp's "svm_simple" kernel.  It deals with missing values much more elegantly
than I do in my own cleaning functions.  (Since I'd already written and tested them,
I didn't want to risk introducing new bugs by replacing parts of them with this,
but maybe in a future version....)
This function is called in the last section of this notebook, where it starts
over from scratch to fit an SVM and then averages its result with the linear one.

```r
# mtyxwp's function to deal with missing data (copied from "svm_simple" kernel
deal_missing <- function(simpledf){
  for(i in 1:ncol(simpledf))
  {
    u <- simpledf[,i]
    if (is.numeric(u))
    {
      simpledf[is.na(u),i] <- median(simpledf[!is.na(u),i])
    } else
    {
      u[is.na(u)] <- "Not Available"
      simpledf[,i] <- as.factor(u)
    }
  }
  return(simpledf)
}
```

OK, so much for functions.  Now on to data.

## Data

For my analysis I use a price index taken from the Federal Housing Finance Agency
(which used to be called the Office of Federal Housing Enterprise Oversight,
and I use the old initials, mostly because I didn't know the name had changed
when I first wrote this code, but I also just like the old acronym better).
The data come from [FHFA's website](https://www.fhfa.gov/DataTools/Downloads/pages/house-price-index.aspx),
but I just cut and pasted the relevant portion into my program.  It refers
to the West North Central House Price Index, monthly from 2006 through 2010.
(Ames, Iowa, where the competition data are from, is in that region, and the
sales span that range of time.)

```r
OFHEO = c(209.32, 210.23, 211.68, 212.71, 214.37, 215.37,
          216.37, 216.22, 215.45, 214.48, 214.73, 211.92,
          212.23, 214.99, 215.82, 216.99, 217.89, 218.28,
          218.69, 216.78, 217.27, 212.78, 212.72, 211.6, 
          208.58, 208.62, 209.68, 210.28, 209.78, 210.87,
          209.68, 208.77, 206.08, 206.07, 200.51, 201.47,
          201.78, 204.24, 201.05, 203.8,  205.1,  206.55,
          205.27, 204.63, 203.47, 204.22, 202.74, 199.78,
          196.35, 197.64, 198.89, 202.13, 204.25, 204.61,
          200.13, 201.76, 198.03, 197.87, 195.11, 193.46 )

Year = c( 2006, 2006, 2006, 2006, 2006, 2006,
          2006, 2006, 2006, 2006, 2006, 2006,
          2007, 2007, 2007, 2007, 2007, 2007,
          2007, 2007, 2007, 2007, 2007, 2007,
          2008, 2008, 2008, 2008, 2008, 2008,
          2008, 2008, 2008, 2008, 2008, 2008,
          2009, 2009, 2009, 2009, 2009, 2009,
          2009, 2009, 2009, 2009, 2009, 2009,
          2010, 2010, 2010, 2010, 2010, 2010,
          2010, 2010, 2010, 2010, 2010, 2010 )

Month = c( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
           1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
           1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
           1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
           1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 )
ofheo <- data.frame( Month, Year, OFHEO )
```

Then I merge that data with the competition data

```r
rawdata <- read.csv(trainfile, na.strings="")
data1 <- merge(rawdata, ofheo, by.x=c("YrSold","MoSold"), by.y=c("Year","Month"))
data1 <- cleanData( data1 )
```

```
## The following `from` values were not present in `x`: Po
## The following `from` values were not present in `x`: Po
```

```
## The following `from` values were not present in `x`: Ex
```

```
## The following `from` values were not present in `x`: Sal
```

```
## The following `from` values were not present in `x`: ELO, NoSewr
```

```
## The following `from` values were not present in `x`: TA
```

```
## The following `from` values were not present in `x`: Po
```
...and I got me a training set

My approach was to divide these data into 3 subsets:
- "train1"   (60%)  for  primary training
- "validate" (20%)  for  cross-validation
- "testing"  (20%)  for  initial testing

```r
set.seed(999)
inTrain <- createDataPartition(y=data1$RelPrice, p=0.8, list=FALSE)
training <- data1[inTrain,]
testing <- data1[-inTrain,]
inTrain1 <- createDataPartition(y=training$RelPrice, p=0.75, list=FALSE)
train1 <- training[inTrain1,]
validate <- training[-inTrain1,]
```

## Initial Analysis and Feature Processing of Primary Training Data

I start by making a working copy of my initial training set.  (In retrospect,
I was probably being too careful, but for now, this is the code.)

```r
da <- train1
```

I'm going to come clean here.  I'm convinced that house prices (even in a place
like Ames, Iowa that's far from the craziness in New York and Tokyo) depend heavily
on broad market conditions.  For Heaven's sake, did we not all live through 2008?
I don't care what the training data say:  if you tried to sell a house in 2009,
even in Iowa, you didn't sell it for the same price you could have gotten in 2006.
In some ad hoc exploratory analysis I ran before writing this code, the data didn't
seem to like the OFHEO (FHFA, whatever) price index variable very much.
But I'm damned if I'm going to let some officious regularization procedure 
tell me it doesn't belong in the model.  Until you can show me test data where
the variable makes the predictions less accurate, it stays in the damn model!

Nonetheless, I'm going to employ regularization procedures, because that's how
you get good predictions.  So I'm forcing them to accept my OFHEO variable.  How?
But putting it on the left-hand side, in the denominator.  Of course it will also
be on the right-hand side, but it will need a coefficient on -1 to be effectively
removed from the model.  (I'm using logarithms, so it effectively has a coefficient
of -1 on the left-hand side, which would wash out if it also has one on the right-hand
side.)  The regularization procedures won't be able to take it out of the model,
because they only affect the right-hand side.  If they take it out of the right-hand
side, it will be reflected one-for-one in the predictions.

So here's my baseline model

```r
basemod <- "RelPrice ~ LnOFHEO + Ln1stFlrSF + LnLivArea + OverallQual + OverallCond"
```

Very simple, but it captures the most important stuff.  If I were in a big hurry,
I'd just use this model and be done with it.  No regularization or crazy-ass support
vector machines.  Just good, old-fashioned ordinary least squares with a small and
obvious set of variables.  It won't win any competitions, but it will generate pretty
good predictions with little effort.

The following wasn't in my original code, but just to make a point, I'm going to run this with my normalization taken out:

```r
simple <- lm( RelPrice+LnOFHEO ~ Ln1stFlrSF + LnLivArea + OverallQual + OverallCond, da)
summary( simple )
```

```
## 
## Call:
## lm(formula = RelPrice + LnOFHEO ~ Ln1stFlrSF + LnLivArea + OverallQual + 
##     OverallCond, data = da)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.48303 -0.09345  0.01713  0.12221  0.59770 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 6.691520   0.191104  35.015  < 2e-16 ***
## Ln1stFlrSF  0.262518   0.024654  10.648  < 2e-16 ***
## LnLivArea   0.323668   0.025874  12.509  < 2e-16 ***
## OverallQual 0.161864   0.005938  27.261  < 2e-16 ***
## OverallCond 0.028147   0.005759   4.888 1.21e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1885 on 871 degrees of freedom
## Multiple R-squared:  0.7761,	Adjusted R-squared:  0.7751 
## F-statistic:   755 on 4 and 871 DF,  p-value: < 2.2e-16
```
It explains well over 3/4 of the variance.  With 4 variables!

Just for the hell of it:

```r
rmse( validate$RelPrice+validate$LnOFHEO, predict(simple, validate) )
```

```
## [1] 0.1630441
```
Behold the power of OLS!

We can fight like feral dogs over the next 4 percentage points of RMSE,
but FWIW, these 4 variables do the heavy lifting.  The difference between simple OLS
and some of the best models in this competition might not be enough to pay
the realtor's commission on one of the houses we're predicting.

That said, some realtors make a damn good living on those few percentage points, 
so let's continue.

Process the many-valued factors.  (Don't worry about warnings that there are
missing levels.)

```r
# Make factors continuous and add continuous versions to baseline model one by one
factors <- c( "Neighborhood", "MSSubClass", "Condition1", "Exterior1st", "Condition2", 
              "Exterior2nd",  "LotConfig",  "Foundation")
mod <- basemod
coeffs <- list()
i <- 0 
for (f in factors) {    
  co <- getCoeffs( da, mod, f )
  i <- i + 1
  coeffs[[i]] <- co
  names(coeffs)[i] <- f
  da <- makeContinuous( da, f, co )
  mod <- paste0( mod, "+", f, "_r" ) 
}
```

We're up to 87% of the variance now (but the adjusted R-squared is not meaningful,
since my "make factors continuous" procedure stole a bunch of degrees of freedom
and hid them under the bed)

```r
# Show output of augmented model
print( summary( lm( formula=mod, data=da ) ) )
```

```
## 
## Call:
## lm(formula = mod, data = da)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.93322 -0.07945  0.00024  0.07383  0.50662 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    -2.065e-13  9.642e-01   0.000  1.00000    
## LnOFHEO        -8.445e-01  1.728e-01  -4.888 1.21e-06 ***
## Ln1stFlrSF      1.703e-01  1.991e-02   8.551  < 2e-16 ***
## LnLivArea       4.096e-01  2.044e-02  20.039  < 2e-16 ***
## OverallQual     8.743e-02  5.701e-03  15.336  < 2e-16 ***
## OverallCond     5.024e-02  4.563e-03  11.011  < 2e-16 ***
## Neighborhood_r  7.546e-01  4.965e-02  15.198  < 2e-16 ***
## MSSubClass_r    8.574e-01  1.383e-01   6.201 8.69e-10 ***
## Condition1_r    1.109e+00  2.190e-01   5.063 5.05e-07 ***
## Exterior1st_r   8.873e-01  1.518e-01   5.847 7.11e-09 ***
## Condition2_r    1.016e+00  2.289e-01   4.440 1.02e-05 ***
## Exterior2nd_r   9.085e-01  3.704e-01   2.453  0.01438 *  
## LotConfig_r     1.092e+00  4.203e-01   2.598  0.00954 ** 
## Foundation_r    1.000e+00  2.871e-01   3.483  0.00052 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1429 on 862 degrees of freedom
## Multiple R-squared:  0.8732,	Adjusted R-squared:  0.8713 
## F-statistic: 456.5 on 13 and 862 DF,  p-value: < 2.2e-16
```

Now we make the ordered variables continuous

```r
ordered <- c("OverallQual",    "OverallCond",  # 1st 2=special cases bcuz in baseline mod  
             "Functional.n", "Fireplaces",     "KitchenQual.n", 
             "BsmtExposure.n", "HeatingQC.n",  "Utilities.n",    "FullBath",  
             "HalfBath",       "GarageCars",   "BsmtFullBath",   "GarageQual.n", 
             "BsmtFinType1.n", "PavedDrive.n", "BsmtCond.n",     "GarageCond.n", 
             "FireplaceQu.n",  "ExterQual.n",  "TotRmsAbvGrd",   "LotShape.n", 
             "BsmtHalfBath",   "PoolQC.n",     "BsmtFinType2.n", "ExterCond.n", 
             "BedroomAbvGr",   "BsmtHeight",   "KitchenAbvGr",   "GarageFinish.n") 

out <- makeOrderedVariablesContinuous(da, mod, ordered)
```

```
## [1] "dummify returned NULL for Utilities.n"
## [1] "Variable GarageFinish.n.gt0 removed due to rank deficiency"
```

```r
da <- out$df
mod <- out$mod
orderedCoeffs <- out$coeffs
varsToDrop <- out$drop
```
Clean the data

```r
da = finalCleaning( da )
```
Explore the result by looking at an OLS with everything but the kitchen sink.  (Oh,
wait, maybe there is a variable about the kitchen sink.  I'll have to check the
data description again.)

```r
regmodel = lm( formula="RelPrice ~ .", data=da )
print( summary( regmodel ) )
```

```
## 
## Call:
## lm(formula = "RelPrice ~ .", data = da)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.61087 -0.05362 -0.00056  0.05735  0.39144 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        3.006e+00  1.062e+00   2.830 0.004774 ** 
## Id                -6.605e-07  8.832e-06  -0.075 0.940400    
## LotFrontage        1.509e-03  8.149e-04   1.852 0.064398 .  
## LotArea            1.680e-06  6.387e-07   2.631 0.008687 ** 
## StreetPave         4.972e-02  6.319e-02   0.787 0.431623    
## AlleyNA            1.844e-02  2.314e-02   0.797 0.425630    
## AlleyPave          4.828e-02  3.208e-02   1.505 0.132771    
## LandContourHLS     5.904e-02  2.845e-02   2.075 0.038321 *  
## LandContourLow     4.816e-02  3.688e-02   1.306 0.191967    
## LandContourLvl     4.208e-02  2.218e-02   1.898 0.058127 .  
## LandSlopeMod       3.299e-02  2.248e-02   1.468 0.142630    
## LandSlopeSev      -1.608e-01  5.257e-02  -3.059 0.002296 ** 
## HouseStyle1.5Unf   1.525e-02  4.576e-02   0.333 0.739077    
## HouseStyle1Story  -9.260e-03  2.544e-02  -0.364 0.715956    
## HouseStyle2.5Fin   1.541e-02  9.610e-02   0.160 0.872674    
## HouseStyle2.5Unf  -1.691e-02  4.622e-02  -0.366 0.714509    
## HouseStyle2Story  -1.261e-02  1.873e-02  -0.674 0.500823    
## HouseStyleSFoyer  -1.773e-03  3.666e-02  -0.048 0.961452    
## HouseStyleSLvl    -5.055e-03  2.895e-02  -0.175 0.861434    
## YearBuilt          4.011e-04  3.869e-04   1.037 0.300172    
## MasVnrTypeBrkFace  3.313e-03  3.947e-02   0.084 0.933131    
## MasVnrTypeNA      -2.679e-02  6.285e-02  -0.426 0.670067    
## MasVnrTypeNone     1.261e-02  3.981e-02   0.317 0.751515    
## MasVnrTypeStone    1.135e-02  4.151e-02   0.273 0.784668    
## MasVnrArea         4.663e-05  3.329e-05   1.401 0.161715    
## BsmtFinSF1         1.808e-04  3.733e-05   4.842 1.55e-06 ***
## BsmtFinSF2         1.047e-04  3.349e-05   3.128 0.001825 ** 
## BsmtUnfSF          4.175e-05  2.547e-05   1.639 0.101531    
## CentralAirY        2.809e-02  1.969e-02   1.427 0.154037    
## ElectricalFuseF    7.406e-03  3.289e-02   0.225 0.821893    
## ElectricalFuseP    2.627e-02  8.674e-02   0.303 0.762097    
## ElectricalMix      2.089e-01  1.231e-01   1.697 0.090061 .  
## ElectricalSBrkr   -1.262e-02  1.621e-02  -0.778 0.436585    
## Ln1stFlrSF         5.728e-02  5.416e-02   1.058 0.290553    
## X2ndFlrSF         -1.681e-05  3.945e-05  -0.426 0.670127    
## LowQualFinSF       1.510e-04  1.158e-04   1.305 0.192379    
## LnLivArea          3.509e-01  6.458e-02   5.433 7.40e-08 ***
## GarageYrBlt       -2.766e-04  3.243e-04  -0.853 0.393919    
## GarageArea         9.026e-05  3.941e-05   2.290 0.022281 *  
## WoodDeckSF         1.144e-04  3.466e-05   3.302 0.001003 ** 
## OpenPorchSF        1.489e-04  6.767e-05   2.200 0.028069 *  
## EnclosedPorch      1.499e-04  7.091e-05   2.114 0.034843 *  
## X3SsnPorch         3.410e-04  1.273e-04   2.678 0.007556 ** 
## ScreenPorch        2.387e-04  6.747e-05   3.538 0.000427 ***
## PoolArea          -1.176e-05  1.074e-04  -0.110 0.912783    
## MiscVal           -3.575e-06  5.906e-06  -0.605 0.545157    
## LnOFHEO           -9.799e-01  1.351e-01  -7.255 9.73e-13 ***
## HasLotFrontage    -8.413e-02  3.553e-02  -2.368 0.018145 *  
## HasGarageYr        5.379e-01  6.317e-01   0.852 0.394737    
## HasBasement       -6.042e-02  5.543e-02  -1.090 0.276037    
## Neighborhood_r     4.231e-01  4.792e-02   8.830  < 2e-16 ***
## MSSubClass_r       2.592e-02  1.986e-01   0.131 0.896203    
## Condition1_r       1.350e+00  1.747e-01   7.731 3.29e-14 ***
## Exterior1st_r      5.007e-01  1.222e-01   4.098 4.59e-05 ***
## Condition2_r       1.232e+00  1.788e-01   6.889 1.16e-11 ***
## Exterior2nd_r      2.816e-01  3.010e-01   0.935 0.349862    
## LotConfig_r        8.800e-01  3.405e-01   2.585 0.009932 ** 
## Foundation_r       7.576e-01  2.852e-01   2.656 0.008067 ** 
## OverallQual_r      6.473e-01  5.970e-02  10.842  < 2e-16 ***
## OverallCond_r      6.856e-01  8.595e-02   7.976 5.38e-15 ***
## Functional.n_r     1.075e+00  1.506e-01   7.135 2.22e-12 ***
## Fireplaces_r       7.131e-01  2.438e-01   2.925 0.003543 ** 
## KitchenQual.n_r    2.183e-01  1.744e-01   1.251 0.211225    
## BsmtExposure.n_r   5.792e-01  1.814e-01   3.193 0.001462 ** 
## HeatingQC.n_r      8.475e-01  2.809e-01   3.018 0.002630 ** 
## FullBath_r         6.087e-01  3.246e-01   1.875 0.061129 .  
## HalfBath_r         7.264e-01  2.775e-01   2.618 0.009023 ** 
## GarageCars_r       5.436e-01  2.036e-01   2.669 0.007760 ** 
## BsmtFullBath_r     3.384e-01  1.889e-01   1.791 0.073685 .  
## GarageQual.n_r     6.567e-01  2.569e-01   2.556 0.010766 *  
## BsmtFinType1.n_r   3.826e-01  2.712e-01   1.411 0.158717    
## PavedDrive.n_r     1.209e+00  3.419e+00   0.354 0.723716    
## BsmtCond.n_r       1.301e+00  2.460e+00   0.529 0.597144    
## FireplaceQu.n_r    3.182e-01  6.621e-01   0.481 0.630993    
## ExterQual.n_r      8.626e-01  6.316e-01   1.366 0.172407    
## TotRmsAbvGrd_r     4.253e-01  3.010e-01   1.413 0.158043    
## LotShape.n_r      -4.126e-02  4.207e-01  -0.098 0.921902    
## BsmtHalfBath_r     3.159e-01  1.086e+00   0.291 0.771292    
## PoolQC.n_r         2.067e+00  6.735e-01   3.069 0.002221 ** 
## BsmtFinType2.n_r   1.863e+00  8.691e-01   2.144 0.032341 *  
## ExterCond.n_r      1.070e+00  1.099e+00   0.974 0.330471    
## BedroomAbvGr_r     6.838e-01  4.817e-01   1.420 0.156147    
## BsmtHeight_r       7.020e-01  8.446e-01   0.831 0.406140    
## GarageFinish.n_r   1.333e+00  1.283e+00   1.039 0.299062    
## SaleMisc           8.602e-03  2.844e-02   0.302 0.762374    
## SaleAbnormal      -4.481e-02  1.633e-02  -2.745 0.006190 ** 
## Contract           8.673e-02  3.971e-02   2.184 0.029233 *  
## WrntyDeed          1.332e-02  2.222e-02   0.600 0.548983    
## NewSale            7.665e-02  3.902e-02   1.964 0.049856 *  
## SingleFam          3.767e-02  1.851e-02   2.036 0.042131 *  
## CarPort           -1.893e-02  4.243e-02  -0.446 0.655552    
## Residential        2.010e-01  4.998e-02   4.021 6.35e-05 ***
## LotFrontage2      -5.834e-06  4.736e-06  -1.232 0.218411    
## SinceRemod        -1.845e-03  1.040e-03  -1.775 0.076322 .  
## SinceRemod2        1.389e-05  1.680e-05   0.826 0.408816    
## BsmtFinSF1sq      -6.091e-08  1.208e-08  -5.044 5.68e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1047 on 780 degrees of freedom
## Multiple R-squared:  0.9384,	Adjusted R-squared:  0.9308 
## F-statistic:   125 on 95 and 780 DF,  p-value: < 2.2e-16
```

## Quick and Dirty Look at Validation Set

Make a working copy.

```r
da2 <- validate
```

Apply the transformations that we got from the training set.

```r
for (f in factors) {    
  co <- coeffs[[f]]
  da2 <- makeContinuous( da2, f, co )
}
for ( var in ordered ) {
  if ( var %in% varsToDrop ) {
    da2[var] <- NULL
  }
  else {
    da2 <- makeOrderedContinuous( da2, var, orderedCoeffs[[var]] )
  }
}
da2 <- finalCleaning( da2 )
```

Predict using the "Kitchen Sink" OLS Model

```r
# Make predictions
prediction <- predict(regmodel, da2, type="response")

# Fill in missing values
baselm <- lm(formula=basemod, data=train1)
basepred <- predict( baselm, validate, type="response")
prediction[is.na(prediction)] <- basepred[is.na(prediction)]

# RMSE
rmse(da2$RelPrice,prediction)
```

```
## [1] 0.0968051
```
That's implausibly good.  Which means CARET's random assignment has given us
an unusually easy-to-predict
validation set.  (It also casts doubt on my earlier ranting about realtors and such,
but I think my basic point still stands.)

Also why I should use k-fold cross-validation in the future, but I'm going to continue
with this one for now, since the code is already written and tested.

Inspecting the earlier OLS output, I threw out the variables that looked like
they weren't helping, and this is what I came up with:

```r
fo = "RelPrice ~ LotFrontage + LotArea + Alley + LandContour + LandSlope "
fo = paste0(fo, "+ YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFinSF2 ")
fo = paste0(fo, "+ BsmtUnfSF + CentralAir + Ln1stFlrSF + X2ndFlrSF ")
fo = paste0(fo, "+ LowQualFinSF + LnLivArea + GarageArea + WoodDeckSF ")
fo = paste0(fo, "+ OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch ")
fo = paste0(fo, "+ LnOFHEO + HasLotFrontage + Neighborhood_r + Condition1_r ")
fo = paste0(fo, "+ Exterior1st_r + Condition2_r + Exterior2nd_r + LotConfig_r ")
fo = paste0(fo, "+ Foundation_r + OverallQual_r + OverallCond_r + Functional.n_r ")
fo = paste0(fo, "+ Fireplaces_r + KitchenQual.n_r + BsmtExposure.n_r + HeatingQC.n_r ")
fo = paste0(fo, "+ FullBath_r + HalfBath_r + GarageCars_r + BsmtFullBath_r ")
fo = paste0(fo, "+ GarageQual.n_r + BsmtFinType1.n_r + ExterQual.n_r + TotRmsAbvGrd_r ")
fo = paste0(fo, "+ PoolQC.n_r + BsmtFinType2.n_r + ExterCond.n_r + SaleAbnormal ")
fo = paste0(fo, "+ Contract + NewSale + SingleFam + Residential + LotFrontage2 ")
fo = paste0(fo, "+ SinceRemod + SinceRemod2 + BsmtFinSF1sq ")

mymodel = lm( formula=fo, data=da )
prediction <- predict(mymodel, da2, type="response")
prediction[is.na(prediction)] <- basepred[is.na(prediction)]
rmse(da2$RelPrice,prediction)
```

```
## [1] 0.09660811
```
Not a big improvement, but, whatev.


# Choosing an Ensemble of Models

Fit the models.  (Go have lunch while this section runs, if you're using Kaggle or
have a slow computer.)

```r
modelfits <- fitModels( da, fo, modelnames, !running_as_kaggle_kernel, 998 )
```

```
## [1] "Training model: lars2"
```

```
## Loading required package: lars
```

```
## Loaded lars 1.2
```

```
## [1] "Training model: cubist"
```

```
## Loading required package: Cubist
```

```
## [1] "Training model: glmboost"
```

```
## Loading required package: mboost
```

```
## Loading required package: stabs
```

```
## This is mboost 2.7-0. See 'package?mboost' and 'news(package  = "mboost")'
## for a complete list of changes.
```

```
## 
## Attaching package: 'mboost'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     %+%
```

```
## [1] "Training model: glmnet"
```

```
## Loading required package: glmnet
```

```
## Loading required package: Matrix
```

```
## Loaded glmnet 2.0-5
```

```
## 
## Attaching package: 'glmnet'
```

```
## The following object is masked from 'package:Metrics':
## 
##     auc
```

```
## [1] "Training model: lasso"
```

```
## Loading required package: elasticnet
```

```
## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
## trainInfo, : There were missing values in resampled performance measures.
```

```
## [1] "Training model: bayesglm"
```

```
## Loading required package: arm
```

```
## Loading required package: MASS
```

```
## Loading required package: lme4
```

```
## 
## arm (Version 1.9-3, built: 2016-11-21)
```

```
## Working directory is /Users/andy/Documents/workspace/learning/courses/courseraScale/houseProj/ames
```

```
## [1] "Training model: ridge"
```

```
## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
## trainInfo, : There were missing values in resampled performance measures.
```

```
## [1] "Training model: xgbLinear"
## [1] "Training model: nnls"
```

```
## Loading required package: nnls
```

```
## [1] "Training model: icr"
```

```
## Loading required package: fastICA
```

```
## [1] "Training model: gbm"
```

```
## Loading required package: gbm
```

```
## Loading required package: survival
```

```
## 
## Attaching package: 'survival'
```

```
## The following object is masked from 'package:caret':
## 
##     cluster
```

```
## Loading required package: splines
```

```
## Loaded gbm 2.1.1
```

```
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        0.1399             nan     0.1000    0.0170
##      2        0.1241             nan     0.1000    0.0140
##      3        0.1108             nan     0.1000    0.0134
##      4        0.0985             nan     0.1000    0.0115
##      5        0.0891             nan     0.1000    0.0086
##      6        0.0813             nan     0.1000    0.0072
##      7        0.0737             nan     0.1000    0.0071
##      8        0.0671             nan     0.1000    0.0061
##      9        0.0612             nan     0.1000    0.0053
##     10        0.0567             nan     0.1000    0.0037
##     20        0.0308             nan     0.1000    0.0011
##     40        0.0178             nan     0.1000    0.0000
##     60        0.0137             nan     0.1000    0.0000
##     80        0.0119             nan     0.1000   -0.0000
##    100        0.0107             nan     0.1000   -0.0000
##    120        0.0099             nan     0.1000   -0.0000
##    140        0.0091             nan     0.1000   -0.0000
##    150        0.0088             nan     0.1000   -0.0000
```

Create the ensemble.

```r
predicted <- makePredictions ( da2, modelfits, basepred, FALSE )
```

```
## 
## 
## 
## Predicting for model: lars2
## 
## 
## 
## Predicting for model: cubist
## 
## 
## 
## Predicting for model: glmboost
## 
## 
## 
## Predicting for model: glmnet
## 
## 
## 
## Predicting for model: lasso
## 
## 
## 
## Predicting for model: bayesglm
## 
## 
## 
## Predicting for model: ridge
## 
## 
## 
## Predicting for model: xgbLinear
## 
## 
## 
## Predicting for model: nnls
## 
## 
## 
## Predicting for model: icr
## 
## 
## 
## Predicting for model: gbm
## $lars2
## [1] 0.09593734
## 
## $cubist
## [1] 0.09149632
## 
## $glmboost
## [1] 0.1094718
## 
## $glmnet
## [1] 0.09611418
## 
## $lasso
## [1] 0.1063246
## 
## $bayesglm
## [1] 0.09619938
## 
## $ridge
## [1] 0.09660811
## 
## $xgbLinear
## [1] 0.1143767
## 
## $nnls
## [1] 0.112101
## 
## $icr
## [1] 0.1517753
## 
## $gbm
## [1] 0.1049162
```

```r
weights <- chooseEnsemble( da2, predicted )
bestmodels <- names(weights)
```

Take a look at the ensemble results for the validation set.  (Note that these are
in-sample predictions from the point of view of the ensemble-maker, so take with
a grain of salt.)

```r
preddf <- cbind( as.data.frame(predicted), da2$RelPrice )
colnames(preddf) <- c(modelnames, "actual")
p_ensemble <- as.data.frame( as.matrix(preddf[bestmodels]) %*% weights )
names( p_ensemble ) <- "ensemble"
rmse( da2$RelPrice, p_ensemble )
```

```
## [1] 0.0863883
```

# Initial Testing

Redo the feature creation process on data that include the validation set

```r
# Make working copy of data
da3 <- training

# Make factors continuous
mod <- basemod
coeffs <- list()
i <- 0 
for (f in factors) {    
  co <- getCoeffs( da3, mod, f )
  i <- i + 1
  coeffs[[i]] <- co
  names(coeffs)[i] <- f
  da3 <- makeContinuous( da3, f, co )
  mod <- paste0( mod, "+", f, "_r" ) 
}

# Make ordered variables continuous
out <- makeOrderedVariablesContinuous(da3, mod, ordered)
```

```
## [1] "dummify returned NULL for Utilities.n"
```

```r
da3 <- out$df
mod <- out$mod
orderedCoeffs <- out$coeffs
varsToDrop <- out$drop

# Make data nice
da3 = finalCleaning( da3 )
```

Process the initial test data accordingly

```r
# Make working copy of data
da4 <- testing

# Make factors continuous and add continuous versions to baseline model one by one
for (f in factors) {    
  co <- coeffs[[f]]
  da4 <- makeContinuous( da4, f, co )
}

# Make ordered variables continuous
for ( var in ordered ) {
  if ( var %in% varsToDrop ) {
    da4[var] <- NULL
  }
  else {
    da4 <- makeOrderedContinuous( da4, var, orderedCoeffs[[var]] )
  }
}

# Make data nice
da4 <- finalCleaning( da4 )
```

Fit models.  (Go have a snack.)

```r
modelfits <- fitModels( da3, fo, bestmodels, !running_as_kaggle_kernel, 997 )
```

```
## [1] "Training model: lars2"
## [1] "Training model: bayesglm"
## [1] "Training model: xgbLinear"
## [1] "Training model: cubist"
```

Make individual model predictions.

```r
predicted <- makePredictions ( da4, modelfits, NULL, FALSE )
```

```
## 
## 
## 
## Predicting for model: lars2
## 
## 
## 
## Predicting for model: bayesglm
## 
## 
## 
## Predicting for model: xgbLinear
## 
## 
## 
## Predicting for model: cubist
## $lars2
## [1] 0.1376046
## 
## $bayesglm
## [1] 0.1382953
## 
## $xgbLinear
## [1] 0.1463091
## 
## $cubist
## [1] 0.1408422
```

Combine results into an ensemble prediction, and check performance.

```r
p_ensemble <- 0*da4$RelPrice
preddf <- as.data.frame( predicted )
p_ensemble <- as.data.frame( as.matrix(preddf[bestmodels]) %*% weights )
names(p_ensemble) <- "ensemble"

# Estimated forecast error
print( rmse( da4$RelPrice, p_ensemble ) )
```

```
## [1] 0.1346126
```

Not as good as I would have hoped, but maybe CARET gave us a hard-to-predict
initial test set, just as it gave us an easy-to-predict validation set.
Again, the future is k-fold.

# Training the Final Model

Feature creation again

```r
# Make working copy of data
da5 <- data1

# Make factors continuous
mod <- basemod
coeffs <- list()
i <- 0 
for (f in factors) {    
  co <- getCoeffs( da5, mod, f )
  i <- i + 1
  coeffs[[i]] <- co
  names(coeffs)[i] <- f
  da5 <- makeContinuous( da5, f, co )
  mod <- paste0( mod, "+", f, "_r" ) 
}

# Make ordered varibles continuous
out <- makeOrderedVariablesContinuous(da5, mod, ordered)
da5 <- out$df
mod <- out$mod
orderedCoeffs <- out$coeffs
varsToDrop <- out$drop

# Make data nice
da5 = finalCleaning( da5 )
```

Let's take a look to the kitchen sink regression for the full training set

```r
regmodel = lm( formula="RelPrice ~ .", data=da5 )
print( summary( regmodel ) )
```

```
## 
## Call:
## lm(formula = "RelPrice ~ .", data = da5)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.70744 -0.05370  0.00374  0.05775  0.49382 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        2.001e+00  7.798e-01   2.566 0.010385 *  
## Id                -3.271e-06  6.846e-06  -0.478 0.632844    
## LotFrontage        8.880e-04  4.089e-04   2.172 0.030054 *  
## LotArea            1.993e-06  4.156e-07   4.796 1.79e-06 ***
## StreetPave         7.956e-02  4.921e-02   1.617 0.106173    
## AlleyNA            4.558e-03  1.730e-02   0.263 0.792219    
## AlleyPave          4.735e-02  2.432e-02   1.947 0.051762 .  
## LandContourHLS     3.727e-02  2.181e-02   1.709 0.087690 .  
## LandContourLow     5.647e-03  2.654e-02   0.213 0.831534    
## LandContourLvl     1.941e-02  1.550e-02   1.252 0.210698    
## LandSlopeMod       3.244e-02  1.703e-02   1.905 0.056936 .  
## LandSlopeSev      -1.041e-01  4.044e-02  -2.575 0.010139 *  
## HouseStyle1.5Unf   5.383e-02  3.408e-02   1.579 0.114488    
## HouseStyle1Story   2.090e-02  1.840e-02   1.136 0.256102    
## HouseStyle2.5Fin  -6.752e-02  5.232e-02  -1.291 0.197078    
## HouseStyle2.5Unf  -1.184e-03  3.660e-02  -0.032 0.974192    
## HouseStyle2Story  -2.119e-02  1.387e-02  -1.528 0.126730    
## HouseStyleSFoyer   3.653e-02  2.733e-02   1.337 0.181493    
## HouseStyleSLvl     9.457e-03  2.076e-02   0.456 0.648762    
## YearBuilt          7.061e-04  2.781e-04   2.539 0.011222 *  
## MasVnrTypeBrkFace  2.093e-02  2.978e-02   0.703 0.482196    
## MasVnrTypeNA      -2.633e-03  4.866e-02  -0.054 0.956854    
## MasVnrTypeNone     3.088e-02  3.003e-02   1.028 0.303955    
## MasVnrTypeStone    3.409e-02  3.157e-02   1.080 0.280440    
## MasVnrArea         2.475e-05  2.437e-05   1.015 0.310074    
## BsmtFinSF1         1.977e-04  2.687e-05   7.360 3.17e-13 ***
## BsmtFinSF2         9.157e-05  2.491e-05   3.676 0.000247 ***
## BsmtUnfSF          5.730e-05  1.873e-05   3.059 0.002267 ** 
## CentralAirY        3.090e-02  1.533e-02   2.015 0.044083 *  
## ElectricalFuseF    5.762e-03  2.483e-02   0.232 0.816520    
## ElectricalFuseP   -4.293e-02  6.717e-02  -0.639 0.522863    
## ElectricalMix      3.311e-02  1.146e-01   0.289 0.772769    
## ElectricalSBrkr   -1.665e-02  1.271e-02  -1.309 0.190635    
## Ln1stFlrSF         1.047e-01  4.289e-02   2.441 0.014789 *  
## X2ndFlrSF          6.718e-05  3.040e-05   2.210 0.027286 *  
## LowQualFinSF       9.165e-05  8.146e-05   1.125 0.260734    
## LnLivArea          3.447e-01  5.021e-02   6.865 1.00e-11 ***
## GarageYrBlt       -2.275e-04  2.534e-04  -0.898 0.369537    
## GarageArea         7.486e-05  3.239e-05   2.311 0.020974 *  
## WoodDeckSF         8.227e-05  2.550e-05   3.226 0.001286 ** 
## OpenPorchSF        3.681e-05  4.894e-05   0.752 0.452133    
## EnclosedPorch      1.036e-04  5.338e-05   1.941 0.052521 .  
## X3SsnPorch         1.510e-04  9.750e-05   1.548 0.121758    
## ScreenPorch        2.468e-04  5.394e-05   4.576 5.17e-06 ***
## PoolArea           1.090e-04  8.747e-05   1.246 0.212805    
## MiscVal           -5.119e-06  5.792e-06  -0.884 0.376979    
## LnOFHEO           -8.418e-01  1.040e-01  -8.091 1.30e-15 ***
## HasLotFrontage    -4.963e-02  2.147e-02  -2.311 0.020975 *  
## HasGarageYr        4.442e-01  4.929e-01   0.901 0.367710    
## HasBasement       -9.223e-02  3.468e-02  -2.659 0.007921 ** 
## Neighborhood_r     4.103e-01  3.869e-02  10.604  < 2e-16 ***
## MSSubClass_r       6.070e-03  1.265e-01   0.048 0.961743    
## Condition1_r       1.118e+00  1.455e-01   7.683 2.95e-14 ***
## Exterior1st_r      5.205e-01  1.210e-01   4.300 1.83e-05 ***
## Condition2_r       1.109e+00  1.571e-01   7.060 2.64e-12 ***
## Exterior2nd_r      1.283e-01  4.538e-01   0.283 0.777402    
## LotConfig_r        8.878e-01  2.482e-01   3.577 0.000359 ***
## Foundation_r       5.336e-01  1.664e-01   3.207 0.001374 ** 
## OverallQual_r      6.104e-01  4.890e-02  12.482  < 2e-16 ***
## OverallCond_r      7.259e-01  6.342e-02  11.445  < 2e-16 ***
## Functional.n_r     9.300e-01  1.316e-01   7.067 2.51e-12 ***
## Fireplaces_r       5.793e-01  1.532e-01   3.782 0.000162 ***
## KitchenQual.n_r    4.829e-01  1.611e-01   2.998 0.002766 ** 
## BsmtExposure.n_r   5.266e-01  1.407e-01   3.742 0.000190 ***
## HeatingQC.n_r      8.328e-01  2.602e-01   3.200 0.001405 ** 
## Utilities.n_r      6.493e-01  3.180e-01   2.042 0.041374 *  
## FullBath_r         4.500e-01  3.592e-01   1.253 0.210519    
## HalfBath_r         6.947e-01  2.364e-01   2.938 0.003356 ** 
## GarageCars_r       4.878e-01  1.700e-01   2.870 0.004171 ** 
## BsmtFullBath_r     3.994e-01  1.397e-01   2.859 0.004310 ** 
## GarageQual.n_r     8.487e-01  2.584e-01   3.284 0.001049 ** 
## BsmtFinType1.n_r   2.238e-01  2.399e-01   0.933 0.351086    
## PavedDrive.n_r     1.098e+00  1.698e+00   0.647 0.517836    
## BsmtCond.n_r       1.948e+00  1.178e+00   1.654 0.098442 .  
## FireplaceQu.n_r    4.849e-01  3.506e-01   1.383 0.166964    
## ExterQual.n_r     -2.149e-01  1.364e+00  -0.158 0.874849    
## TotRmsAbvGrd_r     6.391e-01  3.794e-01   1.685 0.092270 .  
## LotShape.n_r      -2.018e-01  7.511e-01  -0.269 0.788270    
## BsmtHalfBath_r     4.880e-01  7.944e-01   0.614 0.539117    
## PoolQC.n_r         6.742e-01  7.903e-01   0.853 0.393742    
## BsmtFinType2.n_r   1.421e+00  5.079e-01   2.799 0.005202 ** 
## ExterCond.n_r      9.903e-01  1.276e+00   0.776 0.437816    
## BedroomAbvGr_r    -5.993e-02  1.085e+00  -0.055 0.955951    
## BsmtHeight_r       7.688e-01  3.062e-01   2.511 0.012158 *  
## GarageFinish.n_r   1.258e+00  1.058e+00   1.189 0.234537    
## SaleMisc          -4.647e-02  2.331e-02  -1.994 0.046370 *  
## SaleAbnormal      -6.339e-02  1.263e-02  -5.017 5.95e-07 ***
## Contract           7.068e-02  3.002e-02   2.355 0.018683 *  
## WrntyDeed         -7.201e-03  1.805e-02  -0.399 0.689941    
## NewSale            9.952e-02  3.197e-02   3.113 0.001890 ** 
## SingleFam          4.098e-02  1.235e-02   3.318 0.000931 ***
## CarPort           -1.439e-02  3.845e-02  -0.374 0.708355    
## Residential        2.962e-01  3.836e-02   7.720 2.23e-14 ***
## LotFrontage2      -3.301e-06  1.917e-06  -1.722 0.085286 .  
## SinceRemod        -1.011e-03  8.127e-04  -1.244 0.213784    
## SinceRemod2        5.674e-06  1.318e-05   0.431 0.666852    
## BsmtFinSF1sq      -6.892e-08  7.281e-09  -9.465  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1063 on 1363 degrees of freedom
## Multiple R-squared:  0.9339,	Adjusted R-squared:  0.9292 
## F-statistic: 200.5 on 96 and 1363 DF,  p-value: < 2.2e-16
```

Based on these results, I'm going to make some changes to the feature list

```r
fo <- gsub( "+ ExterQual.n_r ", "", fo, fixed=TRUE  )
fo <- gsub( "+ ExterCond.n_r ", "", fo, fixed=TRUE )
fo <- gsub( "+ PoolQC.n_r ", "", fo, fixed=TRUE )
fo <- paste0(fo, "+ FireplaceQu.n_r + BsmtHeight_r + SaleMisc ")

regmodel = lm( formula=fo, data=da5 )
print( summary( regmodel ) )
```

```
## 
## Call:
## lm(formula = fo, data = da5)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.70753 -0.05467  0.00312  0.05957  0.48807 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       1.911e+00  6.935e-01   2.756 0.005926 ** 
## LotFrontage       9.793e-04  3.989e-04   2.455 0.014220 *  
## LotArea           1.923e-06  3.911e-07   4.918 9.79e-07 ***
## AlleyNA           8.239e-03  1.702e-02   0.484 0.628345    
## AlleyPave         5.231e-02  2.400e-02   2.180 0.029459 *  
## LandContourHLS    4.849e-02  2.141e-02   2.265 0.023684 *  
## LandContourLow    2.190e-02  2.581e-02   0.848 0.396316    
## LandContourLvl    2.741e-02  1.511e-02   1.814 0.069828 .  
## LandSlopeMod      2.943e-02  1.661e-02   1.772 0.076680 .  
## LandSlopeSev     -1.041e-01  3.983e-02  -2.614 0.009050 ** 
## YearBuilt         8.864e-04  2.248e-04   3.943 8.43e-05 ***
## MasVnrArea        5.486e-06  1.850e-05   0.297 0.766826    
## BsmtFinSF1        1.846e-04  2.286e-05   8.074 1.45e-15 ***
## BsmtFinSF2        8.309e-05  2.279e-05   3.646 0.000276 ***
## BsmtUnfSF         3.870e-05  1.446e-05   2.677 0.007516 ** 
## CentralAirY       2.749e-02  1.404e-02   1.959 0.050354 .  
## Ln1stFlrSF        1.672e-01  3.483e-02   4.799 1.76e-06 ***
## X2ndFlrSF         6.208e-05  2.714e-05   2.287 0.022317 *  
## LowQualFinSF      6.543e-05  6.429e-05   1.018 0.308978    
## LnLivArea         2.905e-01  4.116e-02   7.058 2.65e-12 ***
## GarageArea        5.938e-05  3.035e-05   1.956 0.050648 .  
## WoodDeckSF        8.073e-05  2.509e-05   3.218 0.001323 ** 
## OpenPorchSF       3.378e-05  4.756e-05   0.710 0.477741    
## EnclosedPorch     1.362e-04  5.204e-05   2.618 0.008945 ** 
## X3SsnPorch        1.570e-04  9.696e-05   1.620 0.105543    
## ScreenPorch       2.441e-04  5.307e-05   4.599 4.63e-06 ***
## LnOFHEO          -8.259e-01  1.026e-01  -8.050 1.76e-15 ***
## HasLotFrontage   -5.337e-02  2.097e-02  -2.545 0.011027 *  
## Neighborhood_r    3.972e-01  3.742e-02  10.614  < 2e-16 ***
## Condition1_r      1.082e+00  1.429e-01   7.569 6.77e-14 ***
## Exterior1st_r     5.524e-01  1.191e-01   4.639 3.82e-06 ***
## Condition2_r      1.123e+00  1.546e-01   7.266 6.15e-13 ***
## Exterior2nd_r     1.306e-01  4.488e-01   0.291 0.771114    
## LotConfig_r       8.853e-01  2.448e-01   3.616 0.000310 ***
## Foundation_r      3.684e-01  1.513e-01   2.435 0.015015 *  
## OverallQual_r     6.236e-01  4.608e-02  13.532  < 2e-16 ***
## OverallCond_r     7.332e-01  5.933e-02  12.358  < 2e-16 ***
## Functional.n_r    9.893e-01  1.235e-01   8.012 2.35e-15 ***
## Fireplaces_r      5.957e-01  1.511e-01   3.943 8.44e-05 ***
## KitchenQual.n_r   5.332e-01  1.544e-01   3.452 0.000572 ***
## BsmtExposure.n_r  4.139e-01  1.281e-01   3.232 0.001258 ** 
## HeatingQC.n_r     8.360e-01  2.555e-01   3.272 0.001093 ** 
## FullBath_r        6.926e-01  3.465e-01   1.999 0.045797 *  
## HalfBath_r        5.920e-01  2.274e-01   2.604 0.009319 ** 
## GarageCars_r      5.579e-01  1.471e-01   3.793 0.000155 ***
## BsmtFullBath_r    3.536e-01  1.332e-01   2.655 0.008013 ** 
## GarageQual.n_r    7.820e-01  2.492e-01   3.138 0.001737 ** 
## BsmtFinType1.n_r  1.451e-01  2.347e-01   0.618 0.536522    
## TotRmsAbvGrd_r    5.771e-01  3.621e-01   1.593 0.111282    
## BsmtFinType2.n_r  1.444e+00  5.024e-01   2.874 0.004116 ** 
## SaleAbnormal     -6.245e-02  1.150e-02  -5.429 6.69e-08 ***
## Contract          6.935e-02  2.411e-02   2.877 0.004081 ** 
## NewSale           1.041e-01  2.575e-02   4.044 5.54e-05 ***
## SingleFam         4.283e-02  9.402e-03   4.556 5.68e-06 ***
## Residential       3.175e-01  3.654e-02   8.691  < 2e-16 ***
## LotFrontage2     -3.464e-06  1.871e-06  -1.851 0.064325 .  
## SinceRemod       -1.139e-03  7.702e-04  -1.478 0.139531    
## SinceRemod2       1.052e-05  1.245e-05   0.845 0.398267    
## BsmtFinSF1sq     -6.433e-08  6.619e-09  -9.720  < 2e-16 ***
## FireplaceQu.n_r   6.543e-01  3.438e-01   1.903 0.057245 .  
## BsmtHeight_r      7.389e-01  2.962e-01   2.494 0.012737 *  
## SaleMisc         -4.680e-02  2.312e-02  -2.024 0.043138 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1064 on 1398 degrees of freedom
## Multiple R-squared:  0.932,	Adjusted R-squared:  0.929 
## F-statistic: 314.1 on 61 and 1398 DF,  p-value: < 2.2e-16
```

Here comes the final fit.  (Time for dinner yet?)

```r
modelfits <- fitModels( da5, fo, bestmodels, !running_as_kaggle_kernel, 996 )
```

```
## [1] "Training model: lars2"
## [1] "Training model: bayesglm"
## [1] "Training model: xgbLinear"
## [1] "Training model: cubist"
```

# Predicting the Test Data with Linear Model Ensemble

Read in and process the test data.  (This looks easy, but it was hard
the first time I did it, because there was considerable untidiness in the test data
that I foolishly failed to anticipate.)

```r
# Read data
testdat <- read.csv(testfile, na.strings="")
data2 <- merge(testdat, ofheo, by.x=c("YrSold","MoSold"), by.y=c("Year","Month"))
data2 <- cleanData( data2 )
```

```
## The following `from` values were not present in `x`: Po
## The following `from` values were not present in `x`: Po
```

```
## Warning in recode(df, "KitchenQual", qualcats): NAs introduced by coercion
```

```
## The following `from` values were not present in `x`: Ex
```

```
## The following `from` values were not present in `x`: Ex
```

```
## The following `from` values were not present in `x`: Sal
```

```
## Warning in recode(df, "Functional", funcats): NAs introduced by coercion
```

```
## The following `from` values were not present in `x`: ELO, NoSeWa, NoSewr
```

```
## Warning in recode(df, "Utilities", c("ELO", "NoSeWa", "NoSewr", "AllPub")):
## NAs introduced by coercion
```

```
## The following `from` values were not present in `x`: Fa, TA
```

```
## The following `from` values were not present in `x`: Po
```

```
## Warning in cleanData(data2): NAs introduced by coercion
```

```
## Warning in cleanData(data2): NAs introduced by coercion

## Warning in cleanData(data2): NAs introduced by coercion

## Warning in cleanData(data2): NAs introduced by coercion

## Warning in cleanData(data2): NAs introduced by coercion

## Warning in cleanData(data2): NAs introduced by coercion

## Warning in cleanData(data2): NAs introduced by coercion
```

```r
# Make factors continuous and add continuous versions to baseline model one by one
for (f in factors) {    
  co <- coeffs[[f]]
  data2 <- makeContinuous( data2, f, co )
}

# Make ordered variables continuous
for ( var in ordered ) {
  if ( var %in% varsToDrop ) {
    data2[var] <- NULL
  }
  else {
    data2 <- makeOrderedContinuous( data2, var, orderedCoeffs[[var]] )
  }
}

data2 <- finalCleaning( data2 )
```

Generate predictions

```r
predicted <- makePredictions ( data2, modelfits, NULL, TRUE )
```

```
## 
## 
## 
## Predicting for model: lars2
## 
## 
## 
## Predicting for model: bayesglm
## 
## 
## 
## Predicting for model: xgbLinear
## 
## 
## 
## Predicting for model: cubist
```

```r
preddf <- as.data.frame( predicted )
p_ensemble <- as.data.frame( as.matrix(preddf[bestmodels]) %*% weights )
names(p_ensemble) <- "ensemble"
```

Transform predictions back into the form that Kaggle wants them

```r
prediction <- p_ensemble + data2$LnOFHEO

result <- data.frame( cbind( data2$Id, exp(prediction) ) )
names(result) <- c("Id", "SalePrice")
sorted_result <- result[order(result$Id),]
```

And one could just store the results, but I'm going to buy another 40 basis points
or so of RMSE improvement by averaging these predictions with those of a completely
different, but comparably performing, model.

# Training Simple SVM and Predicting Test Data

This code is from mtyxwp's "svm_simple" kernel.  mtyxwp's approach to the data is
completely different from mine, and the model is completely different, and
diversity is valuable when it comes to making predictions.  Since I didn't write
this code, I'm not sure I can throw much light on it, beyond what is self-explanatory.
Here's the data processing, which makes no attempt to create new features beyond
what is necessary for tidiness.

```r
train <- read.csv(trainfile, stringsAsFactors = F)
test <- read.csv(testfile, stringsAsFactors = F)
price = train[,c("SalePrice")]

simpledf <- rbind(train[,-81], test)

simpletrain <- deal_missing(simpledf)
sdf = simpletrain

sdftrain = sdf[1:1460,]
sdftrain = cbind(sdftrain,price)
sdftest = sdf[1461:2919,]
sdftrain = sdftrain[,-1]
id = sdftest[,1]
sdftest = sdftest[,-1]

straincpy = sdftrain
straincpy[sapply(straincpy, is.factor)] <- lapply(straincpy[sapply(straincpy, is.factor)], as.numeric)
stestcpy = sdftest
stestcpy[sapply(stestcpy, is.factor)] <- lapply(stestcpy[sapply(stestcpy, is.factor)], as.numeric)
```

And then we fit probably the simplest possible support vector machine, from
the user's point of view.  (A lot is going on under the hood.)

```r
model.svm <- svm(price ~ ., data = sdftrain, cost = 1)
```

And we make predictions

```r
price.svm = predict(model.svm, sdftest)

svmResult = cbind(Id = id, SalePrice = price.svm)
colnames(svmResult) = c("Id","SalePrice")
```


# Avearging Linear Results with SVM Results

One could try to do some sophisticated stacking, and maybe I will in the future,
but for now, just take the (logarithmic) average.

```r
final_answer <- exp (   ( log(sorted_result$SalePrice) + log(svmResult[,"SalePrice"]) ) / 2   )
final_result <- data.frame( cbind( sorted_result$Id, final_answer ) )
names(final_result) <- c("Id", "SalePrice")
write.csv(final_result, file="kaggleSubmission5a.csv", row.names=FALSE)
```

Ready to submit.
