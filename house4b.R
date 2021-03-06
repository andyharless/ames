library(plyr)
library(caret)
library(Metrics)



# PRELIMINARIES I.  FUNCTIONS

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

# Function to do initial data cleaning
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

# Function to get coefficients to be used to make factor continuous given baseline model
getCoeffs <- function( df, basemodel, factor ) {
  mod <- paste0( basemodel, "+", factor, "-1" )
  lm <- lm(formula=mod, data=df)
  fnames <- grep( factor, names(lm$coefficients), fixed=TRUE )
  lm$coefficients[fnames]
}

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

# Function to get coefficients to be used to make ordered variable continuous
#   (given a model that includes any dummies found to have positive coefficients)
getOrderedCoeffs <- function( df, newmod, var ) {
  lm1 <- lm(formula=newmod, data=df)
  fnames <- grep( var, names(lm1$coefficients), fixed=TRUE )
  coeffs <- lm1$coefficients[fnames]
  names(coeffs) <- gsub( paste0(var,".gt"), "", names(coeffs), fixed=TRUE )
  coeffs
}

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

# Function to make coefficients to be used to make ordered variable continuous
makeOrderedCoeffs <- function( df, mod, var ) {
  datemp <- dummify( df, var )
  if (is.null(datemp)) { 
    print( paste0("dummify returned NULL for ", var) )
    return(NULL) 
  }
  modtemp <- addDummiesToModel( datemp, var, mod )
  if (is.null(modtemp)) {
    print("addDummiesToModel returned NULL")
    return(NULL) 
  }
  coeffs <- getOrderedCoeffs( datemp, modtemp, var )
  if (is.null(coeffs)) {
    print("getOrderedCoeffs returned NULL")
    return(NULL) 
  }
  coeffs
}

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



# PRELIMINARIES II.  EXTRA DATA (from FHFA website)

# FHFA (formerly OFHEO) House Price Index for West North Central region
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



# ON TO THE MAIN EVENT

# READ IN AND CLEAN DATA

rawdata <- read.csv("train.csv", na.strings="")
data1 <- merge(rawdata, ofheo, by.x=c("YrSold","MoSold"), by.y=c("Year","Month"))
data1 <- cleanData( data1 )



# DIVIDE DATA INTO SUBSESTS

#    "train1"   (60%)  for  primary training 
#    "validate" (20%)  for  cross-validation 
#    "testing"  (20%)  for  initial testing

set.seed(999)
inTrain <- createDataPartition(y=data1$RelPrice, p=0.8, list=FALSE)
training <- data1[inTrain,]
testing <- data1[-inTrain,]
inTrain1 <- createDataPartition(y=training$RelPrice, p=0.75, list=FALSE)
train1 <- training[inTrain1,]
validate <- training[-inTrain1,]



# RUN BASELINE LINEAR MODEL AND USE TO RECODE CATEGORICAL VARIABLES

# Make working copy of data
da <- train1

# Baseline model
basemod <- "RelPrice ~ LnOFHEO + Ln1stFlrSF + LnLivArea + OverallQual + OverallCond"

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

# Show output of augmented model
print( summary( lm( formula=mod, data=da ) ) )



# CONVERT ORDERED CATEGORICAL AND INTEGER VARIABLES TO CONTINUOUS

# Make "OverallQual" continuous.
mod <- gsub( "+ OverallQual", "", mod, fixed=TRUE ) # Delete from model
orderedCoeffs <- list() # List that will contain coefficients for all ordered variables
i <- 1 
co <- makeOrderedCoeffs( da, mod, "OverallQual" )
orderedCoeffs[[i]] <- co
names(orderedCoeffs)[[i]] <- "OverallQual"
da <- makeOrderedContinuous( da, "OverallQual", co )
mod <- paste0( mod, " + OverallQual_r" )  # Add conintuous version to model

# Make the rest of the ordered variables continuous, and save coefficients used.
mod <- gsub( "+ OverallCond", "", mod, fixed=TRUE )  # Remove OverallCond from model
ordered <- c("OverallCond",    "Functional.n", "Fireplaces",     "KitchenQual.n", 
             "BsmtExposure.n", "HeatingQC.n",  "Utilities.n",    "FullBath",  
             "HalfBath",       "GarageCars",   "BsmtFullBath",   "GarageQual.n", 
             "BsmtFinType1.n", "PavedDrive.n", "BsmtCond.n",     "GarageCond.n", 
             "FireplaceQu.n",  "ExterQual.n",  "TotRmsAbvGrd",   "LotShape.n", 
             "BsmtHalfBath",   "PoolQC.n",     "BsmtFinType2.n", "ExterCond.n", 
             "BedroomAbvGr",   "BsmtHeight",   "KitchenAbvGr",   "GarageFinish.n") 
varsToDrop <- c()
for ( var in ordered ) {
  co <- makeOrderedCoeffs( da, mod, var )
  if ( is.null(co) ) {
    varsToDrop <- c(varsToDrop, var)
    da[var] <- NULL
  }
  else {
    da <- makeOrderedContinuous( da, var, co )
    mod <- paste0( mod, " + ", var, "_r")
    i <- i + 1
    orderedCoeffs[[i]] <- co
    names(orderedCoeffs)[[i]] <- var
  }
}


# MAKE THE DATA NICE

da = finalCleaning( da )


# INSPECT OUTPUT OF FULL MODEL

regmodel = lm( formula="RelPrice ~ .", data=da )
print( summary( regmodel ) )



# FOR A QUICK CHECK, APPLY TRANSFORMAITONS TO CROSS-VALIDATION SET AND PREDICT

# Make working copy of data
da2 <- validate

# Process it
for (f in factors) {    
  co <- coeffs[[f]]
  da2 <- makeContinuous( da2, f, co )
}

co <- orderedCoeffs[["OverallQual"]] 
da2 <- makeOrderedContinuous( da2, "OverallQual", co )
for ( var in ordered ) {
  if ( var %in% varsToDrop ) {
    da2[var] <- NULL
  }
  else {
    da2 <- makeOrderedContinuous( da2, var, orderedCoeffs[[var]] )
  }
}

da2 <- finalCleaning( da2 )

# Make predictions
prediction <- predict(regmodel, da2, type="response")

# Fill in missing values
baselm <- lm(formula=basemod, data=train1)
basepred <- predict( baselm, validate, type="response")
prediction[is.na(prediction)] <- basepred[is.na(prediction)]

# RMSE
rmse(da2$RelPrice,prediction)



# AND HOW ABOUT A QUICKIE VISUAL REGULARIZATION

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



# RUN FITS AND FIND BEST COMBO OF PREDICTORS

# Set up for multiple cores
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

# Fit the models
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
modelfits = list()
for (m in modelnames) {
  print ( paste("Training model:", m) )
  set.seed(998)
  fit <- train( as.formula(fo), data=da, method=m )
  modelfits = c(modelfits, list(fit))
}

# Go back to sequential processing
stopCluster(cluster)
registerDoSEQ()

# Do predictions on validation data and find best models
rmses <- list()
predicted <- list()
for (fi in modelfits) {
  writeLines ( paste("\n\n\nPredicting validation set for model:", fi[[1]]) )
  p <- predict(fi, newdata=da2)
  p[is.na(p)] <- basepred[is.na(p)]
  predicted <- c(predicted, list(p))
  rmses <- c(rmses, rmse(da2$RelPrice, p))
}
names(rmses) <- modelnames
names(predicted) <- modelnames
print( rmses )


preddf <- cbind( as.data.frame(predicted), da2$RelPrice )
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

p_ensemble <- as.data.frame( as.matrix(preddf[names(weights)]) %*% weights )
names( p_ensemble ) <- "ensemble"
rmse( da2$RelPrice, p_ensemble )



# FIND NEW COEFFICIENTS TO MAKE FACTORS CONTINUOUS, INCLUDING THE VALIDATION DATA

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

# Make "OverallQual" continuous.
mod <- gsub( "+ OverallQual", "", mod, fixed=TRUE ) # Delete from model
orderedCoeffs <- list() # List that will contain coefficients for all ordered variables
i <- 1 
co <- makeOrderedCoeffs( da3, mod, "OverallQual" )
orderedCoeffs[[i]] <- co
names(orderedCoeffs)[[i]] <- "OverallQual"
da3 <- makeOrderedContinuous( da3, "OverallQual", co )
mod <- paste0( mod, " + OverallQual_r" )  # Add conintuous version to model

# Make the rest of the ordered variables continuous, and save coefficients used.
mod <- gsub( "+ OverallCond", "", mod, fixed=TRUE )  # Remove OverallCond from model
varsToDrop <- c()
for ( var in ordered ) {
  co <- makeOrderedCoeffs( da3, mod, var )
  if ( is.null(co) ) {
    varsToDrop <- c(varsToDrop, var)
    da3[var] <- NULL
  }
  else {
    da3 <- makeOrderedContinuous( da3, var, co )
    mod <- paste0( mod, " + ", var, "_r")
    i <- i + 1
    orderedCoeffs[[i]] <- co
    names(orderedCoeffs)[[i]] <- var
  }
}

da3 = finalCleaning( da3 )



# APPLY TRANSFORMATIONS TO INITIAL TEST DATA SET

# Make working copy of data
da4 <- testing

# Make factors continuous and add continuous versions to baseline model one by one
for (f in factors) {    
  co <- coeffs[[f]]
  da4 <- makeContinuous( da4, f, co )
}

co <- orderedCoeffs[["OverallQual"]] 
da4 <- makeOrderedContinuous( da4, "OverallQual", co )
for ( var in ordered ) {
  if ( var %in% varsToDrop ) {
    da4[var] <- NULL
  }
  else {
    da4 <- makeOrderedContinuous( da4, var, orderedCoeffs[[var]] )
  }
}

da4 <- finalCleaning( da4 )



# RE-FIT AND TEST

# Set up for multiple cores
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

# Fit the models
modelfits = list()
for (m in names(weights)) {
  print ( paste("Training model:", m) )
  set.seed(997)
  fit <- train( as.formula(fo), data=da3, method=m )
  modelfits = c(modelfits, list(fit))
}

# Go back to sequential processing
stopCluster(cluster)
registerDoSEQ()

# Do predictions on initial test data using selected models
rmses <- list()
predicted <- list()
p_ensemble <- 0*da4$RelPrice
for (fi in modelfits) {
  writeLines ( paste("\n\n\nPredicting initial test set for model:", fi[[1]]) )
  p <- predict(fi, newdata=da4)
  predicted <- c(predicted, list(p))
  rmses <- c(rmses, rmse(da4$RelPrice, p))
}
names(rmses) <- names(weights)
names(predicted) <- names(weights)
preddf <- as.data.frame( predicted )
p_ensemble <- as.data.frame( as.matrix(preddf[names(weights)]) %*% weights )
names(p_ensemble) <- "ensemble"

# Estimated forecast error
print( rmse( da4$RelPrice, p_ensemble ) )
invisible( readline( prompt="Press [enter] to continue" ) )



# REDO ANALYSIS WITH FULL OFFICIAL TRAINING SET

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

# Make "OverallQual" continuous.
mod <- gsub( "+ OverallQual", "", mod, fixed=TRUE ) # Delete from model
orderedCoeffs <- list() # List that will contain coefficients for all ordered variables
i <- 1 
co <- makeOrderedCoeffs( da5, mod, "OverallQual" )
orderedCoeffs[[i]] <- co
names(orderedCoeffs)[[i]] <- "OverallQual"
da5 <- makeOrderedContinuous( da5, "OverallQual", co )
mod <- paste0( mod, " + OverallQual_r" )  # Add conintuous version to model

# Make the rest of the ordered variables continuous, and save coefficients used.
mod <- gsub( "+ OverallCond", "", mod, fixed=TRUE )  # Remove OverallCond from model
varsToDrop <- c()
for ( var in ordered ) {
  co <- makeOrderedCoeffs( da5, mod, var )
  if ( is.null(co) ) {
    varsToDrop <- c(varsToDrop, var)
    da5[var] <- NULL
  }
  else {
    da5 <- makeOrderedContinuous( da5, var, co )
    mod <- paste0( mod, " + ", var, "_r")
    i <- i + 1
    orderedCoeffs[[i]] <- co
    names(orderedCoeffs)[[i]] <- var
  }
}

da5 = finalCleaning( da5 )

# Inspect output of full model
regmodel = lm( formula="RelPrice ~ .", data=da5 )
print( summary( regmodel ) )

# Changes to model
fo <- gsub( "+ ExterQual.n_r ", "", fo, fixed=TRUE  )
fo <- gsub( "+ ExterCond.n_r ", "", fo, fixed=TRUE )
fo <- gsub( "+ PoolQC.n_r ", "", fo, fixed=TRUE )
fo <- paste0(fo, "+ FireplaceQu.n_r + BsmtHeight_r + SaleMisc ")

# Look at the OLS fit
regmodel = lm( formula=fo, data=da5 )
print( summary( regmodel ) )

# Set up for multiple cores
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

# Fit the models
modelfits = list()
for (m in names(weights)) {
  print ( paste("Training model:", m) )
  set.seed(996)
  fit <- train( as.formula(fo), data=da5, method=m )
  modelfits = c(modelfits, list(fit))
}

# Go back to sequential processing
stopCluster(cluster)
registerDoSEQ()



# READ IN AND PROCESS TEST DATA

# Read data
testdat <- read.csv("test.csv", na.strings="")
data2 <- merge(testdat, ofheo, by.x=c("YrSold","MoSold"), by.y=c("Year","Month"))

data2 <- cleanData( data2 )

# Make factors continuous and add continuous versions to baseline model one by one
for (f in factors) {    
  co <- coeffs[[f]]
  data2 <- makeContinuous( data2, f, co )
}

co <- orderedCoeffs[["OverallQual"]] 
data2 <- makeOrderedContinuous( data2, "OverallQual", co )
for ( var in ordered ) {
  if ( var %in% varsToDrop ) {
    data2[var] <- NULL
  }
  else {
    data2 <- makeOrderedContinuous( data2, var, orderedCoeffs[[var]] )
  }
}

data2 <- finalCleaning( data2 )



# MAKE PREDICTIONS

predicted <- list()
for (fi in modelfits) {
  writeLines ( paste("\n\n\nGenerating final predictions for model:", fi[[1]]) )
  p <- predict(fi, newdata=data2)
  predicted <- c(predicted, list(p))
}
names(predicted) <- names(weights)
preddf <- as.data.frame( predicted )
p_ensemble <- as.data.frame( as.matrix(preddf[names(weights)]) %*% weights )
names(p_ensemble) <- "ensemble"

prediction <- p_ensemble + data2$LnOFHEO

result <- data.frame( cbind( data2$Id, exp(prediction) ) )
names(result) <- c("Id", "SalePrice")

sorted_result <- result[order(result$Id),]
write.csv(sorted_result, file="kaggleSubmission4b.csv", row.names=FALSE)
