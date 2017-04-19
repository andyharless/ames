library(plyr)
library(caret)
library(Metrics)


# READ IN DATA

data1 <- read.csv("train.csv", na.strings="")
ofheo <- read.csv("ofheowncnsa.csv")
data1 <- merge(data1, ofheo, by.x=c("YrSold","MoSold"), by.y=c("Year","Month"))



# CLEAN UP DATA

# Convert pseudo-numeric "type of dwelling" identifier to a factor
data1$MSSubClass <- as.factor( data1$MSSubClass )

# Deal with numeric variables that have missing values
data1$LotFrontage = as.character( data1$LotFrontage )
data1$HasLotFrontage = ifelse( data1$LotFrontage=="NA", 0, 1 )
data1$LotFrontage = ifelse( data1$LotFrontage=="NA", "0", data1$LotFrontage ) 
data1$LotFrontage = as.numeric( data1$LotFrontage )

data1$MasVnrArea = as.character( data1$MasVnrArea )
data1$HasMasVnr = ifelse( data1$MasVnrArea=="NA", 0, 1 )
data1$MasVnrArea = ifelse( data1$MasVnrArea=="NA", "0", data1$MasVnrArea ) 
data1$MasVnrArea = as.numeric( data1$MasVnrArea )

data1$GarageYrBlt = as.character( data1$GarageYrBlt )
data1$HasGarageYr = ifelse( data1$GarageYrBlt=="NA", 0, 1 )
data1$GarageYrBlt = ifelse( data1$GarageYrBlt=="NA", "0", data1$GarageYrBlt ) 
data1$GarageYrBlt = as.numeric( data1$GarageYrBlt )


# Dummy for "has basement"
data1$HasBasement = ifelse( data1$BsmtQual=="NA", 0, 1 )

# Function to recode levels to numeric in specified order and add ".n" to name
recode <- function( df, var, lev ) { 
  to <- as.character( 0:(length(lev)-1) )
  newvar <- as.numeric( mapvalues( df[[var]], from=lev, to=to ) )
  newname <- paste0(var,".n")
  df <- cbind( df, newvar )
  names(df)[ncol(df)] <- newname
  df[var] <- NULL
  df
}

# Recode ordered factors as pseudo-continuous numerical variables
qualcats  = c( "Po",  "Fa",  "TA",   "Gd",   "Ex" )
qualcats2 = c( "NA",  qualcats )
funcats   = c( "Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ" )
basecats  = c( "NA",  "Unf", "LwQ",  "Rec",  "BLQ", "ALQ",  "GLQ"         )
data1 <- recode( data1, "ExterCond",    qualcats  )
data1 <- recode( data1, "ExterQual",    qualcats  )
data1 <- recode( data1, "HeatingQC",    qualcats  )
data1 <- recode( data1, "KitchenQual",  qualcats  )
data1 <- recode( data1, "BsmtCond",     qualcats2 )
data1 <- recode( data1, "FireplaceQu",  qualcats2 )
data1 <- recode( data1, "GarageQual",   qualcats2 )
data1 <- recode( data1, "GarageCond",   qualcats2 )
data1 <- recode( data1, "Functional",   funcats   )
data1 <- recode( data1, "BsmtFinType1", basecats  )
data1 <- recode( data1, "BsmtFinType2", basecats  )
data1 <- recode( data1, "PavedDrive",   c("N",   "P",      "Y"                     ) )                                         
data1 <- recode( data1, "Utilities",    c("ELO", "NoSeWa", "NoSewr", "AllPub"      ) )
data1 <- recode( data1, "LotShape",     c("IR3", "IR2",    "IR1",    "Reg"         ) )                                         
data1 <- recode( data1, "BsmtExposure", c("NA",  "No",     "Mn",     "Av",    "Gd" ) )
data1 <- recode( data1, "PoolQC",       c("NA",  "Fa",     "TA",     "Gd",    "Ex" ) )

# BsmtHeight needs special treatment, since it's really a categorized continuous variable
from <- c("NA", "Po", "Fa", "TA", "Gd", "Ex"  ) 
to   <- c("0",  "50", "75", "85", "95", "120" )                                          
data1$BsmtHeight <- as.numeric( mapvalues(data1$BsmtQual, from=from, to=to) )
data1$BsmtQual <- NULL



# DIVIDE DATA INTO SUBSESTS

#    "train1"   (60%)  for  primary training 
#    "validate" (20%)  for  cross-validation 
#    "testing"  (20%)  for  initial testing

set.seed(999)
inTrain <- createDataPartition(y=data1$SalePrice, p=0.8, list=FALSE)
training <- data1[inTrain,]
testing <- data1[-inTrain,]
inTrain1 <- createDataPartition(y=training$SalePrice, p=0.75, list=FALSE)
train1 <- training[inTrain1,]
validate <- training[-inTrain1,]



# RUN BASELINE LINEAR MODEL AND USE TO RECODE CATEGORICAL VARIABLES

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

# Make working copy of data
da <- train1

# Construct baseline model
lhs <- "log(SalePrice/OFHEO)"
vars <- c("log(OFHEO)", "log(X1stFlrSF)", "log(GrLivArea)", "OverallQual", "OverallCond")
rhs <- paste(vars, collapse="+")
basemod <- paste0(lhs, " ~ ", rhs)

# Make factors continuous and add continuous versions to baseline model one by one
factors <- c( "Neighborhood", "MSSubClass", "Condition1", "Exterior1st", "Condition2", 
              "Exterior2nd")
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



# CHECK OUTPUT OF BASELINE MODEL VISUALLY FOR RESIDUAL INTERTEMPORAL PATTERNS

lmtemp = lm(formula=basemod, data=da)

month = da$MoSold
tab = aggregate( lmtemp$residuals, list(month), mean)
plot(tab[order(tab$Group.1),])
invisible(readline(prompt="Press [enter] to continue"))

months = 100*da$YrSold + 8.33*(da$MoSold-1)
tab = aggregate( lmtemp$residuals, list(months), mean)
plot(tab[order(tab$Group.1),])
yn = readline(prompt="Any residual intertemporal pattern (y/n)? ")

if (yn == "n") {
   da$MoSold <- NULL
   da$YrSold <- NULL
}



# INSPECT OUTPUT OF AUGMENTED MODEL AND CONTENTS OF DATA FRAME

print( summary( lm( formula=mod, data=da ) ) )
invisible(readline( prompt="Press [enter] to continue" ))
print( str(da) )
invisible(readline( prompt="Press [enter] to continue" ))
print( head(da) )
invisible(readline( prompt="Press [enter] to continue" ))



# TRANSFORM COLUMNS OF DATA SO I DON'T HAVE TO SPECIFY TRANSFORMATIONS SUBSEQUENTLY

da$SalePrice = log( da$SalePrice / da$OFHEO )
names(da)[names(da)=="SalePrice"] <- "RelPrice"
da$X1stFlrSF = log( da$X1stFlrSF )
names(da)[names(da)=="X1stFlrSF"] <- "Ln1stFlrSF"
da$GrLivArea = log( da$GrLivArea )
names(da)[names(da)=="GrLivArea"] <- "LnLivArea"
da$OFHEO = log( da$OFHEO )
names(da)[names(da)=="OFHEO"] <- "LnOFHEO"



# INSPECT OUTPUT OF FULL MODEL

regmodel = lm( formula="RelPrice ~ .", data=da )
print( summary( regmodel ) )
invisible( readline( prompt="Press [enter] to continue" ) )



# FOR A QUICK CHECK, APPLY TRANSFORMAITONS TO CROSS-VALIDATION SET AND PREDICT

# Make working copy of data
da2 <- validate

# Make factors continuous and add continuous versions to baseline model one by one
factors <- c( "Neighborhood", "MSSubClass", "Condition1", "Exterior1st", "Condition2", 
              "Exterior2nd")
for (f in factors) {    
  co <- coeffs[[f]]
  da2 <- makeContinuous( da2, f, co )
}

# Transform data columns
da2$SalePrice = log( da2$SalePrice / da2$OFHEO )
names(da2)[names(da2)=="SalePrice"] <- "RelPrice"
da2$X1stFlrSF = log( da2$X1stFlrSF )
names(da2)[names(da2)=="X1stFlrSF"] <- "Ln1stFlrSF"
da2$GrLivArea = log( da2$GrLivArea )
names(da2)[names(da2)=="GrLivArea"] <- "LnLivArea"
da2$OFHEO = log( da2$OFHEO )
names(da2)[names(da2)=="OFHEO"] <- "LnOFHEO"
da2$MoSold <- NULL
da2$YrSold <- NULL

# Make predictions
prediction <- predict(regmodel, da2, type="response")

# Fill in missing values
baselm <- lm(formula=basemod, data=train1)
basepred <- predict( baselm, validate, type="response")
prediction[is.na(prediction)] <- basepred[is.na(prediction)]

# RMSE
rmse(da2$RelPrice,prediction)



# AND HOW ABOUT A QUICKIE VISUAL REGULARIZATION

fo = "RelPrice ~  MSZoning + HasLotFrontage + LotFrontage + LotArea + LandSlope "
fo = paste0(fo, "+ BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt ")
fo = paste0(fo, "+ RoofMatl + Foundation + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF ")
fo = paste0(fo, "+ HasBasement + CentralAir + Ln1stFlrSF + X2ndFlrSF + LowQualFinSF ")
fo = paste0(fo, "+ LnLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath ")
fo = paste0(fo, "+ BedroomAbvGr + KitchenAbvGr + Fireplaces + GarageCars ")
fo = paste0(fo, "+ ScreenPorch + PoolArea + LnOFHEO + GarageCond.n + Functional.n ")
fo = paste0(fo, "+ LotShape.n + PoolQC.n + Neighborhood_r + MSSubClass_r ")
fo = paste0(fo, "+ Condition1_r + Exterior1st_r + Condition2_r ")
mymodel = lm( formula=fo, data=da )
prediction <- predict(mymodel, da2, type="response")
prediction[is.na(prediction)] <- basepred[is.na(prediction)]
rmse(da2$RelPrice,prediction)



# RUN REGULARIZED REGRESSIONS, MAKE PREDICTIONS, CHOOSE METHOD

# Fix bad data point in validation set
da2a = da2
da2a$Exterior1st_r[is.na(da2a$Exterior1st_r)] = mean(da2a$Exterior1st_r, na.rm=TRUE)

# Run a bunch of regularized regression models
modelnames = c("penalized", "lars", "lars2", "rqlasso", "relaxo", "ridge", "enet",
               "glmnet", "lasso", "foba", "icr", "nnls")
modelfits = list()
for (m in modelnames) {
  print ( paste("Training model:", m) )
  fit <- train( as.formula(fo), data=da, method=m )
  modelfits = c(modelfits, list(fit))
}

# Do predictions on validation data and find best models
rmses <- list()
predicted <- list()
for (fi in modelfits) {
  writeLines ( paste("\n\n\nPredicting validation set for model:", fi[[1]]) )
  p <- predict(fi, newdata=da2a)
  predicted <- c(predicted, list(p))
  rmses <- c(rmses, rmse(da2a$RelPrice, p))
}
names(rmses) <- modelnames
names(predicted) <- modelnames
bestmods <- names(sort(unlist(rmses))[1:3])
print(bestmods)
print(rmses[bestmods])



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



# APPLY TRANSFORMATIONS TO TRAIN+VALIDATION DATA SET AND INITIAL TEST DATA SET

# Make working copy of data
da4 <- testing

# Make factors continuous and add continuous versions to baseline model one by one
factors <- c( "Neighborhood", "MSSubClass", "Condition1", "Exterior1st", "Condition2", 
              "Exterior2nd")
for (f in factors) {    
  co <- coeffs[[f]]
  da4 <- makeContinuous( da4, f, co )
}

# Transform data columns in train+validation set
da3$SalePrice = log( da3$SalePrice / da3$OFHEO )
names(da3)[names(da3)=="SalePrice"] <- "RelPrice"
da3$X1stFlrSF = log( da3$X1stFlrSF )
names(da3)[names(da3)=="X1stFlrSF"] <- "Ln1stFlrSF"
da3$GrLivArea = log( da3$GrLivArea )
names(da3)[names(da3)=="GrLivArea"] <- "LnLivArea"
da3$OFHEO = log( da3$OFHEO )
names(da3)[names(da3)=="OFHEO"] <- "LnOFHEO"
da3$MoSold <- NULL
da3$YrSold <- NULL

# Transform data columns in initial testing set
da4$SalePrice = log( da4$SalePrice / da4$OFHEO )
names(da4)[names(da4)=="SalePrice"] <- "RelPrice"
da4$X1stFlrSF = log( da4$X1stFlrSF )
names(da4)[names(da4)=="X1stFlrSF"] <- "Ln1stFlrSF"
da4$GrLivArea = log( da4$GrLivArea )
names(da4)[names(da4)=="GrLivArea"] <- "LnLivArea"
da4$OFHEO = log( da4$OFHEO )
names(da4)[names(da4)=="OFHEO"] <- "LnOFHEO"
da4$MoSold <- NULL
da4$YrSold <- NULL



# RE-FIT AND TEST

# Fix bad data point in test set
da4$Condition2_r[is.na(da4$Condition2_r)] = mean(da4$Condition2_r, na.rm=TRUE)

# For now I'm going to start take my visually regularized model as a starting point
fit <- train( as.formula(fo), data=da3, method=bestmods[1] )
p <- predict(fit, newdata=da4)
rmse(da4$RelPrice, p)



# REDO ANALYSIS WITH FULL OFFICIAL TEST SET

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

# Transform data columns
da5$SalePrice = log( da5$SalePrice / da5$OFHEO )
names(da5)[names(da5)=="SalePrice"] <- "RelPrice"
da5$X1stFlrSF = log( da5$X1stFlrSF )
names(da5)[names(da5)=="X1stFlrSF"] <- "Ln1stFlrSF"
da5$GrLivArea = log( da5$GrLivArea )
names(da5)[names(da5)=="GrLivArea"] <- "LnLivArea"
da5$OFHEO = log( da5$OFHEO )
names(da5)[names(da5)=="OFHEO"] <- "LnOFHEO"
da5$MoSold <- NULL
da5$YrSold <- NULL

# Inspect output of full model
regmodel = lm( formula="RelPrice ~ .", data=da5 )
print( summary( regmodel ) )
invisible( readline( prompt="Press [enter] to continue" ) )

# Some exploratory info to help decide what goes in the model
print( mean(da5$MiscVal) )
print( mean(da5$MiscVal > 0) )
print( table(da5$MiscFeature) )
print( table(da5$GarageType) )
print( table(da5$HasGarageYr, da5$GarageType) )
print( table(da5$SaleCondition) )
invisible( readline( prompt="Press [enter] to continue" ) )

# Notes on variable inclusion: 
#   HasGarageYr will proxy for meaningful part of GarageType
#   Coefficients for MiscFeature make no sense: can't be so awful to have 2nd garage
#   Leaving out MiscVal because it's too closely intertwined with nonsense MiscFeature

# Changes to model
salecon = as.character(da5$SaleCondition)
da5$SaleMisc <- ifelse( salecon=="Family" | salecon=="Partial", 1, 0 )
da5$SaleAbnormal <- ifelse( salecon=="Abnorml", 1, 0 )
da5$LowDownPmt <- ifelse( as.character(da5$SaleType)=="ConLD", 1, 0 )
fo <- gsub( "+ HouseStyle ", "", fo )
fo <- gsub( "+ LowQualFinSF ", "", fo )
fo <- gsub( "+ MSSubClass_r ", "", fo )
fo <- paste0(fo, "+ LandContour + LotConfig + HasGarageYr + GarageArea ")
fo <- paste0(fo, "+ WoodDeckSF + EnclosedPorch + LowDownPmt + LowDownPmt ")
fo <- paste0(fo, "+ SaleMisc + SaleAbnormal + HasLotFrontage ")

# Look at the OLS fit
regmodel = lm( formula=fo, data=da5 )
print( summary( regmodel ) )
invisible( readline( prompt="Press [enter] to continue" ) )

# Train the final model
fit <- train( as.formula(fo), data=da5, method=bestmods[1] )



# READ IN AND PROCESS TEST DATA

# Read data
testdat <- read.csv("test.csv", na.strings="")
data2 <- merge(testdat, ofheo, by.x=c("YrSold","MoSold"), by.y=c("Year","Month"))

# Initial cleaning
data2$MSSubClass <- as.factor( data2$MSSubClass )
data2$LotFrontage = as.character( data2$LotFrontage )
data2$HasLotFrontage = ifelse( data2$LotFrontage=="NA", 0, 1 )
data2$LotFrontage = ifelse( data2$LotFrontage=="NA", "0", data2$LotFrontage ) 
data2$LotFrontage = as.numeric( data2$LotFrontage )
data2$MasVnrArea = as.character( data2$MasVnrArea )
data2$HasMasVnr = ifelse( data2$MasVnrArea=="NA", 0, 1 )
data2$MasVnrArea = ifelse( data2$MasVnrArea=="NA", "0", data2$MasVnrArea ) 
data2$MasVnrArea = as.numeric( data2$MasVnrArea )
data2$GarageYrBlt = as.character( data2$GarageYrBlt )
data2$HasGarageYr = ifelse( data2$GarageYrBlt=="NA", 0, 1 )
data2$GarageYrBlt = ifelse( data2$GarageYrBlt=="NA", "0", data2$GarageYrBlt ) 
data2$GarageYrBlt = as.numeric( data2$GarageYrBlt )
data2$HasBasement = ifelse( data2$BsmtQual=="NA", 0, 1 )

# Recode ordered factors as pseudo-continuous numerical variables
data2 <- recode( data2, "ExterCond",    qualcats  )
data2 <- recode( data2, "ExterQual",    qualcats  )
data2 <- recode( data2, "HeatingQC",    qualcats  )
data2 <- recode( data2, "KitchenQual",  qualcats  )
data2 <- recode( data2, "BsmtCond",     qualcats2 )
data2 <- recode( data2, "FireplaceQu",  qualcats2 )
data2 <- recode( data2, "GarageQual",   qualcats2 )
data2 <- recode( data2, "GarageCond",   qualcats2 )
data2 <- recode( data2, "Functional",   funcats   )
data2 <- recode( data2, "BsmtFinType1", basecats  )
data2 <- recode( data2, "BsmtFinType2", basecats  )
data2 <- recode( data2, "PavedDrive",   c("N",   "P",      "Y"                     ) )                                         
data2 <- recode( data2, "Utilities",    c("ELO", "NoSeWa", "NoSewr", "AllPub"      ) )
data2 <- recode( data2, "LotShape",     c("IR3", "IR2",    "IR1",    "Reg"         ) )                                         
data2 <- recode( data2, "BsmtExposure", c("NA",  "No",     "Mn",     "Av",    "Gd" ) )
data2 <- recode( data2, "PoolQC",       c("NA",  "Fa",     "TA",     "Gd",    "Ex" ) )
from <- c("NA", "Po", "Fa", "TA", "Gd", "Ex"  ) 
to   <- c("0",  "50", "75", "85", "95", "120" )                                          
data2$BsmtHeight <- as.numeric( mapvalues(data2$BsmtQual, from=from, to=to) )
data2$BsmtQual <- NULL

# Make factors continuous and add continuous versions to baseline model one by one
factors <- c( "Neighborhood", "MSSubClass", "Condition1", "Exterior1st", "Condition2", 
              "Exterior2nd")
for (f in factors) {    
  co <- coeffs[[f]]
  data2 <- makeContinuous( data2, f, co )
}

# Transform data columns
data2$X1stFlrSF = log( data2$X1stFlrSF )
names(data2)[names(data2)=="X1stFlrSF"] <- "Ln1stFlrSF"
data2$GrLivArea = log( data2$GrLivArea )
names(data2)[names(data2)=="GrLivArea"] <- "LnLivArea"
data2$OFHEO = log( data2$OFHEO )
names(data2)[names(data2)=="OFHEO"] <- "LnOFHEO"
data2$MoSold <- NULL
data2$YrSold <- NULL
salecon = as.character(data2$SaleCondition)
data2$SaleMisc <- ifelse( salecon=="Family" | salecon=="Partial", 1, 0 )
data2$SaleAbnormal <- ifelse( salecon=="Abnorml", 1, 0 )
data2$LowDownPmt <- ifelse( as.character(data2$SaleType)=="ConLD", 1, 0 )

# Fix missing value of MSZoning by assigning to most frequent category
mz = as.character(data2$MSZoning)
data2$MSZoning <- as.factor( ifelse( mz=="NA", "RL", mz ) )

# Fix numeric variables that were read as factors
data2$BsmtFinSF1 = as.numeric( as.character( data2$BsmtFinSF1 ) )
data2$BsmtFinSF1[is.na(data2$BsmtFinSF1)] = mean(data2$BsmtFinSF1, na.rm=TRUE)
data2$BsmtFinSF2 = as.numeric( as.character( data2$BsmtFinSF2 ) )
data2$BsmtFinSF2[is.na(data2$BsmtFinSF2)] = mean(data2$BsmtFinSF2, na.rm=TRUE)
data2$BsmtUnfSF = as.numeric( as.character( data2$BsmtUnfSF ) )
data2$BsmtUnfSF[is.na(data2$BsmtUnfSF)] = mean(data2$BsmtUnfSF, na.rm=TRUE)
data2$BsmtFullBath = as.numeric( as.character( data2$BsmtFullBath ) )
data2$BsmtFullBath[is.na(data2$BsmtFullBath)] = mean(data2$BsmtFullBath, na.rm=TRUE)
data2$BsmtHalfBath = as.numeric( as.character( data2$BsmtHalfBath ) )
data2$BsmtHalfBath[is.na(data2$BsmtHalfBath)] = mean(data2$BsmtHalfBath, na.rm=TRUE)
data2$GarageCars = as.numeric( as.character( data2$GarageCars ) )
data2$GarageCars[is.na(data2$GarageCars)] = mean(data2$GarageCars, na.rm=TRUE)
data2$GarageArea = as.numeric( as.character( data2$GarageArea ) )
data2$GarageArea[is.na(data2$GarageArea)] = mean(data2$GarageArea, na.rm=TRUE)

# Fix numeric variables with missing values
nalist = sapply(data2, function(x) sum(is.na(x)))
print( nalist[nalist>0] )
data2$MSSubClass_r[is.na(data2$MSSubClass_r)] = mean(data2$MSSubClass_r, na.rm=TRUE)
data2$Exterior1st_r[is.na(data2$Exterior1st_r)] = mean(data2$Exterior1st_r, na.rm=TRUE)
data2$Exterior2nd_r[is.na(data2$Exterior2nd_r)] = mean(data2$Exterior2nd_r, na.rm=TRUE)


# MAKE PREDICTIONS

p <- predict(fit, data2)
prediction <- p + data2$LnOFHEO
result <- data.frame( cbind( data2$Id, exp(prediction) ) )
names(result) <- c("Id", "SalePrice")

sorted_result <- result[order(result$Id),]
write.csv(sorted_result, file="kaggleSubmission1.csv", row.names=FALSE)

