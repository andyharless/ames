library(plyr)
library(caret)


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

summary( lm( formula=mod, data=da ) )
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

summary( lm( formula="RelPrice ~ .", data=da ) )
readline( prompt="Press [enter] to continue" )



# RUN REGULARIZED REGRESSIONS, MAKE PREDICTIONS, CHOOSE METHOD, RE-FIT, TEST, &c

# THIS PART NOT YET IMPLEMENTED

