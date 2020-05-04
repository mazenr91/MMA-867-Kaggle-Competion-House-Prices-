library(tidyverse)
library(glmnet)
library(mice)

# Importing Dataset

house<-read.csv("C:\\Users\\rifai\\OneDrive\\Queens University\\MMA\\MMA 867\\Assignment 1\\house-prices-advanced-regression-techniques\\train.csv", header=TRUE, sep=",")
competition<-read.csv("C:\\Users\\rifai\\OneDrive\\Queens University\\MMA\\MMA 867\\Assignment 1\\house-prices-advanced-regression-techniques\\test.csv", header=TRUE, sep=",")


#Combining all data into one dataset
competition$SalePrice<-NA

data<- rbind(house, competition)

# Observe data set items,summary and structure 

head(data)
summary(data)
str(data)


#Data Cleaning & Feature Engineering 

# MSZoning: General zoning classification 

data$MSZoning <- as.character(data$MSZoning)
data$MSZoning[is.na(data$MSZoning)] <- "RH"  
data$MSZoning[data$MSZoning == "FV"] <- "4"
data$MSZoning[data$MSZoning == "RL"] <- "3"
data$MSZoning[data$MSZoning == "RH"] <- "2"
data$MSZoning[data$MSZoning == "RM"] <- "2"
data$MSZoning[data$MSZoning == "C (all)"] <- "1"
data$MSZoning <- as.numeric(data$MSZoning)

#Alley Type

data<-transform(data, Alley = ifelse(is.na(Alley), "None", Alley))

data$Alley <- as.factor(data$Alley)


#Land Contour 

data$LandContour <- as.character(data$LandContour)
data$LandContour[data$LandContour == "Lvl"] <- "1"
data$LandContour[data$LandContour == "Bnk"] <- "2"
data$LandContour[data$LandContour == "HLS"] <- "3"
data$LandContour[data$LandContour == "Low"] <- "4"
data$LandContour <- as.numeric(data$LandContour)

#house style

data$HouseStyle <- as.character(data$HouseStyle)
data$HouseStyle[data$HouseStyle == "1Story"] <- "1"
data$HouseStyle[data$HouseStyle == "1.5Unf"] <- "2"
data$HouseStyle[data$HouseStyle == "1.5Fin"] <- "3"
data$HouseStyle[data$HouseStyle == "2Story"] <- "4"
data$HouseStyle[data$HouseStyle == "2.5Unf"] <- "5"
data$HouseStyle[data$HouseStyle == "2.5Fin"] <- "6"
data$HouseStyle[data$HouseStyle == "SFoyer"] <- "7"
data$HouseStyle[data$HouseStyle == "SLvl"] <- "8"
data$HouseStyle <- as.numeric(data$HouseStyle)


#Roof Style 

data$RoofStyle <- as.character(data$RoofStyle)
data$RoofStyle[data$RoofStyle == "Flat"] <- "1"
data$RoofStyle[data$RoofStyle == "Gable"] <- "2"
data$RoofStyle[data$RoofStyle == "Gambrel"] <- "3"
data$RoofStyle[data$RoofStyle == "Hip"] <- "4"
data$RoofStyle[data$RoofStyle == "Mansard"] <- "5"
data$RoofStyle[data$RoofStyle == "Shed"] <- "6"
data$RoofStyle <- as.numeric(data$RoofStyle)

summary(data)

#Exterior house covering 1 

data$Exterior1st <- as.character(data$Exterior1st)
data$Exterior1st[is.na(data$Exterior1st)] <- "VinylSd"
data$Exterior1st <- as.factor(data$Exterior1st)

#Exterior house covering 2

data$Exterior2nd <- as.character(data$Exterior2nd)
data$Exterior2nd[is.na(data$Exterior2nd)] <- "VinylSd"
data$Exterior2nd <- as.factor(data$Exterior2nd)

# Exterior Quality

data$ExterQual <- as.character(data$ExterQual)
data$ExterQual[data$ExterQual == "Ex"] <- "5"
data$ExterQual[data$ExterQual == "Gd"] <- "4"
data$ExterQual[data$ExterQual == "TA"] <- "3"
data$ExterQual[data$ExterQual == "Fa"] <- "2"
data$ExterQual[data$ExterQual == "Po"] <- "1"
data$ExterQual <- as.numeric(data$ExterQual)

#Exterior Condition

data$ExterCond <- as.character(data$ExterCond)
data$ExterCond[data$ExterCond == "Ex"] <- "5"
data$ExterCond[data$ExterCond == "Gd"] <- "4"
data$ExterCond[data$ExterCond == "TA"] <- "3"
data$ExterCond[data$ExterCond == "Fa"] <- "2"
data$ExterCond[data$ExterCond == "Po"] <- "1"
data$ExterCond <- as.numeric(data$ExterCond)

#Masonry veneer type

data$MasVnrType <- as.character(data$MasVnrType)
data$MasVnrType[is.na(data$MasVnrType)] <- "None"

#Masonry veneer area (SQFT)

data<-transform(data, MasVnrArea = ifelse(is.na(MasVnrArea), mean(MasVnrArea, na.rm=TRUE), MasVnrArea))

#Basement Height 

data$BsmtQual <- as.character(data$BsmtQual)
data$BsmtQual[data$BsmtQual == "Ex"] <- "5"
data$BsmtQual[data$BsmtQual == "Gd"] <- "4"
data$BsmtQual[data$BsmtQual == "TA"] <- "3"
data$BsmtQual[data$BsmtQual == "Fa"] <- "2"
data$BsmtQual[data$BsmtQual == "Po"] <- "1"
data$BsmtQual[is.na(data$BsmtQual)] <- "0"
data$BsmtQual <- as.numeric(data$BsmtQual)

#Basement overall condition

data$BsmtCond <- as.character(data$BsmtCond)
data$BsmtCond[data$BsmtCond == "Ex"] <- "5"
data$BsmtCond[data$BsmtCond == "Gd"] <- "4"
data$BsmtCond[data$BsmtCond == "TA"] <- "3"
data$BsmtCond[data$BsmtCond == "Fa"] <- "2"
data$BsmtCond[data$BsmtCond == "Po"] <- "1"
data$BsmtCond[is.na(data$BsmtCond)] <- "0"
data$BsmtCond <- as.numeric(data$BsmtCond)

#Basement Exposure to Outside Area (walkout or garden) 

data$BsmtExposure <- as.character(data$BsmtExposure)
data$BsmtExposure[data$BsmtExposure == "Gd"] <- "4"
data$BsmtExposure[data$BsmtExposure == "Av"] <- "3"
data$BsmtExposure[data$BsmtExposure == "Mn"] <- "2"
data$BsmtExposure[data$BsmtExposure == "No"] <- "1"
data$BsmtExposure[is.na(data$BsmtExposure)] <- "0"
data$BsmtExposure <- as.numeric(data$BsmtExposure)

#Basement finished area rating 

data$BsmtFinType1 <- as.character(data$BsmtFinType1)
data$BsmtFinType1[data$BsmtFinType1 == "GLQ"] <- "6"
data$BsmtFinType1[data$BsmtFinType1 == "ALQ"] <- "5"
data$BsmtFinType1[data$BsmtFinType1 == "BLQ"] <- "4"
data$BsmtFinType1[data$BsmtFinType1 == "Rec"] <- "3"
data$BsmtFinType1[data$BsmtFinType1 == "LwQ"] <- "2"
data$BsmtFinType1[data$BsmtFinType1 == "Unf"] <- "1"
data$BsmtFinType1[is.na(data$BsmtFinType1)] <- "0"
data$BsmtFinType1 <- as.numeric(data$BsmtFinType1)

#Basement finished square feet

data<-transform(data, BsmtFinSF1 = ifelse(is.na(BsmtFinSF1), mean(BsmtFinSF1, na.rm=TRUE), BsmtFinSF1))


#Basement finished area rating (if multiple types)

data$BsmtFinType2 <- as.character(data$BsmtFinType2)
data$BsmtFinType2[data$BsmtFinType2 == "GLQ"] <- "6"
data$BsmtFinType2[data$BsmtFinType2 == "ALQ"] <- "5"
data$BsmtFinType2[data$BsmtFinType2 == "BLQ"] <- "4"
data$BsmtFinType2[data$BsmtFinType2 == "Rec"] <- "3"
data$BsmtFinType2[data$BsmtFinType2 == "LwQ"] <- "2"
data$BsmtFinType2[data$BsmtFinType2 == "Unf"] <- "1"
data$BsmtFinType2[is.na(data$BsmtFinType2)] <- "0"
data$BsmtFinType2 <- as.numeric(data$BsmtFinType2)

#Type 2 finished sqft

data$BsmtFinSF2[is.na(data$BsmtFinSF2)] <- 0

# Unfinished sqft of basement area

data$BsmtUnfSF[is.na(data$BsmtUnfSF)] <- 0

#Total basement area

data$TotalBsmtSF[is.na(data$TotalBsmtSF)] <- 0
data<-transform(data, TotalBsmtSF = ifelse(TotalBsmtSF==0, 1, TotalBsmtSF)) # In order to apply log later

#Heating Quality/Condition

data$HeatingQC <- as.character(data$HeatingQC)
data$HeatingQC[data$HeatingQC == "Ex"] <- "5"
data$HeatingQC[data$HeatingQC == "Gd"] <- "4"
data$HeatingQC[data$HeatingQC == "TA"] <- "3"
data$HeatingQC[data$HeatingQC == "Fa"] <- "2"
data$HeatingQC[data$HeatingQC == "Po"] <- "1"
data$HeatingQC <- as.numeric(data$HeatingQC)

#Central A/C

data$CentralAir <- as.character(data$CentralAir)

data$CentralAir[data$CentralAir == "No"] <- "0"
data$CentralAir[data$CentralAir == "Yes"] <- "1"

data$CentralAir <- as.factor(data$CentralAir)


#Electrical System

data<-transform(data, Electrical = ifelse(is.na(Electrical), mode(Electrical), Electrical))

data$Electrical<-as.factor(data$Electrical) # convert variable type char to factor 

#Bathroom

data$BsmtFullBath[is.na(data$BsmtFullBath)] <- 0
data$BsmtHalfBath[is.na(data$BsmtHalfBath)] <- 0

#kitchen Quality 

data$KitchenQual <- as.character(data$KitchenQual)
data$KitchenQual[data$KitchenQual == "Ex"] <- "5"
data$KitchenQual[data$KitchenQual == "Gd"] <- "4"
data$KitchenQual[data$KitchenQual == "TA"] <- "3"
data$KitchenQual[data$KitchenQual == "Fa"] <- "2"
data$KitchenQual[data$KitchenQual == "Po"] <- "1"
data$KitchenQual[is.na(data$KitchenQual)] <- "3"
data$KitchenQual <- as.numeric(data$KitchenQual)

#Home Functionality

data$Functional <- as.character(data$Functional)
data$Functional[data$Functional == "Typ"] <- "8"
data$Functional[data$Functional == "Min1"] <- "7"
data$Functional[data$Functional == "Min2"] <- "6"
data$Functional[data$Functional == "Mod"] <- "5"
data$Functional[data$Functional == "Maj1"] <- "4"
data$Functional[data$Functional == "Maj2"] <- "3"
data$Functional[data$Functional == "Sev"] <- "2"
data$Functional[data$Functional == "Sal"] <- "1"
data$Functional[is.na(data$Functional)] <- "4"
data$Functional <- as.numeric(data$Functional)

#Quality of fireplace

data$FireplaceQu <- as.character(data$FireplaceQu)
data$FireplaceQu[data$FireplaceQu == "Ex"] <- "5"
data$FireplaceQu[data$FireplaceQu == "Gd"] <- "4"
data$FireplaceQu[data$FireplaceQu == "TA"] <- "3"
data$FireplaceQu[data$FireplaceQu == "Fa"] <- "2"
data$FireplaceQu[data$FireplaceQu == "Po"] <- "1"
data$FireplaceQu[is.na(data$FireplaceQu)] <- "0"
data$FireplaceQu <- as.numeric(data$FireplaceQu)

#Location of Garage   

data$GarageType <- as.character(data$GarageType)
data$GarageType[is.na(data$GarageType)] <- "nogarage"
data$GarageType <- as.factor(data$GarageType)

#Year garage was built, added when remodelling happened


data$YearRemodAdd<-as.numeric(data$YearRemodAdd)
data$GarageYrBlt[is.na(data$GarageYrBlt)] <- data$YearRemodAdd


#Interior finish of garage 

data$GarageFinish <- as.character(data$GarageFinish)
data$GarageFinish[is.na(data$GarageFinish)] <- "nogarage"
data$GarageFinish <- as.factor(data$GarageFinish)

# Garage Cars 

data<-transform(data, GarageArea = ifelse(GarageArea==0, 1, GarageArea))

#Quality of Garage  

data$GarageQual <- as.character(data$GarageQual)
data$GarageQual[data$GarageQual == "Ex"] <- "5"
data$GarageQual[data$GarageQual == "Gd"] <- "4"
data$GarageQual[data$GarageQual == "TA"] <- "3"
data$GarageQual[data$GarageQual == "Fa"] <- "2"
data$GarageQual[data$GarageQual == "Po"] <- "1"
data$GarageQual[is.na(data$GarageQual)] <- "0"
data$GarageQual <- as.numeric(data$GarageQual)

#Condition of Garage 

data$GarageCond <- as.character(data$GarageCond)
data$GarageCond[data$GarageCond == "Ex"] <- "5"
data$GarageCond[data$GarageCond == "Gd"] <- "4"
data$GarageCond[data$GarageCond == "TA"] <- "3"
data$GarageCond[data$GarageCond == "Fa"] <- "2"
data$GarageCond[data$GarageCond == "Po"] <- "1"
data$GarageCond[is.na(data$GarageCond)] <- "0"
data$GarageCond <- as.numeric(data$GarageCond)

#Quality of Pool

data$PoolQC <- as.character(data$PoolQC)
data$PoolQC[data$PoolQC == "Ex"] <- "4"
data$PoolQC[data$PoolQC == "Gd"] <- "3"
data$PoolQC[data$PoolQC == "TA"] <- "2"
data$PoolQC[data$PoolQC == "Fa"] <- "1"
data$PoolQC[is.na(data$PoolQC)] <- "0"
data$PoolQC <- as.numeric(data$PoolQC)

#Quality of Fence 

data$Fence <- as.character(data$Fence)
data$Fence[data$Fence == "GdPrv"] <- "4"
data$Fence[data$Fence == "MnPrv"] <- "3"
data$Fence[data$Fence == "GdWo"] <- "2"
data$Fence[data$Fence == "MnWw"] <- "1"
data$Fence[is.na(data$Fence)] <- "0"
data$Fence <- as.numeric(data$Fence)

# Miscellaneous Features
data$MiscFeature <- as.character(data$MiscFeature)
data$MiscFeature[is.na(data$MiscFeature)] <- "None"
data$MiscFeature <- as.factor(data$MiscFeature)

#Type of Sale 

data$SaleType <- as.character(data$SaleType)
data$SaleType[is.na(data$SaleType)] <- "WD"
data$SaleType <- as.factor(data$SaleType)

#Lot Frontage

data<-transform(data, LotFrontage = ifelse(is.na(LotFrontage), mean(LotFrontage, na.rm=TRUE), LotFrontage))

#Utilities

data$Utilities <- as.character(data$Utilities)
data$Utilities[data$Utilities == "AllPub"] <- "1"
data$Utilities[data$Utilities == "NoSeWa"] <- "0"
data$Utilities[is.na(data$Utilities)] <- "0"
data$Utilities <- as.numeric(data$Utilities)

#Garage Area

data<-transform(data, data$GarageArea = ifelse(data$GarageArea==0, 1, data$GarageArea)) # In order to apply log later

#Removing Additional NA's

data[data$Id==2577, "GarageArea"] <- 0
data[data$Id==2577, "GarageCars"] <- 0

#New Variables

data$Year_Qual<- data$YearBuilt*data$OverallQual
data$Remodel_Quality<-data$YearRemodAdd*data$OverallQual
data$Basmt_Qual <-data$OverallQual*data$TotalBsmtSF
data$LivingRoom_Qual<-data$OverallQual*data$GrLivArea
data$Bathroom_Qual<-data$OverallQual*data$FullBath
data$Exterior_Qual<-data$OverallQual*data$ExterCond


#Splitting the data into 3 : test,train, predict  

train.data<-subset(data,Id<=1021)
test.data<-subset(data,Id>1021&Id<1461)
predict.data<-subset(data,Id>=1461)  # This is the competition test data



md.pattern(train.data) # Confirm no missing data found 

# Building Model

fit<- step(lm(log(SalePrice)~log(MSSubClass)*MSZoning+ LotFrontage+ log(LotArea)+ 
           Street+ LotShape+ LandContour+ Utilities+ LotConfig+ 
           LandSlope+ Neighborhood+ Condition1+ BldgType+ 
           HouseStyle+ log(OverallQual)+ OverallCond+ YearBuilt+ YearRemodAdd+ 
           RoofStyle+ MasVnrType+ 
           MasVnrArea+ ExterQual+ ExterCond+ Foundation+ BsmtQual+  
           BsmtExposure+ BsmtFinType1+ BsmtFinSF1+ BsmtFinType2+ BsmtFinSF2+ 
           BsmtUnfSF+ sqrt(TotalBsmtSF)+ HeatingQC+ CentralAir+ 
           log(X1stFlrSF)+ X2ndFlrSF+ LowQualFinSF+ log(GrLivArea)+ BsmtFullBath+ 
           BsmtHalfBath+ FullBath+ HalfBath+ BedroomAbvGr+ KitchenAbvGr+ 
           KitchenQual+ TotRmsAbvGrd+ Functional+ Fireplaces+  
           GarageType+ GarageCars+ log(GarageArea)+ 
           GarageQual+ GarageCond+ PavedDrive+ WoodDeckSF+ OpenPorchSF+ 
           EnclosedPorch+ X3SsnPorch+ ScreenPorch+ PoolArea+ 
           Fence+ MiscVal+ MoSold+ YrSold+ SaleType+
           SaleCondition +Year_Qual+Remodel_Quality+Basmt_Qual+LivingRoom_Qual+
           Bathroom_Qual+Exterior_Qual,data=train.data),direction="both")


#Removed:Heating,Electrical,MiscFeature

# Prediction for Test data
predicted.prices<-exp(predict(fit, test.data)) #use the "fit" model to predict prices for test data

percent.errors <- abs((test.data$SalePrice-predicted.prices)/test.data$SalePrice)*100 #calculate absolute percentage errors

mean(percent.errors) #display MAPE

# Prediction for Predict data

predicted.prices<-exp(predict(fit, predict.data)) #use the "fit" model to predict prices for predict data

write.table(cbind(predict.data$Id,predicted.prices),
            file = "Prediction_Step4.csv",row.names = F,col.names = c("Id","SalePrice"))


write.csv(predicted.prices, file = "comp_test_May1.csv")




summary(data)

getwd()

# Regularization

y<-log(train.data$SalePrice)
X<-model.matrix(Id~log(MSSubClass)*MSZoning+ LotFrontage+ log(LotArea)+ 
                  Street+ LotShape+ LandContour+ Utilities+ LotConfig+ 
                  LandSlope+ Neighborhood+ Condition1+ BldgType+ 
                  HouseStyle+ log(OverallQual)+ OverallCond+ YearBuilt+ YearRemodAdd+ 
                  RoofStyle+ MasVnrType+ 
                  MasVnrArea+ ExterQual+ ExterCond+ Foundation+ BsmtQual+  
                  BsmtExposure+ BsmtFinType1+ BsmtFinSF1+ BsmtFinType2+ BsmtFinSF2+ 
                  BsmtUnfSF+ sqrt(TotalBsmtSF)+ HeatingQC+ CentralAir+ 
                  log(X1stFlrSF)+ X2ndFlrSF+ LowQualFinSF+ log(GrLivArea)+ BsmtFullBath+ 
                  BsmtHalfBath+ FullBath+ HalfBath+ BedroomAbvGr+ KitchenAbvGr+ 
                  KitchenQual+ TotRmsAbvGrd+ Functional+ Fireplaces+  
                  GarageType+ GarageCars+ log(GarageArea)+ 
                  GarageQual+ GarageCond+ PavedDrive+ WoodDeckSF+ OpenPorchSF+ 
                  EnclosedPorch+ X3SsnPorch+ ScreenPorch+ PoolArea+ 
                  Fence+ MiscVal+ MoSold+ YrSold+ SaleType+
                  SaleCondition +Year_Qual+Remodel_Quality+Basmt_Qual+LivingRoom_Qual+
                  Bathroom_Qual+Exterior_Qual,data)[,-1]
X<-cbind(data$Id,X)

X.training<-subset(X,X[,1]<=1021)
X.testing<-subset(X,X[,1]>1021 &X[,1]<1461)
X.predicting<-subset(X,X[,1]>=1461)


# Combination of LASSO & RIDGE (alpha=0.4)
combination<-glmnet(x = X.training, y = y, alpha = 0.4)

par(mfrow=c(1,1))

plot(combination, xvar = "lambda")


#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 0.4) #create cross-validation data
plot(crossval)
penalty.combination <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.combination) #see where it was on the graph
opt.fit <-glmnet(x = X.training, y = y, alpha = 0.4, lambda = penalty.combination) #estimate the model with the optimal penalty
coef(opt.fit) #resultant model coefficients

# predicting the performance on the testing set
combination.testing <- exp(predict(opt.fit, s = penalty.combination, newx =X.testing))
mean(abs(combination.testing-test.data$SalePrice)/test.data$SalePrice*100) #calculate and display MAPE

#Predicting Competition

combination.predicting <- exp(predict(opt.fit, s = penalty.combination, newx =X.predicting))

write.table(cbind(predict.data$Id,combination.predicting),
            file = "Prediction_combination.csv",row.names = F,col.names = c("Id","SalePrice"))

