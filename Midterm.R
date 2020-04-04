
library(ISLR)
library(class)
library(caret)
library(MASS)
library(car)
library(FNN)
library(clusterSim)

#Reading the two data files
CancerData <- read.csv("C:/Users/Hiren/OneDrive/OneDrive - Texas A&M University/TAMU/Spring'20/Data Analysis/HW/Mid-term/CancerData.csv")
CancerHoldoutData <- read.csv("C:/Users/Hiren/OneDrive/OneDrive - Texas A&M University/TAMU/Spring'20/Data Analysis/HW/Mid-term/CancerHoldoutData.csv")

#Problem1

#1A
##Data set analysis

##Explarotary Data Analysis to determine promising variables
summary(CancerData)
par(mfrow=c(2,2))
plot(CancerData$TARGET_deathRate,CancerData$incidenceRate)+plot(CancerData$TARGET_deathRate,CancerData$medIncome)+plot(CancerData$TARGET_deathRate,CancerData$povertyPercent)+plot(CancerData$TARGET_deathRate,CancerData$MedianAge)
plot(CancerData$TARGET_deathRate,CancerData$MedianAgeMale)+plot(CancerData$TARGET_deathRate,CancerData$MedianAgeFemale)+plot(CancerData$TARGET_deathRate,CancerData$AvgHouseholdSize)+plot(CancerData$TARGET_deathRate,CancerData$PercentMarried)
plot(CancerData$TARGET_deathRate,CancerData$PctNoHS18_24)+plot(CancerData$TARGET_deathRate,CancerData$PctHS18_24)+plot(CancerData$TARGET_deathRate,CancerData$PctSomeCol18_24)+plot(CancerData$TARGET_deathRate,CancerData$PctBachDeg18_24)
plot(CancerData$TARGET_deathRate,CancerData$PctPrivateCoverage)+plot(CancerData$TARGET_deathRate,CancerData$PctPublicCoverage)+plot(CancerData$TARGET_deathRate,CancerData$PctPublicCoverageAlone)+plot(CancerData$TARGET_deathRate,CancerData$PctWhite)
plot(CancerData$TARGET_deathRate,CancerData$PctBlack)+plot(CancerData$TARGET_deathRate,CancerData$PctAsian)+plot(CancerData$TARGET_deathRate,CancerData$PctOtherRace)+plot(CancerData$TARGET_deathRate,CancerData$PctMarriedHouseholds)


##In Geography the number of levels are way too many, thereby is is required to work on the variable data to make it useful
length(levels(CancerData$Geography))
##We will reduce the number of levels by replacing counties with states and performing data analysis
CancerData$Geography = as.character(CancerData$Geography)
for ( i in 1:length(CancerData$Geography))
{
  CancerData$Geography[i] = unlist(strsplit(CancerData$Geography[i], split=', ', fixed=TRUE))[2]
}
CancerData$Geography<-factor(CancerData$Geography)
levels(CancerData$Geography)
###The levels have been reduced to 51, thereby it becomes easier to predict data now for reduced levels
###Thereby the data has been cleaned


##In Geography the number of levels are way too many, thereby is is required to work on the variable data to make it useful
length(levels(CancerHoldoutData$Geography))
##We will reduce the number of levels by replacing counties with states and performing data analysis
CancerHoldoutData$Geography = as.character(CancerHoldoutData$Geography)
for ( i in 1:length(CancerHoldoutData$Geography))
{
  CancerHoldoutData$Geography[i] = unlist(strsplit(CancerHoldoutData$Geography[i], split=', ', fixed=TRUE))[2]
}
CancerHoldoutData$Geography<-factor(CancerHoldoutData$Geography)
levels(CancerHoldoutData$Geography)
###The levels have been reduced, thereby it becomes easier to predict data now for reduced levels
###Thereby the data has been cleaned


#1B: Outliers
boxplot(CancerData$MedianAge, main = 'Boxplot for MedianAge' )
##Performance without adressing Outliers
linear.cancer.out = lm(TARGET_deathRate~., data = CancerData)
summary(linear.cancer.out)

##Performance after adressing outlers, i.e. assigning mean values to MedianAge

#For CancerData
##The Age data set also has some issues attached, the below analysis would highlight it better.
max(CancerData$MedianAge)
max(CancerData$MedianAgeFemale)
max(CancerData$MedianAgeMale)
##The above results show a huge discrepancy. It also provides us with some insight about the incorectness of the MedianAge Data set
##The median age data set needs to be searched through, to find possible errors in the data.
sum(CancerData$MedianAge>100)
##This suggests that there are 23 data entries with incorrect/inflated MedianAge
##For these data entries, we will take the average of the MedianAgeFemal and MedianAgeMale
for (i in 1:length(CancerData$MedianAge))
{ 
  if (CancerData$MedianAge[i] > 100)
  {
    CancerData$MedianAge[i] = mean(CancerData$MedianAgeMale[i],CancerData$MedianAgeFemale[i])
  }
}
###In the above part the assignment has been completed
sum(CancerData$MedianAge>100)
###Thereby the data has been cleaned

#ForCancerHoldOut
##The Age data set also has some issues attached, the below analysis would highlight it better.
max(CancerHoldoutData$MedianAge)
max(CancerHoldoutData$MedianAgeFemale)
max(CancerHoldoutData$MedianAgeMale)
##The above results show a huge discrepancy. It also provides us with some insight about the incorectness of the MedianAge Data set
##The median age data set needs to be searched through, to find possible errors in the data.
sum(CancerHoldoutData$MedianAge>100)
##This suggests that there are 23 data entries with incorrect/inflated MedianAge
##For these data entries, we will take the average of the MedianAgeFemal and MedianAgeMale
for (i in 1:length(CancerHoldoutData$MedianAge))
{
  if (CancerHoldoutData$MedianAge[i] > 100)
  {
    CancerHoldoutData$MedianAge[i] = mean(CancerHoldoutData$MedianAgeMale[i],CancerHoldoutData$MedianAgeFemale[i])
  }
}
###In the above part the assignment has been completed
sum(CancerHoldoutData$MedianAge>100)
###Thereby the data has been cleaned for CancerHoldout Data set

##Checking Model performance after assigning mean values to Median Age
linear.cancer.out = lm(TARGET_deathRate~., data = CancerData)
summary(linear.cancer.out)



#1C Missing Values in test Data set

# Model performance before the removing the missing value column
linear.cancer.miss = lm(TARGET_deathRate~., data = CancerData)
summary(linear.cancer.miss)

##Cleaning the CancerData set
###From analyzing the data it can be seen that PctSomeCol18_24 has lots of missing data values, therby a decision has been made to opmit the dataset from analysis
sum(is.na(CancerData$PctSomeCol18_24))
length(CancerData$PctSomeCol18_24)
##From above it can be seem that about 75% of the data is missing, therby rendering the data useless

##Cleaning the CancerHoldoutData set
###From analyzing the data it can be seen that PctSomeCol18_24 has lots of missing data values, therby a decision has been made to opmit the dataset from analysis
sum(is.na(CancerHoldoutData$PctSomeCol18_24))
length(CancerHoldoutData$PctSomeCol18_24)
##From above it can be seem that about 80% of the data is missing, therby rendering the data useless

# Model performance after the removing the missing value column
linear.cancer.miss = lm(TARGET_deathRate~.-PctSomeCol18_24, data = CancerData)
summary(linear.cancer.miss)

#1D Checking the collinearity between variables
##We check this for the model developed above
vif(linear.cancer.miss)
##We remove MedianAge and MedianAgeFemale from the model due to their high VIF values, i.e. VIF>5
linear.cancer.v = lm(TARGET_deathRate~.-PctSomeCol18_24-MedianAgeMale-MedianAgeFemale, data = CancerData)
summary(linear.cancer.v)
vif(linear.cancer.v)



##ProblemSet_2
#The linear model is created based on above resuts
linear.cancer = lm(TARGET_deathRate~.-PctSomeCol18_24-MedianAgeMale-MedianAgeFemale, data = CancerData)
summary(linear.cancer)
par(mfrow=c(2,2))
plot(linear.cancer)


##Significant Variables are medIncome, PctOtherRace, PctMarriedHouseholds, PctBatchDeg18_24, incidenceRate, PctHS18_24, Geography
linear.cancer.significant = lm(TARGET_deathRate~PctOtherRace+medIncome+incidenceRate+PctMarriedHouseholds+PctBachDeg18_24+ Geography+PctHS18_24, data = CancerData)
summary(linear.cancer.significant)
plot(linear.cancer.significant)

##Significant Variables are medIncome, PctOtherRace, PctMarriedHouseholds, PctBatchDeg18_24, incidenceRate, PctHS18_24, Geography (Non-Linear Relation)
linear.cancer.sig.NL = lm(TARGET_deathRate~medIncome*PctOtherRace+incidenceRate+PctMarriedHouseholds*Geography+PctBachDeg18_24+I(PctHS18_24^2), data = CancerData)
summary(linear.cancer.sig.NL)
plot(linear.cancer.sig.NL)

##ProblemSet 3
### Dividing the CancerData DAta set into two parts for KNN prediction
Train.split = sample(nrow(CancerData), 0.7*nrow(CancerData))

### The data has been splitted into two parts
Train.CanD = CancerData[Train.split,]
Test.CanD = CancerData[-Train.split,]

### Since the KNN model is to be performed without the Geography, a dataset is created with all the predictor columns except the Geography, Target_deathrate and PctSomeCol18_24
### This has been done to ease up the work required when writing the cbind() function in the following code for KNN prediction model
Train.CD = data.frame(Train.CanD[1:7], Train.CanD[9:12], Train.CanD[14:22])
Test.CD = data.frame(Test.CanD[1:7], Test.CanD[9:12], Test.CanD[14:22])


###Normalizing Data
data.normalize <- function(n,t){    u <- ((n-min(t))/(max(t)-min(t)))
                                          return(u)
                                          }
for(j in 1:length(Train.CD))
{
for (i in 1:length(Train.CanD$incidenceRate)) {
  
  Train.CD[i,j]<-data.normalize(Train.CD[i,j],Train.CD[j])
  
}
}
for(j in 1:length(Test.CD))
{
  for (i in 1:length(Test.CanD$incidenceRate)) {
    
    Test.CD[i,j]<-data.normalize(Test.CD[i,j],Test.CD[j])
    
  }
}

##SPlitting data for KNN
Train.CD1 = data.frame(Train.CD[2:length(Train.CD)])
Test.CD1 = data.frame(Test.CD[2:length(Test.CD)])
train.y = Train.CD$TARGET_deathRate 
test.y = Test.CD$TARGET_deathRate

###The model is prepared for KNN prediction
train.X = cbind(Train.CD1)#training data of predictors
test.X = cbind(Test.CD1)#testing data of predictors

##Evaluating the KNN model for k=5,10,15,20,25
set.seed(2)
mse<-rnorm(5) 
n<-c(5,10,15,20,25)
for (i in 1:5)
{
  knn.pred = knn.reg(train.X,test.X,train.y,k=n[i])
  mse[i] = mean((test.y-knn.pred$pred)^2)
}
par(mfrow=c(1,1))
plot(n,mse,'b',xlab = "Value of K", ylab = "Test Accuracy")
mse


##ProblemSet 3 KNN model for significant variables

##PctBatchDeg18_24, incidenceRate, PctHS18_24
train.significant = data.frame(Train.CD$medIncome,Train.CD$PctOtherRace,Train.CD$incidenceRate,Train.CD$PctHS18_24,Train.CD$PctBachDeg18_24,Train.CD$PctMarriedHouseholds)
test.significant = data.frame(Test.CD$medIncome,Test.CD$PctOtherRace,Test.CD$incidenceRate,Test.CD$PctHS18_24,Test.CD$PctBachDeg18_24,Test.CD$PctMarriedHouseholds)
train.Xs=cbind(train.significant)
test.Xs=cbind(test.significant)
train.y = Train.CD$TARGET_deathRate
test.y = Test.CD$TARGET_deathRate

#Evaluating KNN model
set.seed(2)
mse.significant<-rnorm(5) 
n<-c(5,10,15,20,25)
for (i in 1:5)
{
  knn.pred.s = knn.reg(train.Xs,test.Xs,train.y,k=n[i])
  mse.significant[i] = mean((test.y-knn.pred.s$pred)^2)
}

plot(n,mse.significant,'b',xlab = "Value of K", ylab = "Test Accuracy")
mse.significant

#5
##Performing for non-normalized data set
##MSE for the holdout on LR
par(mfrow=c(1,1))

CancerData.LR <- data.frame(CancerData[1:7],CancerData[9:12],CancerData[14:22])
CancerHoldoutData.LR<-data.frame(CancerHoldoutData[1:7],CancerHoldoutData[9:12],CancerHoldoutData[14:22])
linear.mse.holdout = lm(TARGET_deathRate~.,data = CancerData.LR)
pred.mse.linear = predict(linear.mse.holdout,CancerHoldoutData.LR)
pred.mse.linear
CancerHoldoutData.LR$TARGET_deathRate
mean((CancerHoldoutData.LR$TARGET_deathRate- pred.mse.linear)^2)

##MSE for the holdout on KNN
##We need to remove geography for KNN

length(CancerData)
CancerData1 <- data.frame(CancerData[1:7],CancerData[9:12],CancerData[14:22])
CancerHoldoutData1=data.frame(CancerHoldoutData[1:7],CancerHoldoutData[9:12],CancerHoldoutData[14:22])
length(CancerHoldoutData1)
mse.holdout<-rnorm(5) 

train.Xm = cbind(CancerData1[2:length(CancerData1)])#training data of predictors
test.Xm = cbind(CancerHoldoutData1[2:length(CancerHoldoutData1)])#testing data of predictors
mse.holdout<-0
n<-c(5,10,15,20,25)
for (i in 1:5)
{
  knn.pred.holdout = knn.reg(train.Xm,test.Xm,CancerData1$TARGET_deathRate,k=n[i])
  mse.holdout[i] = mean((CancerHoldoutData1$TARGET_deathRate-knn.pred.holdout$pred)^2)
}

plot(n,mse,'b',xlab = "Value of K", ylab = "Test Accuracy")
mse.holdout


##Normalizing data for knn AND LR

CancerDataN <- data.frame(CancerData[1:7],CancerData[9:12],CancerData[14:22])
CancerHoldoutDataN=data.frame(CancerHoldoutData[1:7],CancerHoldoutData[9:12],CancerHoldoutData[14:22])
n<-c(5,10,15,20,25)
for(j in 1:length(CancerDataN))
{
  for (i in 1:length(CancerDataN$incidenceRate)) {
    
    CancerDataN[i,j]<-data.normalize(CancerDataN[i,j],CancerDataN[j])
    
  }
}

##The Data can be normalized for CancerHoldout
for(j in 1:length(CancerHoldoutDataN))
{
  for (i in 1:length(CancerHoldoutDataN$incidenceRate)) {
    
    CancerHoldoutDataN[i,j]<-data.normalize(CancerHoldoutDataN[i,j],CancerHoldoutDataN[j])
    
  }
}


###LR Model based on normalized data
linear.mse.holdout.N = lm(TARGET_deathRate~.,data = CancerDataN)
pred.mse.linear.N = predict(linear.mse.holdout.N,CancerHoldoutDataN)
mean((CancerHoldoutDataN$TARGET_deathRate- pred.mse.linear.N)^2)

#KNN based on normalized data
train.Xm.N = cbind(CancerDataN[2:length(CancerDataN)])#training data of predictors
test.Xm.N = cbind(CancerHoldoutDataN[2:length(CancerHoldoutDataN)])#testing data of predictors
mse.norm<-0
n<-c(5,10,15,20,25)
for (i in 1:5)
{
  knn.pred.holdout.N = knn.reg(train.Xm.N,test.Xm.N,CancerDataN$TARGET_deathRate,k=n[i])
  mse.norm[i] = mean((CancerHoldoutDataN$TARGET_deathRate-knn.pred.holdout.N$pred)^2)
}
mse.norm
