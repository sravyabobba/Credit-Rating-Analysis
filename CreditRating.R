#Reading th csv File
CRating=read.csv("CreditRating.csv")

str(CRating)
summary(CRating)
######################################################################################
#                              Data Cleansing                                        #
######################################################################################
#Filling missing data
#Removing records with Credit Rating as N/A
CRating = CRating[c(!is.na(CRating$CreditRating)),]
summary(CRating)

#Removing Records with MonthlyIncome as N/A
CRating <- CRating[!is.na(CRating$MonthlyIncome),]
summary(CRating)

#Removing Records with MonthlyPayment as N/A
CRating <- CRating[!is.na(CRating$MonthlyPayment),]
summary(CRating)

#When Age is N/A , It is replaced by Median age
CRating$Age <- ifelse(is.na(CRating$Age) ,44, CRating$Age)
summary(CRating)

#If available income is N/A, It is changed to 0, else the difference between the Montly income and Monthly Expense 
CRating$AvailableIncome <- ifelse(is.na(CRating$AvailableIncome),0, CRating$AvailableIncome)
summary(CRating)

#Changing Credit Rating values to either 0 or 1 for being binomail
CRating$CreditRating <- ifelse(CRating$CreditRating <= 2, 0, 1)
summary(CRating)

#Factorizing Credit Rating
CRating$CreditRating <- as.factor(CRating$CreditRating)

#Changing PerCapitaAvailableIncome from n/a to zero
CRating$PerCapitaAvailableIncome <- ifelse(is.na(CRating$PerCapitaAvailableIncome),0, CRating$PerCapitaAvailableIncome)
summary(CRating)

#Changing Participates in community to median value (2)
CRating$ParticipatesInCommunity <- ifelse(is.na(CRating$ParticipatesInCommunity), 2, CRating$ParticipatesInCommunity)
summary(CRating)

#Changing Social Fabric to median value (3)
CRating$SocialFabric <- ifelse(is.na(CRating$SocialFabric),3, CRating$SocialFabric)
summary(CRating)

################################################################################
#                          CORRELATIONS                                        #
################################################################################
cor(CRating$Age, CRating$LoanSizeAvg)
#-0.03325967
cor(CRating$MonthlyIncome, CRating$MonthlyPayment)
#0.514389
cor(CRating$PerCapitaAvailableIncome, CRating$UpfrontPaymentAvg)
#0.1942766
cor(CRating$MonthlyExpenses, CRating$LoanSizeAvg)
#0.7225472


#######################################################################################
                               # PLOTS #
#######################################################################################

#plotting the graphs 

plot(CRating$MonthlyIncome , CRating$MonthlyExpenses , xlab = "Monthly Income" , ylab = "Monthly Expense")
plot(CRating$Age, CRating$LoanSizeAvg , xlab = "Age", ylab = "Loan Size Avg")

#Identifying the outliers
Outliers= subset(CRating, Age>80 | LoanSizeAvg > 40000)

#Removing Outiliers
#CRating<- CRating [-Outliers,]
# #> CRating<- CRating [-Outliers,]
# Error in data.frame(value, row.names = rn, check.names = FALSE, check.rows = FALSE) : 
#   row names supplied are of the wrong length

#Removing Outliers
CRating= CRating[!(CRating$Age > 80 | CRating$LoanSizeAvg > 40000 ), ]

#Histograms
hist(CRating$LoanPeriod , xlab = "Loan Period" ,ylab = "Frequency" , main = "Histogram of Loan Period" )
hist(CRating$LoanSizeAvg , xlab = "Loan Size Avg" , ylab = "Frequency", main= " Histogram of Loan Size Avg")
hist(CRating$Age , xlab = "Age" ,ylab="Frequency" , main = "Histogram of Age")
hist(CRating$MonthlyPayment, xlab = "Monthly Payment" , ylab = "Frequency" , main="Histogram of Monthly Payment")

#BoxPlots
boxplot(CRating$MonthlyIncome ~ CRating$LoanPeriodMonths , xlab="Monthly Income" , ylab="'Loan Period Months" )
boxplot(CRating$Age ~ CRating$MonthlyPayment , xlab="Age", ylab="MonthlyPayment")
boxplot(CRating$Age ~ CRating$LoanPeriodMonths , xlab ="Age" , ylab="Loan Period Months")

######################################################################################
#                             Independent Variable Selection                         #
######################################################################################

#Removing independent variales which are not required
CRating$AvailableIncome<- NULL
CRating$PerCapitaAvailableIncome <- NULL
CRating$User.ID <- NULL

summary(CRating)
# Sex          Age          FamilySize    YearsAtThisHome  MainHousehold    MonthlyIncome  
# Femenino :52   Min.   :19.00   Min.   :1.000   Min.   : 1.000   Min.   :0.0000   Min.   :  300  
# Masculino:78   1st Qu.:35.00   1st Qu.:3.000   1st Qu.:10.000   1st Qu.:1.0000   1st Qu.: 1200  
# Median :44.00   Median :4.000   Median :10.000   Median :1.0000   Median : 3000  
# Mean   :45.67   Mean   :4.023   Mean   : 9.304   Mean   :0.8692   Mean   : 3463  
# 3rd Qu.:55.00   3rd Qu.:5.000   3rd Qu.:10.000   3rd Qu.:1.0000   3rd Qu.: 5000  
# Max.   :80.00   Max.   :9.000   Max.   :10.000   Max.   :1.0000   Max.   :20000  
# MonthlyExpenses ParticipatesInCommunity  SocialFabric    LoanOpinion     TotalPaidAvg    LoanSizeAvg   
# Min.   :  250   Min.   :0.000           Min.   :2.000   Min.   :3.000   Min.   : 1800   Min.   : 1800  
# 1st Qu.: 1000   1st Qu.:1.000           1st Qu.:2.000   1st Qu.:4.000   1st Qu.: 2082   1st Qu.: 1800  
# Median : 2000   Median :2.000           Median :3.000   Median :4.000   Median : 3565   Median : 3282  
# Mean   : 2404   Mean   :1.608           Mean   :2.715   Mean   :3.946   Mean   : 5674   Mean   : 5320  
# 3rd Qu.: 3000   3rd Qu.:2.000           3rd Qu.:3.000   3rd Qu.:4.000   3rd Qu.: 7712   3rd Qu.: 7650  
# Max.   :10000   Max.   :2.000           Max.   :3.000   Max.   :5.000   Max.   :39997   Max.   :35000  
# UpfrontPaymentAvg LoanPeriodMonths MonthlyPayment    CreditRating
# Min.   :  200     Min.   : 1.000   Min.   :   80.0   0:60        
# 1st Qu.:  500     1st Qu.: 1.250   1st Qu.:  202.5   1:70        
# Median : 1354     Median : 3.500   Median :  777.5               
# Mean   : 2365     Mean   : 5.938   Mean   : 1520.0               
# 3rd Qu.: 3000     3rd Qu.:12.000   3rd Qu.: 1500.0               
# Max.   :15000     Max.   :12.000   Max.   :31994.0   

################################################################################
                              #Logistic Regression#
################################################################################
#installing the caTools package
#install.packages("caTools")
#loading the package
library(caTools)
#splitting the data into training and testing set based on Violator, 67% of the data in training set
set.seed(88)
split = sample.split(CRating$CreditRating, SplitRatio = 0.67)
Ratingtrain = subset(CRating, split == TRUE)
Ratingtest = subset(CRating, split == FALSE)

str(Ratingtrain)
str(Ratingtest)

####Starting the logistic regression#######

mod1 <- glm(CreditRating ~ . , data=Ratingtrain, family = "binomial")
summary(mod1)

#Removing independent variable 'MainHousehold (0.9575)' being most insignificant
mod2 <- glm(CreditRating ~ . -MainHousehold , data=Ratingtrain, family = "binomial")
summary(mod2)

#Removing independent variable 'FamilySize (0.8876)' being most insignificant
mod3 <- glm(CreditRating ~ . -FamilySize -MainHousehold, data=Ratingtrain, family = "binomial")
summary(mod3)

#Removing independent variable 'YearsAtThisHome (0.7158)' being most insignificant
mod4 <- glm(CreditRating ~ . -FamilySize -MainHousehold -YearsAtThisHome , data=Ratingtrain, family = "binomial")
summary(mod4)

#Removing independent variable 'Age (0.7052)' being most insignificant
mod5 <- glm(CreditRating ~ . -FamilySize -Age -MainHousehold -YearsAtThisHome , data=Ratingtrain, family = "binomial")
summary(mod5)

#Removing independent variable 'Sex (0.5453)' being most insignificant
mod6 <- glm(CreditRating ~ . -FamilySize -Age -MainHousehold -YearsAtThisHome -Sex, data=Ratingtrain, family = "binomial")
summary(mod6)

#Removing independent variable 'ParticipatesInCommunity (0.5889)' being most insignificant
mod7 <- glm(CreditRating ~ . -FamilySize -Age -MainHousehold -YearsAtThisHome -Sex -ParticipatesInCommunity , data=Ratingtrain, family = "binomial")
summary(mod7)

#Now we see that 'MonthlyIncome' and 'MonthlyExpenses' are most insignificant, thus we would not remove more variables.
#This may cause overfitting of model.

#Predicting accuracy on Training dataset with each model (intermediate stages)
PredictTrain1 =  predict(mod2,type = "response")
table(Ratingtrain$CreditRating, PredictTrain1 >0.5)
#    FALSE TRUE
# 0    24   16
# 1    13   34
(24+34)/(24+16+13+34)
#Accuracy 0.6666667 = 66.67%

PredictTrain2 =  predict(mod3,type = "response")
table(Ratingtrain$CreditRating, PredictTrain2 >0.5)
#    FALSE TRUE
# 0    25   15
# 1    13   34
(25+34)/(25+15+13+34)
#Accuracy 0.6781609 = 67.81%

PredictTrain3 =  predict(mod4,type = "response")
table(Ratingtrain$CreditRating, PredictTrain3 >0.5)
#    FALSE TRUE
# 0    24   16
# 1    13   34
(24+34)/(24+16+13+34)
#Accuracy 0.6666667 = 66.67%

PredictTrain4 =  predict(mod5,type = "response")
table(Ratingtrain$CreditRating, PredictTrain4 >0.5)
#     FALSE TRUE
# 0    22   18
# 1    14   33
(22+33)/(22+18+14+33)
#Accuracy 0.6321839 = 63.21%

PredictTrain5 =  predict(mod6,type = "response")
table(Ratingtrain$CreditRating, PredictTrain5 >0.5)
#    FALSE TRUE
# 0    27   13
# 1    15   32
(27+32)/(27+13+15+32)
#Accuracy 0.6781609 = 67.81%

PredictTrain6 =  predict(mod7,type = "response")
table(Ratingtrain$CreditRating, PredictTrain6 >0.5)
#    FALSE TRUE
# 0    24   16
# 1    15   32
(24+32)/(24+16+15+32)
#Accuracy 0.6436782 = 64.36%

#Testing model on test data with all models (intermdediate stages)
PredictTest1 =  predict(mod2,type = "response", newdata = Ratingtest)
table(Ratingtest$CreditRating, PredictTest1 >0.5)
#     FALSE TRUE
# 0    15    5
# 1     6   17
(15+17)/(15+5+6+17)
#Accuracy 0.744186 = 74.41%

PredictTest2 =  predict(mod3,type = "response", newdata = Ratingtest)
table(Ratingtest$CreditRating, PredictTest2 >0.5)
#     FALSE TRUE
# 0    15    5
# 1     6   17
(15+17)/(15+5+6+17)
#Accuracy 0.744186 = 74.41%

PredictTest3 =  predict(mod4,type = "response", newdata = Ratingtest)
table(Ratingtest$CreditRating, PredictTest3 >0.5)
#     FALSE TRUE
# 0    15    5
# 1     6   17
(15+17)/(15+5+6+17)
#Accuracy 0.744186 = 74.41%

PredictTest4 =  predict(mod5,type = "response", newdata = Ratingtest)
table(Ratingtest$CreditRating, PredictTest4 >0.5)
#     FALSE TRUE
# 0    15    5
# 1     6   17
(15+17)/(15+5+6+17)
#Accuracy 0.744186 = 74.41%

PredictTest5 =  predict(mod6,type = "response", newdata = Ratingtest)
table(Ratingtest$CreditRating, PredictTest5 >0.5)
#     FALSE TRUE
# 0    15    5
# 1     6   17
(15+17)/(15+5+6+17)
#Accuracy 0.744186 = 74.41%

#Final prediction with mod7
PredictTest6 =  predict(mod7,type = "response", newdata = Ratingtest)
table(Ratingtest$CreditRating, PredictTest6 >0.5)
#     FALSE TRUE
# 0    15    5
# 1     6   17
(15+17)/(15+5+6+17)
#Accuracy 0.744186 = 74.41%

#Changing thersold to 0.7 (Just to check shift from current results)
table(Ratingtest$CreditRating, PredictTest6 >0.7)
#     FALSE TRUE
# 0    18    2
# 1     7   16
(18+16)/(18+2+7+16)
#Accuracy 0.7906977 = 79.06%

#Changing thersold to 0.7 (Just to check shift from current results)
table(Ratingtest$CreditRating, PredictTest6 >0.2)
#     FALSE TRUE
# 0     4   16
# 1     2   21
(4+21)/(4+16+2+21)
#Accuracy 0.5813953 = 58.13%

#Comparison of accuracy with Baseline model
table(Ratingtest$CreditRating)
# 0  1 
# 20 23 
(0+23)/(0+1+20+23)
#Accuracy = 0.5227273 = 52.27% < 74.41% (Calculated using mod7 on test data with thersold .5)

#install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(PredictTrain6, Ratingtrain$CreditRating)
ROCCurve = performance(ROCRpred, "tpr" , "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoofs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred, "auc")@y.values)
#Area under curve is 0.7696809

#########################################################################################
                       # Classification and Regression trees #
#########################################################################################

#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)

#CART model
CRTree = rpart(CreditRating ~ . -FamilySize -Age -MainHousehold -YearsAtThisHome -Sex 
               -ParticipatesInCommunity , data = Ratingtrain, method="class", minbucket=15)
prp(CRTree)
rpart.plot(CRTree, tweak = 1.0)
print(CRTree)

#install.packages("randomForest")
library(randomForest)

#Make predictions using Tree
PredictCRating = predict(CRTree, newdata = Ratingtest, type = "class")
confusionMatrix(PredictCRating, Ratingtest$CreditRating)
table(Ratingtest$CreditRating, PredictCRating)
(12+16)/(12+8+7+16)

#Testing different values of minbuckets
#accuracy with minbucket 5 = 0.6977 
#accuracy with minbucket 10 = 0.5814    
#accuracy with minbucket 15 = 0.6512 
#accuracy with minbucket 20 = 0.6512
#accuracy with minbucket 22 = 0.6977
#accuracy with minbucket 25 = 0.7209  
#accuracy with minbucket 30 = 0.5349   

#Choosing minbucket as 15, having good accuracy and not very complex structure

#Build randomForest model
CRForest = randomForest(as.factor(CreditRating) ~ . -FamilySize -Age -MainHousehold 
                                                    -YearsAtThisHome -Sex 
                                                    -ParticipatesInCommunity,
                                                    data = Ratingtrain, 
                                                    ntree=200, nodesize=20 )



#Make predictions using randomForest with ntree as 200 and nodesize as 20
PredictForest = predict(CRForest, newdata = Ratingtest)
confusionMatrix(PredictForest,Ratingtest$CreditRating)
table(as.factor(Ratingtest$CreditRating), PredictForest)
# PredictForest
#   0  1
# 0 14  6
# 1  9 14
(14+14)/(14+6+9+14)
#Accuracy = 0.6511628 = 65.12%

varImpPlot(CRForest)

#Cross validation

#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross-validation
train(as.factor(CreditRating) ~ . -FamilySize -Age -MainHousehold -YearsAtThisHome -Sex 
                                  -ParticipatesInCommunity, data = Ratingtrain, 
                                  method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

# CART 
# 
# 87 samples
# 15 predictors
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 78, 78, 79, 78, 78, 79, ... 
# Resampling results across tuning parameters:
#   
#   cp    Accuracy   Kappa       
# 0.01  0.5541667   0.108457837
# 0.02  0.5541667   0.108457837
# 0.03  0.4944444  -0.005339667
# 0.04  0.4944444  -0.005339667
# 0.05  0.4944444   0.007790542
# 0.06  0.4944444   0.011167653
# 0.07  0.5166667   0.057920899
# 0.08  0.5166667   0.057920899
# 0.09  0.5388889   0.105063756
# 0.10  0.5388889   0.105063756
# 0.11  0.5388889   0.105063756
# 0.12  0.6208333   0.274172013
# 0.13  0.6208333   0.274172013
# 0.14  0.6208333   0.274172013
# 0.15  0.6208333   0.274172013
# 0.16  0.6208333   0.274172013
# 0.17  0.6208333   0.274172013
# 0.18  0.6208333   0.274172013
# 0.19  0.6208333   0.274172013
# 0.20  0.6208333   0.274172013
# 0.21  0.6208333   0.274172013
# 0.22  0.6208333   0.274172013
# 0.23  0.6208333   0.274172013
# 0.24  0.6208333   0.274172013
# 0.25  0.6208333   0.274172013
# 0.26  0.6208333   0.274172013
# 0.27  0.6208333   0.274172013
# 0.28  0.5652778   0.104495179
# 0.29  0.5652778   0.104495179
# 0.30  0.5652778   0.104495179
# 0.31  0.5166667  -0.004651163
# 0.32  0.5166667  -0.004651163
# 0.33  0.5166667  -0.004651163
# 0.34  0.5166667  -0.004651163
# 0.35  0.5166667  -0.004651163
# 0.36  0.5166667  -0.004651163
# 0.37  0.5388889   0.000000000
# 0.38  0.5388889   0.000000000
# 0.39  0.5388889   0.000000000
# 0.40  0.5388889   0.000000000
# 0.41  0.5388889   0.000000000
# 0.42  0.5388889   0.000000000
# 0.43  0.5388889   0.000000000
# 0.44  0.5388889   0.000000000
# 0.45  0.5388889   0.000000000
# 0.46  0.5388889   0.000000000
# 0.47  0.5388889   0.000000000
# 0.48  0.5388889   0.000000000
# 0.49  0.5388889   0.000000000
# 0.50  0.5388889   0.000000000
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was cp = 0.27. 

#Highest accuracy is 62.08% with cp being 0.27 and 
#we will take .28 as new cp value to build another model.

# Create a new CART model
CRTreeCV = rpart(as.factor(CreditRating) ~ . -FamilySize -Age -MainHousehold 
                                            -YearsAtThisHome -Sex -ParticipatesInCommunity,
                                            data = Ratingtrain, method="class", cp = 0.28)
prp(CRTreeCV)
rpart.plot(CRTreeCV)
print(CRTreeCV)
summary(CRTreeCV)


# Make predictions
PredictCV = predict(CRTreeCV, newdata = Ratingtest, type = "class")
confusionMatrix(PredictCV, Ratingtest$CreditRating)
MyTab = table(as.factor(Ratingtest$CreditRating), PredictCV)
MyTab
# PredictCV
#   0  1
# 0 17  3
# 1 11 12

AccuCV = (MyTab[1,1]+ MyTab[2,2])/sum(MyTab)
AccuCV
# Accuracy = 0.6744186 = 67.44%

#Comparison of accuracy with Baseline model
table(Ratingtest$CreditRating)
# 0  1 
# 20 23 
(0+23)/(0+1+20+23)
#Accuracy = 0.5227273 = 52.27% < 67.44% (Calculated using cross-validation)