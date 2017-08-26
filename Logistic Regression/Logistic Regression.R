# Direct Marketting Data Set
dm <- read.csv("C:\\Users\\mainak\\Desktop\\DADAI\\Data-Science\\Data Science With R\\Logistic Regression\\dm.csv")
library(dplyr)
library(ggplot2)
# Purpose of this model is to come up with a process to identify good customers, 
#identify customer id's who are considered good 


#Customer who Amount Spent is greaer than the mean is considered as good customer
#Creating a variable Binary variable target comprising of 1s and 0s
dm%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm
dm%>%select(-AmountSpent)->dm
summary(dm)

####################################################################################################################
#Minimal Data Prep

dm$History1<-ifelse(is.na(dm$History),"Missing",as.character(dm$History))
dm$History1<-as.factor(dm$History1)

dm$Children<-as.factor(dm$Children)
dm$Catalogs<-as.factor(dm$Catalogs)

dm<-dm[,-8] # removing History variable consisting of Nas

#################################################################################################################
#Splitting the data set into test and training samples
  #1, Usng Sample() function
  set.seed(200)
  index<-sample(nrow(dm),0.70*nrow(dm),replace=F)
  train<-dm[index,] # 700/10
  test<-dm[-index,] # 300/10
  
  table(train$Target)/nrow(train)
  table(test$Target)/nrow(test)

  #2. Using createDataPartition() function from Caret package
  library(caret)
  index_caret<-createDataPartition(y=dm$Target,times = 1,p=0.70,list=F)
  train_caret<-dm[index_caret,] # 700/10
  test_caret<-dm[-index_caret,] # 300/10
  
  table(train_caret$Target)/nrow(train_caret)
  table(test_caret$Target)/nrow(test_caret)

# As we can see both the function partitioned/splitted the data in equals i.e 70:30
#########################################################################################################################
  
#Build the first model using all the variables from train data set
#glm() is used to build logistic model.(Generalised Linear Model)
mod<-glm(Target~.,data=train[,9],family="binomial")
summary(mod)  
  
#Lets do a step wise model prediction to determine significant variable
step(mod,direction = "both")  
#Age + Location + Salary + Children + Catalogs + History1

#---------------------------------------------------------------------------------------------#

#Lets build the new model with significant variables given by step() function
mod1<-glm(formula = Target ~ Age + Location + Salary + Children + Catalogs + History1, family = "binomial", data = train)  
summary(mod1)  

#Creating dummies for Age('Young'), History('Medium') and Children(2 and 3) in train and test data sets

train$AgeYoung_d<-ifelse(train$Age=="Young",1,0)

train$Hist.Mid_d<-ifelse(train$History1=="Medium",1,0)

train$Children2_d<-ifelse(train$Children=="2",1,0)

train$Children3_d<-ifelse(train$Children=="3",1,0)

test$AgeYoung_d<-ifelse(test$Age=="Young",1,0)

test$Hist.Mid_d<-ifelse(test$History1=="Medium",1,0)

test$Children2_d<-ifelse(test$Children=="2",1,0)

test$Children3_d<-ifelse(test$Children=="3",1,0)  

#--------------------------------------------------------------------------------------------------#
#lets build the model with dummy variables
mod2<-glm(Target~AgeYoung_d+Location+Salary+Children3_d+Children2_d+Catalogs+Hist.Mid_d,data=train,family="binomial")
summary(mod2)
#Notes
#1. Every IDV has low p-value ie less than significant level
#2. Residual deviance is quite less than Null deviance(No predictor values)
#3. AIC value is also low.
#######################################################################################################################################

#Model Validation

# 1. Checking confidence Intervals
confint(mod2) 
# As we can see every IDV has narrower confidence interval  

#------------------------------------------------------------------------------------------------#

#2. Predictions with typr="response" for test data set
pred<-predict(mod2,type="response",newdata=test)
#predict function outputs predicted probabilities when type="response" is given.
# We need to change the predicted probabilities to actual class labels

table(dm$Target)/nrow(dm) # 0 - 0.601, 1 - 0.399
# So customer greater than 0.399 belongs to good customer
pred<-ifelse(pred>=0.399,1,0) # Setting 0.399 as cutoff value for good customer

#------------------------------------------------------------------------------------------------#

#3. Kappa Metric - kappa2() requires a data frame of the target variable(actual class labels) 
#and predicted class labels
library(irr) #kappa2() belongs to irr library
kappa2(data.frame(test$Target,pred))
#Kappa = 0.746  

#--------------------------------------------------------------------------------------------------#

#4. Confusion Matrix - Confusion matrix needs predicted class labels and actual class labels and
#which class label to be treated as positive class label.
library(caret)
confusionMatrix(pred,test$Target,positive="1") # Belongs to caret library
#O/p:             Reference
#Prediction      0   1
#            0  158  13
#            1  24   105

#--------------------------------------------------------------------------------------------------#

#5 ROC curve analysis
library(ROCR)
#Claculate prediction of ROCR library by passing actual and predicted class labels
prediction<-prediction(test$Target,pred)

# Calculate performance by passing true positve rate and false positive rate using character label
# "tpr" and "fpr"
perf<-performance(prediction,"tpr","fpr")

#plot the perf ie ROC curve and compare by drawing a 45 degree line
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")
#As we can see the ROC curve is way higher than 45 degrees which implies it is a good classifier

# Calcualte the performance by passing auc parameter
auc<-performance(prediction,"auc")
auc
unlist(auc@y.values)
#auc@y value = 0.868 

#-----------------------------------------------------------------------------------------------------#
#Gains Chart - gains chart is prepared to for customer targeting
# First parameter to gains chart is actual class labels.
# Second parameter to gains chart is probability scores from test data set
# Third parameter is the number of groups
library(gains)
gains(test$Target,predict(mod2,type="response",newdata=test),groups = 10)
test$prob<-predict(mod2,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

targeted_Customer_IDS<-test[test$prob>0.732602471&test$prob<=0.999747759,"Cust_Id"]
#subset the data using the probability scores obtained from above quantiles
#and target the customer IDs for good customers.

  
