##Classification Trees using rpart()##
library(dplyr)
library(irr)   # Used for determining Kappa Matrix
library(rpart) # Used for building Decision tree classification model
library(caret) # Used bor building confussion Matrix
#Tree plotting
library(rattle)
library(rpart.plot)
library(RColorBrewer)



dm<-read.csv("C:\\Users\\mainak\\Desktop\\DADAI\\Data-Science\\Data Science With R\\Decision Tree\\dm.csv")
#The way we defined the good customer and bad customer is based on the fact, how much they spend relative
#to the average expenditure of the total population.If a person spends more than the average expenditure
#of the total population, then he is considered as a good customer, and if a person spends less than the
#average expenditure of total population, he is considered as a bad customer.

dm%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm
dm%>%select(-AmountSpent)->dm

#Minimal Data Prep
str(dm)

#Renaming nas with Missing category for History variable
dm$History1<-as.factor(ifelse(is.na(dm$History),"Missing",as.character(dm$History)))
summary(dm$History1)

#Converting children and Catalogs variables to factor variables
dm$Children<-as.factor(dm$Children)
dm$Catalogs<-as.factor(dm$Catalogs)

# Removing history variable from dataset
dm<-dm[,-8]
# Removing custID variable from dataset
dm<-dm[,-9]


################################################################################################################################################### 
#2. decision Tree building using rpart() and plotting the tree using fancyRpartPlot()

# Lets build decision tree classsifier for the direct marketting dataset
#Here we are using method="class" for building classification tree. Alternatively we can use method="anova" for building regression tree

mod<-rpart(Target~.,data=dm,control=rpart.control(cp=0.002,maxdepth=7),method="class",parms=list(split="gini"))

# Lets plot the classification tree using baseR plot method.
plot(mod, margin=0.1, main="Classification Tree for Direct Marketing")
text(mod, use.n=TRUE, all=TRUE, cex=.7)
# As we can see the plot is not clear to make classifications

# Lets use an advanced visualization plot belonging to RCOlorBrewer
fancyRpartPlot(mod)

printcp(mod)
plotcp(mod, minline = TRUE)
#As we can see from the screeplot goes underneath this dotted line at a value of 0.035 and this is the ‘cp’ value or the cost complexity value.

#So lets prune the tree using prune() function by providing cp value of 0.35
mod1<-prune(mod,cp= 0.035)

fancyRpartPlot(mod1)

################################################################################################################################################
#3.Rules derivation

mod1
#node4
#if history1={Low,Medium,Missing} and Salary < 58650, then 0 (bad) 
#If History1={low,medium,missing}.... 

##################################################################################################################
# 4. Model Validation using various performance matrix.

    # 4.1 Confusion Matrix
    #We need to calculate the confusion matrix between actual and predicted.Actual will be the target variable.
    
    actual<-dm$Target
    predicted<-predict(mod1,type = "class")
    #predict function will produce actual class labels
    
    head(predicted)
    head(as.numeric(predicted))
    predicted<-as.numeric(predicted)#convert factor to integer
    predicted<-ifelse(predicted==2,1,0)# 2 represents 1 AND 1 represents 0 after numeric conversion.
    
    confusionMatrix(predicted,actual,positive="1")
    
    #Output of confusion matrix
    #0 - Bad customer
    #1 - Good customer
    #          0     1
    #0         552   71
    #1         49    328
    
    #Now we can see that model is predicting 552 customers as bad customers and misclassifying 49 bad customers as good customers
    #also model is classifying 328 good customers as good customers and misclassifying 75 good customers as bad customers 

    ###############################################################################################################################
    
    #4.2 Kappa Matric
    # Kappa metric also needs actual and predicted class labels in a data frame
    kappa2(data.frame(actual,predicted))
    #output : 0.74
    ###################################################################################################################

    #4.3 ROC curve analysis
    library(ROCR)
    #Claculate prediction of ROCR library by passing actual and predicted class labels
    pred<-prediction(actual,predicted)
    
    # Calculate performance by passing true positve rate and false positive rate using character label
    # "tpr" and "fpr"
    perf<-performance(pred,"tpr","fpr")
    
    #plot the perf ie ROC curve and compare by drawing a 45 degree line
    plot(perf,col="red")
    abline(0,1, lty = 8, col = "grey")
    #As we can see the ROC curve is way higher than 45 degrees which implies it is a good classifier
    
    # Calcualte the performance by passing auc parameter
    auc<-performance(pred,"auc")
    auc
    unlist(auc@y.values)
    #auc@y value = 0.87    

