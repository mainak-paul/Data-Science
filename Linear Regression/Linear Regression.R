# Direct Marketting Data Set
data <- read.csv("C:\\Users\\mainak\\Desktop\\DADAI\\Data-Science\\Data Science With R\\Linear Regression\\DirectMarketing.csv")
library(dplyr)
library(ggplot2)
#Purpose of this model is to determine good customer based on AMountSpent dependent variable.


##################################################################################################################################
#Exploratory Data Analysis
str(data)

#1. Age and Amount Spent
plot(data$Age,data$AmountSpent,col="red")

#Age group Middle and Old have similar spending trend , we can combine both category into one.
#Create New Factor Variable Age1

data$Age1 <- as.factor(ifelse(data$Age!='Young','Middle-Old',as.character(data$Age)))
summary(data$Age1)
plot(data$Age1,data$AmountSpent,col="red")

#2. Gender and Amount Spent
summary(data$Gender)
plot(data$Gender,data$AmountSpent,col="red")

#3. Own house and Amount Spent
summary(data$OwnHome)
plot(data$OwnHome,data$AmountSpent,col="red")

#4. Married and Amount Spent
summary(data$Married)
plot(data$Married,data$AmountSpent,col="red")

#5. Location and Amount Spent
summary(data$Location)
plot(data$Location,data$AmountSpent,col="red")

#6. Salary and Amount Spent
summary(data$Salary)
plot(data$Salary,data$AmountSpent,col="red")
#Might be heteroescadasticity because a funnel like shape is forming

#7. Children and Amount Spent
summary(data$Children)
data$Children<-as.factor(data$Children) # Converting integer variable to factor variable
plot(data$Children,data$AmountSpent,col="red")
#People with 2 and 3 children have sinilar spending behaviour
#Lets combine the 2 groups into one "3-2"
data$Children1<-as.factor(ifelse(data$Children==3|data$Children==2,"3-2",as.character(data$Children)))
summary(data$Children1)
plot(data$Children1,data$AmountSpent,col="red")

#8. History and Amount Spent
summary(data$History)

tapply(data$AmountSpent,data$History,mean)
#Impute/Replace Missing values
ind<-which(is.na(data$History))
mean(data[ind,"AmountSpent"])
#Mean of missing category is close to medium category

#Lets plot Missing and medium category
Amount_Medium<-data%>%filter(History=="Medium")%>%select(AmountSpent)

p<-ggplot(data[ind,],aes(x=AmountSpent))
q<-ggplot(Amount_Medium,aes(x=AmountSpent))

p+geom_histogram()
q+geom_histogram()

#Create a category called missing.
data$History1<-as.factor(ifelse(is.na(data$History),"Missing",as.factor(data$History)))
summary(data$History1)
#Label the levels
data$History1<-factor(data$History1,labels=c("High","Low","Medium","Missing"))


#9. Catalogues And Amount Spent
summary(data$Catalogs)
plot(data$Catalogs,data$AmountSpent,col="red")


#########################################################################################################################3

# Model Building and check for significant variable
# Lets remove the variables for which new variables were created
#Age, Children, History
data1<-data[,-c(1,7,8)]

mod<-lm(AmountSpent~.,data=data1)
summary(mod)
#Model Prediction for significant variables
#Gender + Location + Salary + Catalogs + Children1 + History1

#Lets do a step wise model prediction to determine significant variable
step(mod,direction = "both")

#step also giving the below significant variable
#Gender + Location + Salary + Catalogs + Children1 + History1

#---------------------------------------------------------------------------------------------#

#Lets build the new model with significant variables given by step() function

mod2<-lm(formula = AmountSpent ~ Gender + Location + Salary + Catalogs + Children1 + History1, data = data1)
summary(mod2)
#--------------------------------------------------------------------------------------------------------------#

#Remove insignificant variabes
#HistoryMissing
#GenderMale

#Create dummy variables
data1$Male_d<-ifelse(data1$Gender=="Male",1,0)
data1$Female_d<-ifelse(data1$Gender=="Female",1,0)

data1$Missing_d<-ifelse(data$History1=="Missing",1,0)
data1$Low_d<-ifelse(data$History1=="Low",1,0)
data1$Med_d<-ifelse(data$History1=="Medium",1,0)
data1$High_d<-ifelse(data$History1=="High",1,0)

mod3<-lm(formula = AmountSpent ~ Male_d + Location + Salary + Catalogs + Children1+Med_d+Low_d , data = data1)

summary(mod3)

#------------------------------------------------------------------------------------------------------------#
# Remove male dummy as it is also seems to be insignificant

mod4<-lm(formula = AmountSpent ~ Location + Salary + Catalogs + Children1+Med_d+Low_d, data = data1)

summary(mod4)


#############################################################################################################################

#Model validation

# 1. Assumption of normality check using Histogram and quantile plot
library(car)
hist(mod4$residuals)
qqPlot(mod4$residuals) # quantile quantile plot belongong to car package
# Some dispersion at the tail. Non normal behaviour observed

#2. Multicolinearity check
vif(mod4)

#3. Constant  variance check using fitted values and residuals
plot(mod4$fitted.values,mod4$residuals) 
#Funnel shape is observed

#--------------------------------------------------------------------------------------------------------------------#

# So from the above model we can determine that model is not appropriated and is heteroscedastic in nature/
# So lets apply remedies by applying log transformation.

mod5<-lm(formula = log(AmountSpent) ~ Location + Salary + Catalogs + Children1+Med_d+Low_d, data = data1)

summary(mod5)

qqPlot(mod5$residuals)#qqplot looks okay
plot(mod5$fitted.values,mod5$residuals)# Still funnel


#---------------------------------------------------------------------------------------------------------------------#

#Square Root Transformation

mod6<-lm(formula = sqrt(AmountSpent) ~ Location + Salary + Catalogs + Children1+Med_d+Low_d, data = data1)
summary(mod6)
qqPlot(mod6$residuals)
plot(mod6$fitted.values,mod6$residuals)

# Thus qqplot and constant variance check seems to be okay by applying Square root transformation
# These can be considered as the good model

