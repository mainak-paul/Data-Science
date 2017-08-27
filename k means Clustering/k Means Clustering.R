##Clustering in R

#Read and explore the data

##--------------------------------------Step1 : Reading and exploring the dataset ---------------------------------------------

data<-read.csv("C://Users//mainak//Desktop//DADAI//Data-Science//Data Science With R//k means Clustering//cust.csv")

# Exploratory Analysis
dim(data)
str(data) # All variables are of int datatype
names(data)
summary(data) 
colSums(is.na(data)) # No nas or null values

#Frequency distribution
table(data$Channel)
table(data$Region)

#Subsetting Data by removing Channel and Region from data frame
sample<-data[,3:8]

##--------------------------------------Step2 : Scaling the data ---------------------------------------------
#Formula = (column  - mean(column))/sd(column)
#Subtracting each column value from the mean of the total column and dividing it by the standard deviation of the column
#Repeat for all columns

list<-names(sample)
scaled_data<-data.frame(rownum<-1:440) #Creating a dummy data frame of total no of record as that of sample dataframe
#Appending the scaled value of each value of column to the scaled_data data frame
for(i in 1:length(list))
{
  
  x<-(sample[,i]-mean(sample[,i]))/(sd(sample[,i]))
  scaled_data<-cbind(scaled_data,x)
  names(scaled_data)[i+1]<-paste("scaled_",list[i])
  print(list[i])
  
}
head(scaled_data)
scaled_data<-scaled_data[,-1] # removing the first column comprising of row nums

# Combing the sample and scaled data frame
sample<-cbind(sample,scaled_data)
names(sample)

##--------------------------------------Step3 : kmeans algorithm ---------------------------------------------

#syntax : kmeans(scaled_data,k) ; where k refers to the number of clusters
set.seed(200)
fit.km<-kmeans(sample[,7:12],3) # Running the K-means algorithm on scaled data of numeric values

#We will get a list object
fit.km$size #No of observations in each cluster.
#o/p: 322 105 13

fit.km$withinss #Within sum of squares metric for each cluster

fit.km$totss #The total sum of squares

fit.km$tot.withinss #Total within-cluster sum of squares, i.e., sum(withinss)

fit.km$betweenss #The between-cluster sum of squares, i.e. totss-tot.withinss

fit.km$cluster # Cluster number for each row

##--------------------------------------Step4 : find the optimal number of clusters (k value) ---------------------------------------------

#Create a screeplot-plot of cluster's tot.withinss wrt number of clusters

wss<-1:15
number<-1:15

for (i in 1:15)
  
{
  wss[i]<-kmeans(sample[,7:12],i)$tot.withinss # tot.withinss for each value of K
}

#Better plot using ggplot2
library(ggplot2)
data1<-data.frame(wss,number)
p<-ggplot(data1,aes(x=number,y=wss),color="red")
p+geom_point()+scale_x_continuous(breaks=seq(1,20,1))

#Shortlised optimal number of clusters : 9

##--------------------------------------Step5a : Run the algorithm with k=9(in order to compare)---------------------------------------------

#Build a 9 clusters

fit.km1<-kmeans(sample[,7:12],9)

sample$cluster9<-fit.km1$cluster # Adding the cluster to the sample data set


##--------------------------------------Step5b : Profile the clusters---------------------------------------------

#Cluster summary
cmeans<-aggregate(sample[,1:6],by=list(sample$cluster9),mean)
names(cmeans)
#Population summary
apply(sample[,1:6],2,mean)
apply(sample[,1:6],2,sd)

#z-score calculation function
list2<-names(cmeans)
#z score =population_mean - group_mean /population_sd
for(i in 1:length(list2))
{
  print(i)
  y<-(cmeans[,i+1] - apply(sample[,1:6],2,mean)[i])/(apply(sample[,1:6],2,sd)[i])
  cmeans<-cbind(cmeans,y)
  names(cmeans)[i+7]<-paste("z",list2[i+1],sep="_")
  print(list2[i+1])
}

cmeans<-cmeans[,-14] # Removing the last column from data frame consisting of Nas
names(cmeans)

#Note : The higher the value of z, the different is the group from the population

#If still not satisfied, again run an iteration

##--------------------------------------Step7 : Visualise the clusters---------------------------------------------

#Plotting groups across two dimensions

library(ggplot2)
data<-cbind(data,sample$cluster9)
head(data)
names(data)[9]<- "cluster"  #Changing the name to cluster

#Milk Vs Grocery
p<-ggplot(data,aes(x=Milk,y=Grocery))
p+geom_point(aes(colour=as.factor(cluster)))

#Across Region
p+geom_point(aes(colour=as.factor(cluster)))+
  facet_grid(Region~.)

#Across Channel
p+geom_point(aes(colour=as.factor(cluster)))+
  facet_grid(Channel~.)

#Further Deep dive
p+geom_point(aes(colour=as.factor(cluster),size=Detergents_Paper))+
  facet_grid(Region~Channel) #You see some impact


#With the above plots we can gain insights about various clusters and their impact

