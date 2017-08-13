setwd("C:\\Users\\mainak\\Desktop\\DADAI\\Data Science\\Data Science With R\\ggplot2")

# Base plotting
# Using plot() to study to continous variables
ir<-iris
str(ir)

# plot(x=variable to be displayed on x axis, y = variable to be displayed on y axis)
plot(x=ir$Petal.Width,y=ir$Petal.Length)

# Adding xlabels, ylables and title
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"))

##################################################################################################################################
# Adding colors, plotting symbols

#Adding colors using col
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col="red")


#Adding different plotting symbol using pch
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=2)

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=3)

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=4)

###########################################################################################################################################################
#Adding  more options
# type, lwd
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=4,type="p",lwd=2)

# Making a conditional bivariate plot

# Seeing relationship across different species
#col, pch, cex
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col=ir$Species)

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),pch=as.numeric(ir$Species),col=ir$Species)

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),cex=as.numeric(ir$Species))

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),cex=as.numeric(ir$Species),col=ir$Species)

#############################################################################################################################################################################
#Adding a legend

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),pch=as.numeric(ir$Species))

legend(0.2,7,c("Setosa","Versicolor","Verginica"),pch=unique(as.numeric(ir$Species)))

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col=ir$Species,pch=as.numeric(ir$Species))

legend(0.2,7,c("Setosa","Versicolor","Verginica"),pch=unique(as.numeric(ir$Species)),col=unique(as.numeric(ir$Species)))

##########################################################################################################################################################################
#Studying a continous variable across groups
#Distribution of Sepal lengths across different species
# Univariate Analysis

#Box plots

boxplot(ir$Petal.Length)
summary(ir$Petal.Length) #Mean<Median, negatively skewed

boxplot(ir$Sepal.Width)
summary(ir$Sepal.Width) #Mean>Median, positively skewed

#Improving the asethetics of boxplot
boxplot(ir$Petal.Length,col="red",main="Distribution of Petal length")

plot(x=ir$Species,y=ir$Sepal.Width,xlab="Species",main="Sepal Length across sepcies",col="red")

##########################################################################################################
#Using histograms
hist(ir$Sepal.Width,col="orange")

hist(ir$Sepal.Width,col="orange",labels=TRUE)
#density plot using histogram
hist(ir$Sepal.Width,col="orange",freq=FALSE)
hist(ir$Sepal.Width,col="orange",labels=TRUE,freq=FALSE)
lines(density(ir$Sepal.Width))

#################################################################################################################

#Adding multiple plots in single plotting window
par(mfrow=c(1,2))
plot(x=ir$Species,y=ir$Sepal.Width,xlab="Species",main="Sepal Width across sepcies",col="red")
plot(x=ir$Species,y=ir$Sepal.Length,xlab="Species",main="Sepal Length across sepcies",col="red")
dev.off()

###########################################################################################################

##Direct Marketing data

mk<-read.csv('C:\\Users\\mainak\\Desktop\\DADAI\\Data-Science\\Data Science With R\\ggplot2\\DirectMarketing.csv')
library(ggplot2)

#Understand the relationship between Salary and AmountSpent
#geom_point
p<-ggplot(mk,aes(x=Salary,y=AmountSpent))
p+geom_point()
#Understanding the conditional relationship based on Gender
p+geom_point(aes(colour=Gender))+xlab("Salary in $")+ylab("Expenditure in $")

#Making a trellis plot for both the genders and fitting a tred line
p+geom_point(aes(colour=Gender))+geom_smooth(aes(colour=Gender))

#Creating a trellis plot
p+geom_point(aes(colour=Gender))+geom_smooth(aes(colour=Gender))+facet_grid(Gender~.)

####################################################################################################
#Understanding Univaraites
p<-ggplot(mk,aes(x=AmountSpent))
p+geom_histogram()
#Understanding Gender wise distribution
p+geom_histogram(aes(fill=Gender))

#Modifying the position
p+geom_histogram(aes(fill=Gender,colour=Gender),position="stack",alpha=0.3)

#Alternative, draw a trellis plot
p+geom_histogram(aes(fill=Gender))+facet_grid(Gender~.)

#Polishing the graph
p+geom_histogram(aes(fill=Gender,colour=Gender),alpha=0.3)+facet_grid(Gender~.)

############################################################################################################

#Boxplots
p<-ggplot(mk,aes(y=AmountSpent,x=Gender,fill=Gender))
p+geom_boxplot()

#########################################################################################################

#Density plots
options(scipen=999)
p<-ggplot(mk,aes(x=AmountSpent))
#Conditional density plot
p+geom_density(aes(fill=Gender,colour=Gender),alpha=0.4)

######################################################################################################

#2d counts
p<-ggplot(mk,aes(x=Salary,y=AmountSpent))
p+geom_bin2d()

