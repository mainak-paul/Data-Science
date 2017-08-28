#Information about the list of packages currently loaded in R
search()

#Information About the Current R Session
sessionInfo()

#unloading a package from current session of R
detach(package:RODBC)

#Getting and setting working directory
getwd()
setwd("C://Users//mainak//Desktop//DADAI//Data-Science//Data Science With R//RMySql and RODBC")


#saves in the working directory
save.image()

# saves in a specified file
save.image(file="C:/Users/Admin/Documents/R/image.RData") 

#Loads the session data
load('C:/Users/Admin/Documents/R/image.RData')

# Remove all objects in the current session
rm(list=ls())
