# orange Juice dataset
oj <- read.csv("C:\\Users\\mainak\\Desktop\\DADAI\\Data-Science\\Data Science With R\\dplyr - And -baseR\\oj.csv")
library(dplyr)

# 1.Subsetting Data
    # 1.1 Using filter() function of dplyr
    dplyr_filter_1<-filter(oj,brand=="tropicana")
    dim(filter(oj,brand=="tropicana")) # 9649 17
    
    #Conditional Statements
    dplyr_filter_2<-filter(oj,brand=="tropicana"|brand=="dominicks")
    dim(filter(oj,brand=="tropicana"|brand=="dominicks")) #19298 17
    
    ##################################################################################
    
    # 1.2 Subsetting Data using base R
    baseR_filter_1<-oj[oj$brand=="tropicana",]
    dim(oj[oj$brand=="tropicana",]) # 9649 17
    
    #Conditional Statement Using Base R
    baseR_filter_2<-oj[oj$brand=="tropicana"|oj$brand=="dominicks",]
    dim(oj[oj$brand=="tropicana"|oj$brand=="dominicks",]) #19298 17

#####################################################################################
    
# 2. Selecting Columns from Data Frame
    # 2.1 Using select() function of dplyr
    dplyr_select<-select(oj,brand,INCOME,feat)
    dim(select(oj,brand,INCOME,feat)) #28947 3
    
    # Remove particular columns from data frame
    dplyr_deselect<-select(oj,-brand,-INCOME,-feat)
    dim(select(oj,-brand,-INCOME,-feat)) #28947 14
    
    ##################################################################################
    
    # 2.2 Selecting columns() using baseR
    baseR_Select<-oj[,c("brand","INCOME","feat")]
    dim(oj[,c("brand","INCOME","feat")]) #28947 3
    
    # Remove particular columns using basR
    baseR_Deselect<-oj[,!(names(oj) %in% c("brand","INCOME","feat"))]
    dim(oj[,!(names(oj) %in% c("brand","INCOME","feat"))])
    
##########################################################################################
    
# 3. Mutating/Adding Columns to Data Frame
    # 3.1 Mutating/Adding Column to data frame using mutate() function
    oj<-mutate(oj,logIncome_dplyr=log(INCOME))
    
    ######################################################################################
    
    # 3.2 Adding Column using base R
    oj$logIncome_base <-log(oj$INCOME)
    
############################################################################################

# 4. Aranging/Sorting Column 
    # 4.1 Sorting column in ascending using arrange() function
    sort_ascending_dplyr<-arrange(oj,INCOME)
     
    # Sorting Column in descending order
    sort_descending_dplyr_1<-arrange(oj,desc(INCOME))
    sort_descending_dplyr_2<-arrange(oj,-INCOME)
    
    #########################################################################################
    
    # 4.2 Sorting Column in ascending column using baseR order() function
    sort_ascending_baseR<-oj[order(oj$INCOME),]
    
    #Sorting column in descending order
    sort_descending_baseR<-oj[order(oj$INCOME,decreasing = TRUE),]
    
    
################################################################################################    
    
    
#5. Grouping and Summarizing Data frame using group_by() and summarize() function     
    
    # 5.1 grouping data based on brand using group_by function
    gr_brand<-group_by(oj,brand)
    #summarizing the grouped date and finding mean() and standard deviation() of Income
    summarize(gr_brand,mean(INCOME),sd(INCOME))
    
    #############################################################################################
    #5.2 grouping and summarizing in baseR using aggregate() function
    aggregate(oj$INCOME, list(oj$brand), mean)
    
################################################################################################
    
#6. Pipelines - outputs a complex manipulation using single line of code
    
    pipelined_dplyr<- oj%>%filter(price>=2.5)%>%mutate(logIncome=log(INCOME))%>%summarize(mean(logIncome),median(logIncome),sd(logIncome))
    
    # First argument is the data set
    # Second the data is filtered/subsetted using filter() function where price>=2.5
    # Third a new column is being added using mutate() function
    # Fourth the newly added column is summarized with mean(), median() and standard deviation using summarize function
    
    
    
    