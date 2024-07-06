adult <- read.csv('/Users/rahuljadhav/Downloads/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/adult_sal.csv')
print(head(adult))
library(dplyr)
adult <- select(adult,-X)
print(str(adult))
print(summary(adult))
print(table(adult$type_employer))
####
### DATA CLEANING ###
####


unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,unemp)
print(table(adult$type_employer))



group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}


adult$type_employer <- sapply(adult$type_employer,group_emp)
print(table(adult$type_employer))

####
### Married Status ###
####

print(table(adult$marital))


group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}



adult$marital <- sapply(adult$marital,group_marital)
print(table(adult$marital))

####
### Country ###
####

print(table(adult$country))
print(levels(adult$country))



Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')



group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}


adult$country <- sapply(adult$country,group_country)
print(table(adult$country))


adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)


print(str(adult))


####
### MISSING DATA ###
####

library(Amelia)
adult[adult == '?'] <- NA

table(adult$type_employer)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)

print(table(adult$type_employer))

missmap(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

####
### DROP MISSING DATA ###
####

adult <- na.omit(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

####
### DATA VISUALIZATION ###
####

library(ggplot2)
library(dplyr)
ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()
ggplot(adult,aes(hr_per_week)) + geom_histogram() + theme_bw()
names(adult)[names(adult)=="country"] <- "region"


ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




#################################
## LOGISTIC REGRESSION MODEL ##
#################################

# TRAIN TEST SPLIT

# Import Library
library(caTools)

# Set a random see so your "random" results are the same as this notebook
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(adult$income, SplitRatio = 0.7) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(adult, sample == T)

# Testing Data
test = subset(adult, sample == F)


model = glm(income ~ ., family = binomial(link = 'logit'), data = train)
summary(model)
new.step.model <- step(model)
summary(new.step.model)
##test$predicted.income = predict(model,test, type = "response")
##table(test$income, test$predicted.income > 0.5)
                                                                                                                         False <- c(6372,872)
                                                                                                                          True  <- c(548,1423)
                                                                                                                           confusion.matrix <- cbind(False,True)
rownames(confusion.matrix) <- c('<=50K','>50K')
print(confusion.matrix)


acc <- (6372+1423)/(6372+1423+548+872)
acc
#recall
6732/(6372+548)
#precision
6732/(6372+872)




