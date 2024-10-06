#regression recap
#install.packages("carData")
library(carData)
SalDat <- Salaries
SalDat
model_a <- lm(salary~rank+discipline+rank*discipline, data = SalDat) #can specify. * means interaction
model <- lm(salary ~ ., data = SalDat) #using a . means "all other variables"
model_b <- lm(salary~.+ .^2,data = SalDat) # +.^2 means "all first order interactions
#note that if including an interaction term, you need to include all components as well...
# i.e. y ~ a+b+a*b is ok. y~ a+b+a*c is not ok. three way interaction: y~a+b+c+a*b+a*c+b*c+a*b*c
#the code will work but the theory is wrong (in general)
#including interactions can make a model very big very fast, especially when vars are factors
summary(model_a)
summary(model_b)
summary(model)
#prediction in this case is simple. 
#For any observation, multiply value of each variable by its associated coefficient...
#...add all together (plus intercept)

#what about logistic regression?

#data prep
X <- read.table( "/Users/jackcleary/Desktop/Stats 3rd Year/Lab 6/breast_cancer.dat", sep=",", header=T, na.strings="?" )

# find out which rows have NA's and remove (why do we do this?)
idx <- which( is.na(X), arr.ind=TRUE )
X <- X[-idx[,1],]
X

# recode the class_tumor as benign == 0
X$class_tumor[ X$class_tumor == 2 ] <- 0
X$class_tumor[ X$class_tumor == 4 ] <- 1
Y <- X[,-1] # create a dataframe without the id_number

# fit the glm using binomial family (note logit is default anyway)
modfit <- glm( class_tumor ~ . , data=Y, family=binomial(link = "logit") ) 
summary(modfit)

#what are these coefficients? what do they mean? ***recall, applies for one unit increase***
#log-odds
coef(modfit) #positive = more likely, negative = less likely
#odds
#greater than 1 = more likely, less than 1 = less likely. 2 = twice as likely. 0.5, half as likely
t <- exp(coef(modfit)) 
format(t, scientific=FALSE)

#predicted probabilities
predmod <- predict(modfit, type="response")
head(predmod)

#how to go from probabilities to predicted outcome - B or M?
# create a dataframe of predicted outcomes
dataframe_pred <- data.frame(predmod)
# name the column
colnames(dataframe_pred)<- c("Probability")
#add a new column to the dataframe to hold predicted outcomes (B or M) based on some threshold 
dataframe_pred[,"Prediction"] <- NA

#assign the prediction based on threshold of 0.5 (threshold picked for demo)
for (i in 1:nrow(dataframe_pred)){
  if (dataframe_pred[i,]$Probability>=0.5){
    dataframe_pred[i,2]<-"M"
  } else
    dataframe_pred[i,2]<-"B"
}

# set up a table to compare model predicted outcomes with actual outcomes
table <- table(dataframe_pred$Prediction,X$class_tumor)
table

#how to pick threshold? - visually by ROC curve. Later lectures/labs will show other methods
#Install and load ROCR package
#install.packages("ROCR")
library(ROCR)
#ROC and Performance function
ROCRpred <- prediction(predmod, X$class_tumor)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
#what AUC do we get?
# An AUC of 1 represents is a perfect model an an AUC of 0.5 represents model failure, worse than random guessing
AUC <-performance(ROCRpred, "auc")
AUC@y.values
#Plot ROC curve
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.2), text.adj=c(0.0,2.0), xlim = c(0, 0.06))

#but how do we decide what threshold we want? 
#depends on purpose of analysis. Depends on judgement

# find minimum alpha value at threshold of 0.2
cutoffPoint <- which.min( abs(ROCRperf@alpha.values[[1]] - 0.2) )
# construct data frame of this alpha 
testDF <- data.frame(cutoff = ROCRperf@alpha.values[[1]][cutoffPoint],
                     fpr = ROCRperf@x.values[[1]][cutoffPoint],
                     tpr = ROCRperf@y.values[[1]][cutoffPoint])
testDF


#make a new table as before... or! caret - a very useful function
#we will come back to this in later weeks

library(caret)
class.glm <- as.factor(ifelse(predmod <= 0.2, 0, 1 ))
confusionMatrix(class.glm, as.factor(X$class_tumor), positive = "1")

#these results (and output from the regrssion) tell us how our model worked on the whole dataset.
#Sometimes, the dataset in front of us is all we care about (seeking to explain)
#Sometimes interested in estimating how our model would work on new data. (seeking to predict)
#How could we assess this?

