#install.packages("mlbench")
library(mlbench)
X <-data(PimaIndiansDiabetes)
X <- data.frame(PimaIndiansDiabetes)
X

X$diabetes <- as.character.factor(X$diabetes)
X$diabetes[ X$diabetes == "pos" ] <- 1
X$diabetes[ X$diabetes == "neg" ] <- 0
X$diabetes <- as.numeric(X$diabetes)

#Remove the zero values which are unneccesary by assigning them as the median
X$glucose[ X$glucose == 0] <- median(X$glucose)
X$pressure[ X$pressure == 0] <- median(X$pressure)
X$triceps[ X$triceps == 0] <- median(X$triceps)
X$insulin[ X$insulin == 0] <- median(X$insulin)
X$mass[ X$mass == 0] <- median(X$mass)

# fit the glm using binomial family (note logit is default anyway)
modfit <- glm( diabetes ~ . , data=X, family=binomial(link = "logit") ) 
summary(modfit)

#log-odds
coef(modfit) #positive = more likely, negative = less likely
#odds
#greater than 1 = more likely, less than 1 = less likely. 2 = twice as likely. 0.5, half as likely
t <- exp(coef(modfit)) 
format(t, scientific=FALSE)

#predicted probabilities
predmod <- predict(modfit, type="response")
head(predmod)

# create a dataframe of predicted outcomes
dataframe_pred <- data.frame(predmod)
# name the column
colnames(dataframe_pred)<- c("Probability")
#add a new column to the dataframe to hold predicted outcomes (P or N) based on some threshold 
dataframe_pred[,"Prediction"] <- NA

#assign the prediction based on threshold of 0.5 (threshold picked for demo)
for (i in 1:nrow(dataframe_pred)){
  if (dataframe_pred[i,]$Probability>=0.5){
    dataframe_pred[i,2]<-"P"
  } else
    dataframe_pred[i,2]<-"N"
}

# set up a table to compare model predicted outcomes with actual outcomes
table <- table(dataframe_pred$Prediction,X$diabetes)
table

#install.packages("ROCR")
library(ROCR)
#ROC and Performance function
ROCRpred <- prediction(predmod, X$diabetes)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")

# An AUC of 1 represents is a perfect model and an AUC of 0.5 represents model failure, worse than random guessing
AUC <-performance(ROCRpred, "auc")
AUC@y.values

#Plot ROC curve
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.2), text.adj=c(0.0,2.0), xlim = c(0, 0.06))

# find minimum alpha value at threshold of 0.2
cutoffPoint <- which.min( abs(ROCRperf@alpha.values[[1]] - 0.2) )
# construct data frame of this alpha 
testDF <- data.frame(cutoff = ROCRperf@alpha.values[[1]][cutoffPoint],
                     fpr = ROCRperf@x.values[[1]][cutoffPoint],
                     tpr = ROCRperf@y.values[[1]][cutoffPoint])
testDF

#install.packages("caret")
library(caret)
class.glm <- as.factor(ifelse(predmod <= 0.2, 0, 1 ))
confusionMatrix(class.glm, as.factor(X$diabetes), positive = "1")


