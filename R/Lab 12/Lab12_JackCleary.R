X <- read.table( "/Users/jackcleary/Desktop/Stats 3rd Year/Lab 12/diabetes.csv", sep=",", header=T, na.strings="?" )

#general summary

#Get rid of Skin Thickness column as it has too many zero values
X <- subset(X, select = -c(SkinThickness))

#Get rid of rows with invalid figures - zeros/ NAs
X$Classify <- 1
X$Classify[ X$Glucose == 0 | X$BloodPressure == 0 | X$BMI == 0 ] <- NA

idx <- which( is.na(X), arr.ind=TRUE )
miss <- X[idx[,1],]
X <- X[-idx[,1],]
X <- subset(X, select = -c(Classify))

# fit the glm using binomial family (note logit is default anyway)
modfit <- glm( Outcome ~ . , data=X, family=binomial(link = "logit") ) 
modfit
BIC(modfit)

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

#set up a table to compare model predicted outcomes with actual outcomes
table <- table(dataframe_pred$Prediction, X$Outcome)
table
#---------------------------------------------------------------------------------------------------
#Assess the model
#install.packages("arm")
library(arm)

#Plot the Binned Plot for the model
par(mfrow = c(1,1))
PearsonResid <- resid(modfit, type = "pearson")
hist(PearsonResid)
DevResid <- resid(modfit, type = "deviance")
hist(DevResid)

#Model
binnedplot(fitted(modfit), residuals(modfit, type = "response"), 
           nclass = NULL, xlab = "Expected Values", ylab = "Average Residual", 
           main = "Binned Residual Plot", cex.pts = 0.8, col.pts = 1, col.int = "gray")

#Each Variable
colNames = c("Pregnancies", "Glucose", "Blood Pressure", "Insulin", "BMI", "Diabetes Pedigree Function", "Age")

for(i in 1:7){
  par(mfrow = c(1,3))
  plot(X[,i], PearsonResid, xlab = colNames[i], ylab = "Pearson Residuals", abline(h=c(-2,2), lty = 2, col = "red"))
  plot(X[,i], DevResid, xlab = colNames[i], ylab = "Deviance Residuals", abline(h=2, lty = 2, col = "red"))
  
  binnedplot(X[,i], residuals(modfit, type = "response"), 
             nclass = NULL, xlab = colNames[i], ylab = "Average Residual", 
             main = "Binned Residual Plot", cex.pts = 0.8, col.pts = 1, col.int = "gray")
}

#plot ROC curve
#install.packages("ROCR")
library(ROCR)
#ROC and Performance function
ROCRpred <- prediction(predmod, X$Outcome)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")

# An AUC of 1 represents is a perfect model and an AUC of 0.5 represents model failure, worse than random guessing
AUC <-performance(ROCRpred, "auc")
AUC
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

#-----------------------------------------------------------------------------------------------
#Change the model and assess new model
#Change the BMI column to the log of the BMI value
par(mfrow = c(1,1))
X$BMI <- log(X$BMI)

modfit2 <- glm( Outcome ~ . , data=X, family=binomial(link = "logit")) 
modfit2
BIC(modfit2)

#predicted probabilities
predmod2 <- predict(modfit2, type="response")
head(predmod2)

# create a dataframe of predicted outcomes
dataframe_pred2 <- data.frame(predmod2)
# name the column
colnames(dataframe_pred2)<- c("Probability")
#add a new column to the dataframe to hold predicted outcomes (P or N) based on some threshold 
dataframe_pred2[,"Prediction"] <- NA

#assign the prediction based on threshold of 0.5 (threshold picked for demo)
for (i in 1:nrow(dataframe_pred2)){
  if (dataframe_pred2[i,]$Probability>=0.5){
    dataframe_pred2[i,2]<-"P"
  } else
    dataframe_pred2[i,2]<-"N"
}

#set up a table to compare model predicted outcomes with actual outcomes
table2 <- table(dataframe_pred2$Prediction, X$Outcome)
table2


#Plot the Binned Plot for the model
PearsonResid2 <- resid(modfit2, type = "pearson")
hist(PearsonResid2)
DevResid2 <- resid(modfit2, type = "deviance")
hist(DevResid2)

#Model
binnedplot(fitted(modfit2), residuals(modfit2, type = "response"), 
           nclass = NULL, xlab = "Expected Values", ylab = "Average Residual", 
           main = "Binned residual plot", cex.pts = 0.8, col.pts = 1, col.int = "gray")

#Each Variable
colNames = c("Pregnancies", "Glucose", "Blood Pressure", "Insulin", "BMI Log", "Diabetes Pedigree Function", "Age")

for(i in 1:7){
  par(mfrow = c(1,3))
  plot(X[,i], PearsonResid2, xlab = colNames[i], ylab = "Pearson Residuals", abline(h=c(-2,2), lty = 2, col = "red"))
  plot(X[,i], DevResid2, xlab = colNames[i], ylab = "Deviance Residuals", abline(h=2, lty = 2, col = "red"))
  binnedplot(X[,i], residuals(modfit2, type = "response"), 
             nclass = NULL, xlab = colNames[i], ylab = "Average Residual", 
             main = "Binned residual plot", cex.pts = 0.8, col.pts = 1, col.int = "gray")
}

#plot ROC curve
#install.packages("ROCR")
library(ROCR)
#ROC and Performance function
ROCRpred2 <- prediction(predmod2, X$Outcome)
ROCRperf2 <- performance(ROCRpred2, "tpr", "fpr")

# An AUC of 1 represents is a perfect model and an AUC of 0.5 represents model failure, worse than random guessing
AUC2 <-performance(ROCRpred2, "auc")
AUC2
AUC@y.values

#Plot ROC curve
plot(ROCRperf2)
plot(ROCRperf2, colorize=TRUE)
plot(ROCRperf2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.2), text.adj=c(0.0,2.0), xlim = c(0, 0.06))

# find minimum alpha value at threshold of 0.2
cutoffPoint2 <- which.min( abs(ROCRperf2@alpha.values[[1]] - 0.2) )
# construct data frame of this alpha 
testDF2 <- data.frame(cutoff = ROCRperf2@alpha.values[[1]][cutoffPoint2],
                     fpr = ROCRperf2@x.values[[1]][cutoffPoint2],
                     tpr = ROCRperf2@y.values[[1]][cutoffPoint2])
testDF2