#read in credit data, remove missing values, and add ID variable
dat <- read.csv("/Users/jackcleary/Desktop/Stats 3rd Year/Lab 10/credit.csv", na.strings="")
dat <- na.omit(dat)
dat$ID <- seq.int(nrow(dat))
#ensure outcome is a factor
#note that order here is "no" "yes", so a logistic regression model will predict "yes"
dat$Loan_Status <-as.factor(dat$Loan_Status)

library(glmnet)
#set up data in format glmnet can use (must have dummy vars for categorical predictors)
#removed intercept term when making xfactors
xfactors <- model.matrix(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed+Credit_History+Property_Area, data=dat)[, -1]
xnonfactors<- subset(dat, select = c("ApplicantIncome", "CoapplicantIncome", "LoanAmount", "Loan_Amount_Term"))
prepared.dat <- cbind(xnonfactors, xfactors, dat$Loan_Status, dat$ID)
names(prepared.dat)[15:16] <- c("Loan_Status", "ID")

#subset data to create training, valdiation, and test data
library(dplyr)
train.dat <- prepared.dat %>% sample_frac(.6)
rem.dat <- dplyr::anti_join(prepared.dat, train.dat, by = 'ID')
test.dat <- rem.dat %>% sample_frac(.5)
val.dat <- dplyr::anti_join(rem.dat, test.dat, by = 'ID')

#create predictor matrix and outcome vector for training, validation, and test data
train.X <- as.matrix(within(train.dat, rm(Loan_Status, ID)))
val.X <- as.matrix(within(val.dat, rm(Loan_Status, ID)))
test.X <- as.matrix(within(test.dat, rm(Loan_Status, ID)))
train.y <- train.dat$Loan_Status
val.y <- val.dat$Loan_Status
test.y <- test.dat$Loan_Status

#cross-validate to tune lambda for ridge and lasso
cvridge <- cv.glmnet(train.X, train.y, family="binomial", alpha=0, nlambda=20, type.measure="auc")
cvlasso <- cv.glmnet(train.X, train.y, family="binomial", alpha=1, nlambda=20, type.measure="auc")

#fit models with final lambda
ridgemod <- glmnet(train.X, train.y, family="binomial", alpha = 0, lambda = cvridge$lambda.1se)
lassomod <- glmnet(train.X, train.y, family="binomial", alpha = 1, lambda = cvlasso$lambda.1se)

#pre-standardise the predictor matrix for elastic net 
#(glmnet wrapped in caret gets fussy if you try to preprocess within the command)

train.stdX <-scale(train.X)

library(caret)
# Set training control
train_control <- trainControl(method = "repeatedcv",
                              number = 1,
                              repeats = 1,
                              search = "random",
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              verboseIter = TRUE)

# Train the model
elastic_grid <- train(train.stdX, train.y,
                           method = "glmnet",
                           tuneLength = 25,
                           trControl = train_control,
                           metric= "ROC",
                           family = "binomial",
                           standardize = FALSE)

#fit the model with best lambda and alpha
elasticmod <- glmnet(train.X, train.y, family="binomial", alpha = elastic_grid$bestTune$alpha, lambda = elastic_grid$bestTune$lambda)

#Looking at Coefs for final models
Intercepts <- cbind(ridgemod$a0,lassomod$a0,elasticmod$a0)
Coefs <- cbind(ridgemod$beta,lassomod$beta, elasticmod$beta)
Betas <-rbind(Intercepts, Coefs)
rownames(Betas)[1] = "(Intercept)"
colnames(Betas) = c("Ridge", "Lasso", "Elastic Net")
Betas

#Methods for choosing cutoffs in validation data 
#(only ridge model shown here as example. Same for the others)

#Method using visualisation of ROC curves
fit.ridge <- predict(ridgemod, val.X, type="response")

library(ROCR)
ROCRfit.ridge = prediction(fit.ridge, val.y)
ROCRperf.tr.ridge = performance(ROCRfit.ridge, "tpr", "fpr")

plot(ROCRperf.tr.ridge, colorize=TRUE, main="Ridge")

#Method using pROC
library(pROC)
ridge.roc <- roc(val.y, as.vector(fit.ridge) )
coords(ridge.roc, "best", best.method="youden", transpose=TRUE)

#Method using visualisation over Grid
# accuracy here used as metric, could use something else
cutoffs <- seq(min(fit.ridge),max(fit.ridge),(max(fit.ridge)-min(fit.ridge))/100)
accuracy <- NULL

for (i in seq(along = cutoffs)){
  prediction <- ifelse(fit.ridge >= cutoffs[i], "Y", "N") #Predicting for cut-off
  accuracy <- c(accuracy,length(which(val.y ==prediction))/length(prediction)*100)
}

plot(cutoffs, accuracy, pch =19,type='l',col= "steelblue",
     main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")


#Method for comparing performance metrics of various final models in test data 
#(here using pROC "youden" to choose threshold)

#fit lasso and elastic models
fit.lasso <- predict(lassomod, val.X, type="response")
fit.elastic <- predict(elasticmod, val.X, type="response")

#get thresholds
thresh.r <- coords(roc(val.y, as.vector(fit.ridge)), "best", best.method="youden", transpose=TRUE, ret="threshold")
thresh.l <- coords(roc(val.y, as.vector(fit.lasso)), "best", best.method="youden", transpose=TRUE, ret="threshold")
thresh.e <- coords(roc(val.y, as.vector(fit.elastic)), "best", best.method="youden", transpose=TRUE, ret="threshold")

#predict classifications in test data
final.r <- predict(ridgemod, test.X, type="response")
final.l <- predict(lassomod, test.X, type="response")
final.e <- predict(elasticmod, test.X, type="response")

#use caret to see various measures of performance
class.ridge <- as.factor(ifelse(final.r <= thresh.r, "N", "Y"))
confusionMatrix(class.ridge, test.y, positive = "Y")

class.lasso <- as.factor(ifelse(final.l <= thresh.l, "N", "Y"))
confusionMatrix(class.lasso, test.y, positive = "Y")

class.elastic <- as.factor(ifelse(final.e <= thresh.e, "N", "Y"))
confusionMatrix(class.elastic, test.y, positive = "Y")