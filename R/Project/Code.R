library(dplyr)
library(lattice)
library(ggplot2)
library(GGally)
library(readr)
library(car)
library(MASS)
library(caret)

#-------------------------------------Create Dataset----------------------------------------------
ford <- read.table("/Users/jackcleary/Desktop/Stats 3rd Year/Project/Used Car Listings/ford.csv", sep=",", header=T, na.strings="?")

#Initial Exploration
summary(ford)
nrow(ford)
ncol(ford)
head(ford)

#Create Strings for each model name from existing observations
fiestaStr <- ford[1,1]
focusStr <- ford[2,1]
kugaStr <- ford[9,1]

#Create subsets for each model
fiesta <- subset(ford, model == fiestaStr)
focus <- subset(ford, model == focusStr)
kuga <- subset(ford, model == kugaStr)

#Create master dataset containing all three of the models
cars <- rbind(fiesta, focus, kuga)

#Change Year variable to years old for accuracy - Data was collected in 2020
cars$year <- 2020 - cars$year

#-------------------------------------Data Cleaning----------------------------------------------
columnList1 <- c("price", "year", "transmission", "mileage", "fuelType", "tax", "mpg", "engineSize", "model")

#Remove any cars from the datasets which do not have petrol or diesel engines as they make the study too complicated
#Remove any cars from the datasets which have Semi-Auto or Other transmissions as they make the study too complicated
#Also remove any invalid figures - zeros/NAs
cars$Classify <- 1
cars$Classify[ cars$fuelType == "Hybrid" | cars$fuelType == "Electric" | cars$fuelType == "Other" ] <- NA
cars$Classify[ cars$transmission == "Semi-Auto" | cars$transmission == "Other"] <- NA
cars$Classify[ cars$engineSize == 0 | cars$tax == 0 | cars$mpg == 0 | cars$year < 0 |cars$price > 50000 ] <- NA
nrow(cars)

#Remove the NA values from the dataset
idx <- which( is.na(cars), arr.ind=TRUE )
miss <- cars[idx[,1],]
cars <- cars[-idx[,1],]
nrow(cars)

#Remove the Classify column as it is now meaningless
head(cars)
cars <- subset(cars, select = -c(Classify))
cars <- cars[,columnList1]
head(cars)

#-------------------------------------Changing Variable Types to Numbers----------------------------------------------

#Change the transmission, fuelType and model Variables to Numbered Values
head(cars)
cars$transmission[ cars$transmission == "Automatic" ] <- 0
cars$transmission[ cars$transmission == "Manual" ] <- 1
cars$transmission <- as.integer(cars$transmission)

cars$fuelType[ cars$fuelType == "Diesel" ] <- 0
cars$fuelType[ cars$fuelType == "Petrol" ] <- 1
cars$fuelType <- as.integer(cars$fuelType)

cars$model[ cars$model == " Fiesta" ] <- 1
cars$model[ cars$model == " Focus" ] <- 2
cars$model[ cars$model == " Kuga" ] <- 3
cars$model <- as.integer(cars$model)

cars$year <- as.integer(cars$year)
#-------------------------------------Exploring Data Further----------------------------------------------
#Boxplots
par(mfrow = c(2,3))
boxplot(price ~ model, data = cars)
boxplot(year ~ model, data = cars)
boxplot(mileage ~ model, data = cars)
boxplot(tax ~ model, data = cars)
boxplot(mpg ~ model, data = cars)
boxplot(engineSize ~ model, data = cars)

#Plot each variable vs Price
par(mfrow = c(3,3))
plot(x = cars$year, y = cars$price, xlab = "Year", ylab = "Price")
plot(x = cars$transmission, y = cars$price, xlab = "Transmission", ylab = "Price")
plot(x = cars$mileage, y = cars$price, xlab = "Mileage", ylab = "Price")
plot(x = cars$fuelType, y = cars$price, xlab = "Fuel Type", ylab = "Price")
plot(x = cars$tax, y = cars$price, xlab = "Tax", ylab = "Price")
plot(x = cars$mpg, y = cars$price, xlab = "Miles Per Gallon", ylab = "Price")
plot(x = cars$engineSize, y = cars$price, xlab = "Engine Size", ylab = "Price")
plot(x = cars$model, y = cars$price, xlab = "Model", ylab = "Price")

res <- var(cars)
round(res, 2)

res2 <- cov(cars, method = "spearman")
round(res2, 2)

res3 <- cor(cars, method = "spearman")
round(res3, 2)

par(mfrow = c(1,1))
#install.packages("corrplot")
library(corrplot)
corrplot(res3, tl.col = "black", tl.srt = 45)
#-------------------------------------Linear Model 1----------------------------------------------
#Re-index
rownames(cars) <- 1:nrow(cars)

nrow(cars)
nrow(cars[cars$model == 1 , ]) #1 - 4787
nrow(cars[cars$model == 2 , ]) #4788 - 8704
nrow(cars[cars$model == 3 , ]) #8705 - 10712

trainIndex <- c(1:3830, 4788:7921, 8705:10310)
testIndex <- c(3831:4787, 7922:8704, 10311:10712)
train <- cars[trainIndex, ]
test <- cars[testIndex, ]

mod1 <- lm(price ~ ., train)
summary(mod1)

#Checking regression assumption by diagnostic plots
par(mfrow=c(2,2))
plot(mod1)

#Observe outliers
cars[c(3775, 5019, 7784, 9630), ]

#Cross-Validation MAE calculation and comparison
mod1Pred <- predict(mod1, test[, -1])
par(mfrow=c(1,1))
plot(test$price, mod1Pred, xlab = "Actual Prices", ylab = "Predicted Prices")
abline(0,1)

mae(test$price, mod1Pred)
rmse(test$price, mod1Pred)
mean(test$price)

#-------------------------------------Linear Model 2----------------------------------------------

cars <- cars[-3775, ]

#Transformation of price
cars2 <- cars
rownames(cars2) <- 1:nrow(cars2)
head(cars2)
cars2$price <- (sqrt(cars$price))
head(cars2)

par(mfrow=c(1,2))
qqnorm(cars$price, main="Normal Q-Q Plot of Price Before Log Transformation");qqline(cars$price)
qqnorm(cars2$price, main="Normal Q-Q Plot of Price After Log Transformation");qqline(cars2$price)

train2Index <- c(1:3829, 4787:7920, 8704:10309)
test2Index <- c(3830:4786, 7921:8703, 10310:10711)
train2 <- cars2[train2Index, ]
test2 <- cars2[test2Index, ]

mod2 <- lm(price ~ ., train2)
summary(mod2)

step_mod2 <- stepAIC(mod2)
summary(step_mod2)

#Checking regression assumption by diagnostic plots
par(mfrow=c(2,2))
plot(mod2)

#Cross-Validation MAE calculation and comparison
mod2Pred <- predict(mod2, test2[, -1])
par(mfrow=c(1,1))
plot(test2$price^2, mod2Pred^2, xlab = "Actual Prices", ylab = "Predicted Prices")
abline(0,1)

mae((test2$price)^2, (mod2Pred)^2)
rmse((test2$price)^2, (mod2Pred)^2)
mean((test2$price)^2)

#-------------------------------------Linear Model 3----------------------------------------------

cooksD1 <- cooks.distance(mod2)
inf1 <- cooksD1[(cooksD1 <= (3 * mean(cooksD1, na.rm = TRUE)))] 

inf_name1 <- names(inf1)
train3 <- train2[inf_name1,]
test3 <- test2

mod3 <- lm(price ~ ., train3)
summary(mod3)

step_mod3 <- stepAIC(mod3)
summary(step_mod3)

#Checking regression assumption by diagnostic plots
par(mfrow=c(2,2))
plot(mod3)

#Cross-Validation MAE calculation and comparison
mod3Pred <- predict(mod3, test3[, -1])
par(mfrow=c(1,1))
plot(test3$price^2, mod3Pred^2, xlab = "Actual Prices", ylab = "Predicted Prices")
abline(0,1)

mae((test3$price)^2, (mod3Pred)^2)
rmse((test3$price)^2, (mod3Pred)^2)
mean((test3$price)^2)

#-------------------------------------Fiesta vs Focus vs Kuga----------------------------------------------
fiestaTrain <- train3[train3$model == 1, ]
focusTrain <- train3[train3$model == 2, ]
kugaTrain <- train3[train3$model == 3, ]

fiestaTest <- test3[test3$model == 1, ]
focusTest <- test3[test3$model == 2, ]
kugaTest <- test3[test3$model == 3, ]

nrow(kugaTrain)
nrow(kugaTest)

#############Fiesta 
fiestaMod <- lm(price ~ ., fiestaTrain)
summary(fiestaMod)

#Checking regression assumption by diagnostic plots
par(mfrow=c(2,2))
plot(fiestaMod)

#Cross-Validation MAE calculation and comparison
fiestaPred <- predict(fiestaMod, fiestaTest[, -1])
par(mfrow=c(1,1))
p1 <- plot(fiestaTest$price^2, fiestaPred^2, xlab = "Actual Prices", ylab = "Predicted Prices", abline(0,1))

mae((fiestaTest$price)^2, (fiestaPred)^2)
rmse((fiestaTest$price)^2, (fiestaPred)^2)
mean((fiestaTest$price)^2)


#############focus 
focusMod <- lm(price ~ ., focusTrain)
summary(focusMod)

#Checking regression assumption by diagnostic plots
par(mfrow=c(2,2))
plot(focusMod)

#Cross-Validation MAE calculation and comparison
focusPred <- predict(focusMod, focusTest[, -1])
par(mfrow=c(1,1))
p2 <- plot(focusTest$price^2, focusPred^2, xlab = "Actual Prices", ylab = "Predicted Prices", abline(0,1))

mae((focusTest$price)^2, (focusPred)^2)
rmse((focusTest$price)^2, (focusPred)^2)
mean((focusTest$price)^2)


#############kuga 
kugaMod <- lm(price ~ ., kugaTrain)
summary(kugaMod)

#Checking regression assumption by diagnostic plots
par(mfrow=c(2,2))
plot(kugaMod)

#Cross-Validation MAE calculation and comparison
kugaPred <- predict(kugaMod, kugaTest[, -1])
par(mfrow=c(1,1))
p3 <- plot(kugaTest$price^2, kugaPred^2, xlab = "Actual Prices", ylab = "Predicted Prices", abline(0,1))

mae((kugaTest$price)^2, (kugaPred)^2)
rmse((kugaTest$price)^2, (kugaPred)^2)
mean((kugaTest$price)^2)

#Comparisons
par(mfrow = c(1,3))
p1 <- plot(fiestaTest$price^2, fiestaPred^2, xlab = "Actual Prices", ylab = "Predicted Prices", abline(0,1), xlim = c(0,25000), ylim = c(0,25000))
p2 <- plot(focusTest$price^2, focusPred^2, xlab = "Actual Prices", ylab = "Predicted Prices", abline(0,1), xlim = c(0,25000), ylim = c(0,25000))
p3 <- plot(kugaTest$price^2, kugaPred^2, xlab = "Actual Prices", ylab = "Predicted Prices", abline(0,1), xlim = c(0,25000), ylim = c(0,25000))

