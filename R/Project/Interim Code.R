ford <- read.table("/Users/jackcleary/Desktop/Stats 3rd Year/Project/Used Car Listings/ford.csv", sep=",", header=T, na.strings="?")

#Initial Exploration
summary(ford)
nrow(ford)
ncol(ford)
head(ford)

fiestaStr <- ford[1,1]
focusStr <- ford[2,1]
kugaStr <- ford[9,1]

fiesta <- subset(ford, model == fiestaStr)
focus <- subset(ford, model == focusStr)
kuga <- subset(ford, model == kugaStr)

columnList <- c("price", "year", "transmission", "mileage", "fuelType", "tax", "mpg", "engineSize")
columnList1 <- c("price", "year", "transmission", "mileage", "fuelType", "tax", "mpg", "engineSize", "model")

#-------------------------------------COMBINED----------------------------------------------
cars <- rbind(fiesta, focus, kuga)

#Remove any cars from the datasets which do not have petrol or diesel engines as they make the study too complicated
#Remove any cars from the datasets which have Semi-Auto or Other transmissions as they make the study too complicated
#Also remove any invalid figures - zeros/NAs
cars$Classify <- 1
cars$Classify[ cars$fuelType == "Hybrid" | cars$fuelType == "Electric" | cars$fuelType == "Other" ] <- NA
cars$Classify[ cars$transmission == "Semi-Auto" | cars$transmission == "Other"] <- NA
cars$Classify[ cars$engineSize == 0 | cars$tax == 0 | cars$mpg == 0 | cars$year > 2020 ] <- NA
nrow(cars)

#Remove the NA values from the dataset
idx <- which( is.na(cars), arr.ind=TRUE )
miss <- cars[idx[,1],]
cars <- cars[-idx[,1],]
nrow(cars)

#Remove the Classify column as it is now meaningless
#Remove the model column as it makes the study to complicated
cars <- subset(cars, select = -c(Classify))
cars <- cars[,columnList1]
head(cars)

#Change the transmission, model and fuel type Variables to Numbered Values
cars$transmission <- as.character.factor(cars$transmission)
cars$transmission[ cars$transmission == "Manual" ] <- 1
cars$transmission[ cars$transmission == "Automatic" ] <- 0

cars$model <- as.character.factor(cars$model)
cars$model[ cars$model == fiestaStr ] <- 1
cars$model[ cars$model == focusStr ] <- 2
cars$model[ cars$model == kugaStr ] <- 3

cars$fuelType <- as.character.factor(cars$fuelType)
cars$fuelType[ cars$fuelType == "Petrol" ] <- 1
cars$fuelType[ cars$fuelType == "Diesel" ] <- 0
summary(cars)

#Change Year variable to years old for accuracy
cars$year <- 2020 - cars$year

cars$price <- as.numeric(cars$price)
cars$year <- as.numeric(cars$year)
cars$transmission <- as.numeric(cars$transmission)
cars$mileage <- as.numeric(cars$mileage)
cars$fuelType <- as.numeric(cars$fuelType)
cars$tax <- as.numeric(cars$tax)
cars$mpg <- as.numeric(cars$mpg)
cars$engineSize <- as.numeric(cars$engineSize)
cars$model <- as.numeric(cars$model)

par(mfrow = c(2,3))
boxplot(price ~ model, data = cars)
boxplot(year ~ model, data = cars)
boxplot(mileage ~ model, data = cars)
boxplot(tax ~ model, data = cars)
boxplot(mpg ~ model, data = cars)
boxplot(engineSize ~ model, data = cars)

res <- var(cars)
round(res, 2)

res2 <- cov(cars, method = "spearman")
round(res2, 2)

res3 <- cor(cars, method = "spearman")
round(res3, 2)

par(mfrow = c(1,1))
#some graphs
#install.packages("corrplot")
library(corrplot)
corrplot(res3, tl.col = "black", tl.srt = 45)


#-----------------------------------FIESTA---------------------------------------------------
#Remove any fiesta from the datasets which do not have petrol or diesel engines as they make the study too complicated
#Remove any fiesta from the datasets which have Semi-Auto or Other transmissions as they make the study too complicated
#Also remove any invalid figures - zeros/NAs
fiesta$Classify <- 1
fiesta$Classify[ fiesta$fuelType == "Hybrid" | fiesta$fuelType == "Electric" | fiesta$fuelType == "Other" ] <- NA
fiesta$Classify[ fiesta$transmission == "Semi-Auto" | fiesta$transmission == "Other"] <- NA
fiesta$Classify[ fiesta$engineSize == 0 | fiesta$tax == 0 | fiesta$mpg == 0 | fiesta$year > 2020 ] <- NA
nrow(fiesta)

#Remove the NA values from the dataset
idx <- which( is.na(fiesta), arr.ind=TRUE )
miss <- fiesta[idx[,1],]
fiesta <- fiesta[-idx[,1],]
nrow(fiesta)

#Remove the Classify column as it is now meaningless
#Remove the model column as it makes the study to complicated
fiesta <- subset(fiesta, select = -c(Classify, model))
fiesta <- fiesta[,columnList]
head(fiesta)

#Change the transmission and fuel type Variables to Numbered Values
fiesta$transmission <- as.character.factor(fiesta$transmission)
fiesta$transmission[ fiesta$transmission == "Manual" ] <- 1
fiesta$transmission[ fiesta$transmission == "Automatic" ] <- 0

fiesta$fuelType <- as.character.factor(fiesta$fuelType)
fiesta$fuelType[ fiesta$fuelType == "Petrol" ] <- 1
fiesta$fuelType[ fiesta$fuelType == "Diesel" ] <- 0

#Change Year variable to years old for accuracy
fiesta$year <- 2020 - fiesta$year

fiesta$price <- as.numeric(fiesta$price)
fiesta$year <- as.numeric(fiesta$year)
fiesta$transmission <- as.numeric(fiesta$transmission)
fiesta$mileage <- as.numeric(fiesta$mileage)
fiesta$fuelType <- as.numeric(fiesta$fuelType)
fiesta$tax <- as.numeric(fiesta$tax)
fiesta$mpg <- as.numeric(fiesta$mpg)
fiesta$engineSize <- as.numeric(fiesta$engineSize)

boxplot()

res <- var(fiesta)
round(res, 2)

res2 <- cov(fiesta, method = "spearman")
round(res2, 2)

res3 <- cor(fiesta, method = "spearman")
round(res3, 2)

#some graphs
#install.packages("corrplot")
library(corrplot)
corrplot(res3, tl.col = "black", tl.srt = 45)
#----------------------------------FOCUS-------------------------------------------------------
#Remove any focus from the datasets which do not have petrol or diesel engines as they make the study too complicated
#Remove any focus from the datasets which have Semi-Auto or Other transmissions as they make the study too complicated
#Also remove any invalid figures - zeros/NAs
focus$Classify <- 1
focus$Classify[ focus$fuelType == "Hybrid" | focus$fuelType == "Electric" | focus$fuelType == "Other" ] <- NA
focus$Classify[ focus$transmission == "Semi-Auto" | focus$transmission == "Other"] <- NA
focus$Classify[ focus$engineSize == 0 | focus$tax == 0 | focus$mpg == 0 | focus$year > 2020] <- NA
nrow(focus)

#Remove the NA values from the dataset
idx <- which( is.na(focus), arr.ind=TRUE )
miss <- focus[idx[,1],]
focus <- focus[-idx[,1],]
nrow(focus)

#Remove the Classify column as it is now meaningless
#Remove the model column as it makes the study to complicated
focus <- subset(focus, select = -c(Classify, model))
focus <- focus[,columnList]
head(focus)

#Change the transmission and fuel type Variables to Numbered Values
focus$transmission <- as.character.factor(focus$transmission)
focus$transmission[ focus$transmission == "Manual" ] <- 1
focus$transmission[ focus$transmission == "Automatic" ] <- 0

focus$fuelType <- as.character.factor(focus$fuelType)
focus$fuelType[ focus$fuelType == "Petrol" ] <- 1
focus$fuelType[ focus$fuelType == "Diesel" ] <- 0

#Change Year variable to years old for accuracy
focus$year <- 2020 - focus$year
head(focus)

focus$price <- as.numeric(focus$price)
focus$year <- as.numeric(focus$year)
focus$transmission <- as.numeric(focus$transmission)
focus$mileage <- as.numeric(focus$mileage)
focus$fuelType <- as.numeric(focus$fuelType)
focus$tax <- as.numeric(focus$tax)
focus$mpg <- as.numeric(focus$mpg)
focus$engineSize <- as.numeric(focus$engineSize)

res <- var(focus)
round(res, 2)

res2 <- cov(focus, method = "spearman")
round(res2, 2)

res3 <- cor(focus, method = "spearman")
round(res3, 2)

#some graphs
#install.packages("corrplot")
library(corrplot)
corrplot(res3, tl.col = "black", tl.srt = 45)
#----------------------------------KUGA--------------------------------------------------------

#Remove any kuga from the datasets which do not have petrol or diesel engines as they make the study too complicated
#Remove any kuga from the datasets which have Semi-Auto or Other transmissions as they make the study too complicated
#Also remove any invalid figures - zeros/NAs
kuga$Classify <- 1
kuga$Classify[ kuga$fuelType == "Hybrid" | kuga$fuelType == "Electric" | kuga$fuelType == "Other" ] <- NA
kuga$Classify[ kuga$transmission == "Semi-Auto" | kuga$transmission == "Other"] <- NA
kuga$Classify[ kuga$engineSize == 0 | kuga$tax == 0 | kuga$mpg == 0 | kuga$year > 2020] <- NA
nrow(kuga)

#Remove the NA values from the dataset
idx <- which( is.na(kuga), arr.ind=TRUE )
miss <- kuga[idx[,1],]
kuga <- kuga[-idx[,1],]
nrow(kuga)

#Remove the Classify column as it is now meaningless
#Remove the model column as it makes the study to complicated
kuga <- subset(kuga, select = -c(Classify, model))
kuga <- kuga[,columnList]
head(kuga)

#Change the transmission and fuel type Variables to Numbered Values
kuga$transmission <- as.character.factor(kuga$transmission)
kuga$transmission[ kuga$transmission == "Manual" ] <- 1
kuga$transmission[ kuga$transmission == "Automatic" ] <- 0

kuga$fuelType <- as.character.factor(kuga$fuelType)
kuga$fuelType[ kuga$fuelType == "Petrol" ] <- 1
kuga$fuelType[ kuga$fuelType == "Diesel" ] <- 0

#Change Year variable to years old for accuracy
kuga$year <- 2020 - kuga$year
head(kuga)

kuga$price <- as.numeric(kuga$price)
kuga$year <- as.numeric(kuga$year)
kuga$transmission <- as.numeric(kuga$transmission)
kuga$mileage <- as.numeric(kuga$mileage)
kuga$fuelType <- as.numeric(kuga$fuelType)
kuga$tax <- as.numeric(kuga$tax)
kuga$mpg <- as.numeric(kuga$mpg)
kuga$engineSize <- as.numeric(kuga$engineSize)

res <- var(kuga)
res <- round(res, 2)
summary(res)

res2 <- cov(kuga, method = "spearman")
res2 <- round(res2, 2)

res3 <- cor(kuga, method = "spearman")
res3 <- round(res3, 2)

#some graphs
#install.packages("corrplot")
library(corrplot)
corrplot(res3, tl.col = "black", tl.srt = 45)

#------------------------------WRITE FILE------------------------------------------------------
#Write CSV file for submission
write.csv(cars, "/Users/jackcleary/Desktop/Stats 3rd Year/Project/Used Car Listings/UsedCars.csv", quote = FALSE)

#------------------------------FIT MODEL------------------------------------------------------
cars_mod <- lm(price ~ ., cars)
summary(cars_mod)
plot(cars_mod)

fiesta_mod <- lm(price ~ ., cars)
summary(fiesta_mod)
plot(fiesta_mod)

focus_mod <- lm(price ~ ., cars)
summary(focus_mod)
plot(focus_mod)

kuga_mod <- lm(price ~ ., cars)
summary(kuga_mod)
plot(kuga_mod)

