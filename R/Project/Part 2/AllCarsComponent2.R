audi <- read.csv("/Users/jackcleary/Desktop/Stats 3rd Year/Project/Used Car Listings/audi.csv", sep=",", header=T, na.strings="?")
bmw <- read.csv("/Users/jackcleary/Desktop/Stats 3rd Year/Project/Used Car Listings/bmw.csv", sep=",", header=T, na.strings="?")
ford <- read.csv("/Users/jackcleary/Desktop/Stats 3rd Year/Project/Used Car Listings/ford.csv", sep=",", header=T, na.strings="?")
hyundai <- read.csv("/Users/jackcleary/Desktop/Stats 3rd Year/Project/Used Car Listings/hyundi.csv", sep=",", header=T, na.strings="?")
merc <- read.csv("/Users/jackcleary/Desktop/Stats 3rd Year/Project/Used Car Listings/merc.csv", sep=",", header=T, na.strings="?")
skoda <- read.csv("/Users/jackcleary/Desktop/Stats 3rd Year/Project/Used Car Listings/skoda.csv", sep=",", header=T, na.strings="?")
toyota <- read.csv("/Users/jackcleary/Desktop/Stats 3rd Year/Project/Used Car Listings/toyota.csv", sep=",", header=T, na.strings="?")
vauxhall <- read.csv("/Users/jackcleary/Desktop/Stats 3rd Year/Project/Used Car Listings/vauxhall.csv", sep=",", header=T, na.strings="?")
vw <- read.csv("/Users/jackcleary/Desktop/Stats 3rd Year/Project/Used Car Listings/vw.csv", sep=",", header=T, na.strings="?")

#Create vectors containing car brand names and column headings and combine datasets
columnList <- c("brand", "year", "transmission", "mileage", "fuelType", "tax", "mpg", "engineSize", "price")
brandNames <- c("audi", "bmw", "ford", "hyundai", "merc", "skoda", "toyota", "vauxhall", "vw")

#summaries of each separate dataset
summary(audi)
summary(bmw)
summary(ford)
summary(hyundai)
summary(merc)
summary(skoda)
summary(toyota)
summary(vauxhall)
summary(vw)

#Add a brand column, numbers 1-9 for each brand of car
audi$brand <- 1
bmw$brand <- 2
ford$brand <- 3
hyundai$brand <- 4
merc$brand <- 5
skoda$brand <- 6
toyota$brand <- 7
vauxhall$brand <- 8
vw$brand <- 9

#combine datasets
cars <- rbind(audi, bmw, ford, hyundai, merc, skoda, toyota, vauxhall, vw)

#Remove any cars from the datasets which do not have petrol or diesel engines as they make the study too complicated
#Remove any cars from the datasets which have Semi-Auto or Other transmissions as they make the study too complicated
#Also remove any invalid figures - zeros/NAs
cars$Classify <- 1
cars$Classify[ cars$fuelType == "Hybrid" | cars$fuelType == "Electric" | cars$fuelType == "Other" ] <- NA
cars$Classify[ cars$transmission == "Semi-Auto" | cars$transmission == "Other"] <- NA
cars$Classify[ cars$engineSize == 0 | cars$tax == 0 | cars$mpg == 0] <- NA
nrow(cars)

#Remove the NA values from the dataset
idx <- which( is.na(cars), arr.ind=TRUE )
miss <- cars[idx[,1],]
cars <- cars[-idx[,1],]

#Remove the Classify column as it is now meaningless
#Remove the model column as it makes the study to complicated
cars <- subset(cars, select = -c(Classify, model))
cars <- cars[,columnList]
nrow(cars)
ncol(cars)

#Change the transmission and fuel type Variables to Numbered Values
cars$transmission <- as.character.factor(cars$transmission)
cars$transmission[ cars$transmission == "Manual" ] <- 1
cars$transmission[ cars$transmission == "Automatic" ] <- 0

cars$fuelType <- as.character.factor(cars$fuelType)
cars$fuelType[ cars$fuelType == "Petrol" ] <- 1
cars$fuelType[ cars$fuelType == "Diesel" ] <- 0

head(cars)
cars$year <- 2020 - cars$year
head(cars)


#Write CSV file for submission
write.csv(cars, "/Users/jackcleary/Desktop/Stats 3rd Year/Project/Used Car Listings/UsedCars.csv", quote = FALSE)
cars <- subset(cars, select = -c(brand > 1))
cars

#Correlations
res <- cov(cars)
round(res, 2)


