eu_data <- read.csv(file = '/Users/jackcleary/Desktop/Stats 3rd Year/Lab 2/EU_2019_01_02.csv')

#Part 1
#creates subset of the eu_data set with only those people aged between 30 and 50 inc.
new_eu_data1 <- subset(eu_data, age < 51 & age > 29) 
print(new_eu_data1)
print(ncol(new_eu_data1))
print(nrow(new_eu_data1))
write.csv(new_eu_data1, "EU_cust_30_50.csv")

#creates subset containing only those with tertiary education and either self-employed or entrepreneur jobs
new_eu_data2 <- subset(eu_data, education == "tertiary" & (job == "self-employed" | job == "entrepreneur"))
print(new_eu_data2)
print(ncol(new_eu_data2))
print(nrow(new_eu_data2))
write.csv(new_eu_data2, "EU_cust_ter_se_entre.csv")

#Part 2
#create list of possible occupations
occupations <- c("admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
                 "blue-collar","self-employed","retired","technician","services")
#cycle through list of occupations and create a csv file for each type
for (i in 1:12){
  new_data <- subset(eu_data, job == occupations[i])
  filename <- paste("EU_cust_occupation_", occupations[i],".csv", sep = "") #create string for naming csv files correctly
  write.csv(new_data, filename)
}

#Part 3
eu_data <- read.csv(file = '/Users/jackcleary/Desktop/Stats 3rd Year/Lab 2/EU_2019_01_02.csv')
us_data <- read.csv(file = '/Users/jackcleary/Desktop/Stats 3rd Year/Lab 2/US_2019_02_01.csv', sep = ";")

for (i in 1:nrow(us_data)){
  us_data$balance[i] <- us_data$balance[i] * 0.87
  us_data$poutcome <- "unknown"
  us_data$origin <- "US"
}

for (j in 1:nrow(eu_data)){
  eu_data$origin <- "EU"
  if (eu_data$contact[j] == "telephone"){
    eu_data[[9]][j] = "teleph"
  }
}
print(us_data)
print(eu_data)
