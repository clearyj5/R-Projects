# Wisconsin breast cancer data
# the file is comma separated values, no header
# missing is coded as ?
X <- read.table( "/Users/jackcleary/Desktop/Stats 3rd Year/Lab 6/breast_cancer.dat", sep=",", header=T, na.strings="?" )
X
# recode the class_tumor as benign == 0
X$class_tumor[ X$class_tumor == 2 ] <- 0
X$class_tumor[ X$class_tumor == 4 ] <- 1
X
#general summary
summary(X)

# find out which rows have NA's
idx <- which( is.na(X), arr.ind=TRUE )
miss <- X[idx[,1],]
#investigate missing values
table(miss$class_tumor)

# remove these rows
X <- X[-idx[,1],]
# create a datframe without the id_number
Y <- X[,-1]
Y
# create a datframe without the classification
Z <- subset(Y, select =-c(class_tumor))
Z

#install.packages("data.table")
#library(data.table)
W <- Y
setDT(W)
#summary stats by group
W[, as.list(summary(W)), by = class_tumor] 

W[, lapply(.SD, median), by = class_tumor]  # Summarize by group

#frequencies by group
apply(Y, 2, function(z) ftable(z,Y$class_tumor))

#correlations
res <- cor(Y)
round(res, 2)

#some graphs
#install.packages("corrplot")
library(corrplot)
corrplot(res, tl.col = "black", tl.srt = 45)

par(mfrow=c(1,1))
boxplot(Z)
boxplot(mitoses ~class_tumor , data = Y)

par(mfrow=c(3,3))
sink("NUL")    # now suppresses
apply(Z, 2, function(z) boxplot(z~Y$class_tumor))
sink() 

mylist <- names(Z)
mylist
for (i in 1:length(mylist)) { # Loop over loop.vector
  # store data in column.i as x
  x <- Z[,i]
  name <- mylist[i]
  boxplot(x~Y$class_tumor, main=name)
}
