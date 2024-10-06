#--------------------------------DATA PREP--------------
library(cluster)
library(mclust)
library(clustMD)
library(dplyr)
library(factoextra)

data(Byar)
par(mfrow = c(1,1))
# Transformation skewed variables
Byar$Size.of.primary.tumour <- sqrt(Byar$Size.of.primary.tumour)
Byar$Serum.prostatic.acid.phosphatase <- log(Byar$Serum.prostatic.acid.phosphatase)

# Order variables (Continuous, ordinal, nominal)
Y <- as.matrix(Byar[, c(1, 2, 5, 6, 8, 9, 10, 11, 3, 4, 12, 7)])

# Start categorical variables at 1 rather than 0
Y[, 9:12] <- Y[, 9:12] + 1

# Standardize continuous variables
Y[, 1:8] <- scale(Y[, 1:8])

# Merge categories of EKG variable for efficiency
Yekg <- rep(NA, nrow(Y))
Yekg[Y[,12]==1] <- 1
Yekg[(Y[,12]==2)|(Y[,12]==3)|(Y[,12]==4)] <- 2
Yekg[(Y[,12]==5)|(Y[,12]==6)|(Y[,12]==7)] <- 3
Y[, 12] <- Yekg

# -----------------------------CHECK OPTIMAL NUMBER OF CLUSTERS--------------------------------------

#getting the gower distance which is the average of partial dissimilarity ranges in [0 1]
gow <- daisy(Y, metric="gower")#gower distance
gow <- as.matrix(gow)

fviz_nbclust(gow, pam, method = "wss")
fviz_nbclust(gow, pam, method = "silhouette")
fviz_nbclust(gow, pam, method = "gap_stat")

#-------------------------------HIERARCHICAL----------------------------------------

#Complete linkage, gower distance
clusters1 <-hclust(daisy(Y, metric="gower"), method = "complete")
plot(clusters1, xlab = "Complete Linkage")
abline(h = 0.450075, col = "red")
abline(h = 0.55, col = "blue")
abline(h = 0.4, col = "green")
Cut1 <- cutree(clusters1, 3)
table(Cut1, Y[,12])

Cut2 <- cutree(clusters1, 2)
table(Cut2, Y[,12])
Cut4 <- cutree(clusters1, 4)
table(Cut4, Y[,12])

#Complete linkage, gower distance
clusters1 <-hclust(daisy(Y, metric="gower"), method = "average")
plot(clusters1, xlab = "Complete Linkage")

# -------------------------------K-MEDOIDS----------------------------------------

#partitioning the data into medoids using the pam function 
kmedoids <-pam(gow, 3)
head(kmedoids)

##accessing the details of the medoids
kmedoids$clusinfo

#carrying out a check of data so far
table(kmedoids$clustering, Y[,12])

fviz_cluster(kmedoids, data = Y, show.clust.cent = TRUE, stand = TRUE)
kmedoids$clusinfo
#-------------------------------CLUST MD--------------------------------------------
#install.packages("snow")
library(clustMD) 
model1 <- clustMDparallel(X = Y, G = 1:3, CnsIndx = 8, OrdIndx = 11, Nnorms = 20000,
                          MaxIter = 500, models = c("EVI", "EII", "VII"), store.params = FALSE, scale = TRUE,
                          startCL = "kmeans", autoStop= TRUE, ma.band=30, stop.tol=0.0001)
summary(model1)
plot(model1)
model1$ICLarray
plot(model1$BICarray)

model2 <- clustMD(X = Y, G = 3, CnsIndx = 8, OrdIndx = 11, Nnorms = 20000,
                  MaxIter = 500, model = "EVI", store.params = FALSE, scale = TRUE,
                  startCL = "kmeans", autoStop= TRUE, ma.band=30, stop.tol=0.0001)
table(model2$cl, Y[,12])
table(model2$cl)



