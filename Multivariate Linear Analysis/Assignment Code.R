library(cluster)
library(flexclust)
library("class")
library(MASS)
library("caret")
library(ellipse)
hfd <- read.csv("/Users/jackcleary/Desktop/Multivariate Linear Analysis/hfd_sub.csv")

#-------------------SUMMARY STATISTICS---------------------------------
summary(hfd)
nrow(hfd)
ncol(hfd)
head(hfd)

#Create DF without gates
hfd1 <- hfd[,-5]

pairs(hfd1, col = hfd[,5])

library(ggplot2)
# Barplot to show the sizes of each gate
gatesDF <- data.frame(gateNumber = c(1, 2, 3, 4), gateCount = c(nrow(subset(hfd, gate == '1')), nrow(subset(hfd, gate == '2')), nrow(subset(hfd, gate == '3')), nrow(subset(hfd, gate == '4'))))
gatesDF
p <- ggplot(data=gatesDF, aes(x=gateNumber, y=gateCount)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=gateCount), vjust=1.6, color="white", size=4)+
  theme_minimal()
p + labs(x="Gate Number", y="Number of Observations")

#variance, covariance and correlation
res <- var(hfd)
round(res, 2)

res2 <- cov(hfd, method = "spearman")
round(res2, 2)

cor(hfd)

#### For unsupervised analysis, we will omit the variable "gate"
#-------------------------PCA------------------------------------------
PCA_hfd <- prcomp(hfd[,1:4])
PCA_hfd
summary(PCA_hfd)

# PC1 ~ v small values ~ all neg bar CD19
# The first 3 PCs explain 98.3% of the variance
plot(PCA_hfd, main = "Healthy Flow Data", col = "red")
plot(PCA_hfd, main = "Scree Plot", type = "l")
# Based on the scree plot and barchart, we can consider the first 3 PCs
newHfd <- predict(PCA_hfd)
head(newHfd, n = 5)

# Plotting the Principle components based on class
plot(newHfd[,1], newHfd[,2], type="n", xlab="PC1", ylab="PC2")
text(newHfd[,1], newHfd[,2], labels = substr(hfd[,5],1,1), col = hfd[,5])

### Plot pairs
pairs(hfd[,1:2], col = hfd[,5])
# only cont.
pairs(newHfd, col = hfd[,5])
# but if we only interested in first three PCs
pairs(newHfd[,1:3], col = hfd[,5])

#-------------------------HIERARCHICAL------------------------------------------

#Complete Linkage
euclid1 <- dist(hfd1, method = "euclidean")
clusters1 <- hclust(euclid1, method = "complete")

#plot(clusters1, xlab = "Complete Linkage")
abline(h = mean(clusters1$height) + 3*sd(clusters1$height), col = "red")
mean(clusters1$height) + 3*sd(clusters1$height)
Cut1 <- cutree(clusters1, 4)

table(Cut1, hfd[,5])

randIndex(Cut1, hfd[,5], correct = FALSE)
randIndex(Cut1, hfd[,5])

#Average Linkage
euclid2 <- dist(hfd1, method = "euclidean")
clusters2 <- hclust(euclid2, method = "average")

plot(clusters2, xlab = "Average Linkage")
abline(h = 2, col = "red")
mean(clusters2$height) + 3*sd(clusters2$height)

Cut2 <- cutree(clusters2, 4)
table(Cut2, hfd[,5])

randIndex(Cut2, hfd[,5], correct = FALSE)
randIndex(Cut2, hfd[,5])

#Single Linkage
euclid3 <- dist(hfd1, method = "euclidean")
clusters3 <- hclust(euclid3, method = "single")

plot(clusters3, xlab = "Single Linkage")
abline(h = mean(clusters3$height) + 3*sd(clusters3$height), col = "red")

Cut3 <- cutree(clusters3, 4)
table(Cut3, hfd[,5])

randIndex(Cut3, hfd[,5], correct = FALSE)
randIndex(Cut3, hfd[,5])


#-------------------------K-MEANS-------------------------------------
euclid <- dist(hfd1, method = "euclidean")

#Find optimal K
par(mfrow = c(1,3))
fviz_nbclust(hfd1, kmeans, method = "wss")
fviz_nbclust(hfd1, kmeans, method = "silhouette")
#fviz_nbclust(hfd1, kmeans, method = "gap_stat")

#Using k = 3
kmeans <- kmeans(hfd1, 3)
table(kmeans$cluster, hfd[,5])

randIndex(kmeans$cluster, hfd[,5], correct = FALSE)
randIndex(kmeans$cluster, hfd[,5])

pairs(hfd1, col = kmeans$cluster)
plot(hfd1, col = kmeans$cluster)

#Using k = 4
kmeans1 <- kmeans(hfd1, 4)
table(kmeans1$cluster, hfd[,5])
kmeans1$centers

randIndex(kmeans1$cluster, hfd[,5], correct = FALSE)
randIndex(kmeans1$cluster, hfd[,5])

pairs(hfd1, col = kmeans1$cluster)
plot(hfd1, col = kmeans1$cluster)

pairs(hfd1, col =  (hfd[,5] + 4))


#-------------------------K-NEAREST NEIGHBOURS-------------------------------------

library(MASS)
#install.packages("class")
library("class")

## order according to class
hfd <- hfd[order(hfd[,5], decreasing = F),]

#Split into Training, Test and Validation
trainingIndex <- c(1:696, 1:295 + 2089, 1:109 + 2974,1:188 + 3301 )
testIndex <- c(1:696 + 696, 1:295 + 2089 + 295, 1:109 + 2974 + 109, 1:188 + 3301 + 188 )
validIndex <- c(1:697 + 1392, 1:295 + 2089 + 590, 1:109 + 2974 + 218,1:187 + 3301 + 376)

train <- hfd[trainingIndex, 1:4]
test <- hfd[testIndex, 1:4]
valid <- hfd[validIndex, 1:4]

#for function of k
kmax <- 50
k <- 1:kmax
p <- rep(0, kmax)
ntest <- nrow(test)

k_summary <- cbind(k, p)
colnames(k_summary) <- c("k","% misclassified")

for(i in 1:kmax){
  result <- knn(train, test, cl = hfd[trainingIndex, 5], k = i)
  class_agree <- table(result, hfd[testIndex,5])
  sum_agree <- sum(diag(class_agree))
  k_summary[i, 2] <- 100* ((ntest - sum_agree) / ntest)
}
k_summary[1:15, ]
par(mfrow = c(1,1))
plot(k_summary[1:12,], type = "l", ylim = c(0,.6))
abline(v=4, lty = 2, col = "red")
abline(v=7, lty = 2, col = "blue")


#re-run for k=4
result4 <- knn(train, valid, cl = hfd[trainingIndex, 5], k=4)
class_agree4 <- table(result4, hfd[trainingIndex,5])
class_agree4
sum_class_agree4 <- sum(diag(class_agree4))
sum_class_agree4
misclass_rate4 <- (nrow(valid)-sum_class_agree4)/nrow(valid)
misclass_rate4
confusionMatrix(class_agree4)


#re-run for k=7
result7 <- knn(train, valid, cl = hfd[trainingIndex, 5], k=7)
class_agree7 <- table(result7, hfd[trainingIndex,5])
class_agree7
sum_class_agree7 <- sum(diag(class_agree7))
sum_class_agree7
misclass_rate7 <- (nrow(valid)-sum_class_agree7)/nrow(valid)
misclass_rate7

confusionMatrix(class_agree7)

#-------------------------DISCRIMINANT ANALYSIS-------------------------------------

#Plot graphs showing covariances
hfd <- hfd[order(hfd[,5], decreasing =F),]
plot(hfd [,c(1,3)], col = hfd[,5])
lines(ellipse(cov(hfd [c(1:2089), c(1, 3)]), centre = colMeans(hfd [c(1:2089), c(1, 3)]),level = c(0.8)), col = "yellow")
lines(ellipse(cov(hfd [c(2090:2975), c(1, 3)]), centre = colMeans(hfd [c(2090:2975), c(1, 3)]), level = c(0.8)), col = "black")
lines(ellipse(cov(hfd [c(2976:3303), c(1, 3)]), centre = colMeans(hfd [c(2976:3303), c(1, 3)]), level = c(0.8)))
lines(ellipse(cov(hfd [c(3304:3864), c(1, 3)]), centre = colMeans(hfd [c(3304:3864), c(1, 3)]), level = c(0.8)))
plot(hfd [,c(2,4)], col = hfd[,5])
lines(ellipse(cov(hfd [c(1:2089), c(2, 4)]), centre = colMeans(hfd [c(1:2089), c(2, 4)]),level = c(0.8)), col = "yellow")
lines(ellipse(cov(hfd [c(2090:2975), c(2, 4)]), centre = colMeans(hfd [c(2090:2975), c(2, 4)]), level = c(0.8)))
lines(ellipse(cov(hfd [c(2976:3303), c(2, 4)]), centre = colMeans(hfd [c(2976:3303), c(2, 4)]), level = c(0.8)))
lines(ellipse(cov(hfd [c(3304:3864), c(2, 4)]), centre = colMeans(hfd [c(3304:3864), c(2, 4)]), level = c(0.8)), col = "yellow")

#Split data using 80:20 split
trainingIndex2 <- c(1:1671,2090:2797,2975:3236, 3302:3751)
testIndex2 <- c(1672:2089, 2798:2974, 3237:3301, 3752:3864)
train2 <- hfd[trainingIndex2,]
test2 <- hfd[testIndex2,]

#QDA
qsol <- qda(train2[, c(1:4)], grouping = train2[,5])
QDApred <- predict(qsol, test2[, c(1:4)])
QDAagree <- table(QDApred$class, test2[,5])
rownames(QDAagree) <- c("Predict 1", "Predict 2", "Predict 3", "Predict 4")
colnames(QDAagree) <- c("Gate 1", "Gate 2", "Gate 3", "Gate 4")
QDAagree



plot(hfd[, c(1,3)], col = as.factor(hfd[, 5]), pch = as.numeric(qsol$class))
lines(ellipse(cov(hfd[c(1:1671), c(1,3)]), centre = colMeans(hfd[c(1:1671), c(1,3)]), level = c(0.5)), col = "yellow")
lines(ellipse(cov(hfd[c(2090:2797), c(1,3)]), centre = colMeans(hfd[c(2090:2797), c(1,3)]),level = c(0.5)), col = "black")
lines(ellipse(cov(hfd[c(2975:3236), c(1,3)]), centre = colMeans(hfd[c(2975:3236), c(1,3)]),level = c(0.5)), col = "red")
lines(ellipse(cov(hfd[c(3302:3751), c(1,3)]), centre = colMeans(hfd[c(3302:3751), c(1,3)]),level = c(0.5)), col = "red")





