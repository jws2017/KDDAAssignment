#---
#   title: "Credit Screening"
#   author: "Jordan Salmon"
#   date: "5/18/2021"
#---

####Retrieval and Initial Exploration of the Data####

crx <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", stringsAsFactors = TRUE, na.strings = "?")
show(crx)
str(crx)

####Data Cleaning and Preparation#####
crx <- crx[complete.cases(crx),]
for (i in 1:length(crx)) {
  if (class(crx[,i]) == "numeric" | class(crx[,i]) == "integer") hist(crx[,i], xlab = names(crx)[i], main = paste("Histogram of", names(crx)[i]))
}
crx <- crx[crx$X01 < 67,]
hist(crx$X01)
crx <- crx[crx$X00202 < 2000,]
hist(crx$X00202)
crx <- crx[crx$X1.25 < 20,]
hist(crx$X1.25)
crx$X. <- as.logical(as.numeric(crx$X.)-1)

####Clustering####

crx.copy <- crx
for (column in names(crx.copy)) {
  if (class(crx.copy[,column]) == "factor") crx.copy[,column] <- as.integer(crx.copy[,column])
}
clust <- hclust(dist(crx.copy))
kclust4 <- kmeans(crx.copy, 4)
kclust6 <- kmeans(crx.copy, 6)
kclust8 <- kmeans(crx.copy, 8)

plot(clust)

####Association Rules#####
library("arules")
library("arulesViz")
crxT <- as(crx, "transactions")
crxRules <- apriori(crxT, parameter = list(confidence = 0.8, support = 0.25, minlen = 2, maxlen = 4))
print(inspect(crxRules))

####Decision Trees####

trainingData <- crx[caTools::sample.split(crx$X.),]
testData <- crx[-as.integer(row.names(trainingData)),]
tree <- rpart::rpart(X. ~ ., trainingData)
rpart.plot::rpart.plot(tree)
prediction <- predict(tree, testData)
pprob <- predict(tree, testData, type = "prob")
confusionMatrix <- table(predicted = prediction, actual = testData$X.)
ROC <- pROC::roc(testData$X., pprob)
plot(ROC)
