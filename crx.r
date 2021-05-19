#---
#   title: "Credit Screening"
#   author: "Jordan Salmon"
#   date: "5/18/2021"
#---

####Retrieval and Initial Exploration of the Data####

crx <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", header = FALSE, stringsAsFactors = TRUE, na.strings = "?")
show(crx)
str(crx)

####Data Cleaning and Preparation#####

crx <- crx[complete.cases(crx),]
for (i in 1:length(crx)) {
  if (is.numeric(crx[,i])) {
    hist(crx[,i])
    crx[,i] <- log(crx[,i] + 1)
    hist(crx[,i], main = paste("Histogram (after) of", names(crx)[i]))
    boxplot(crx[,i], main = paste("Boxplot for", names(crx)[i]))
  }
}

#TODO: Decide what to do about the outliers

####Clustering####

crx.copy <- crx
for (column in names(crx.copy)) {
  if (is.factor(crx.copy[,column])) crx.copy[,column] <- as.integer(crx.copy[,column])
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

trainingData <- crx[caTools::sample.split(crx[,16]),]
testData <- crx[-as.integer(row.names(trainingData)),]
tree <- rpart::rpart(X. ~ ., trainingData)
rpart.plot::rpart.plot(tree)
prediction <- predict(tree, testData)
pprob <- predict(tree, testData, type = "prob")
confusionMatrix <- table(predicted = prediction, actual = testData[,16])
ROC <- pROC::roc(testData[,16], pprob)
plot(ROC)
