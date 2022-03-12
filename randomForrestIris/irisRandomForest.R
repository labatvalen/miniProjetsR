install.packages("scatterplot3d")
library("scatterplot3d")

### R code from vignette source 'ch-exploration.rnw'

###################################################
### code chunk number 1: ch-exploration.rnw:6-9
###################################################
# free memory
rm(list = ls())
gc()


###################################################
### code chunk number 2: ch-exploration.rnw:20-24
###################################################
dim(iris)
names(iris)
str(iris)
attributes(iris)


###################################################
### code chunk number 3: ch-exploration.rnw:28-31
###################################################
iris[1:5,]
head(iris)
tail(iris)


###################################################
### code chunk number 4: ch-exploration.rnw:36-38
###################################################
iris[1:10, "Sepal.Length"]
iris$Sepal.Length[1:10]


###################################################
### code chunk number 5: ch-exploration.rnw:48-49
###################################################
summary(iris)


###################################################
### code chunk number 6: ch-exploration.rnw:53-55
###################################################
quantile(iris$Sepal.Length)
quantile(iris$Sepal.Length, c(.1, .3, .65))


###################################################
### code chunk number 7: ch-exploration.rnw:63-65
###################################################
var(iris$Sepal.Length)
hist(iris$Sepal.Length)


###################################################
### code chunk number 8: ch-exploration.rnw:74-75
###################################################
plot(density(iris$Sepal.Length))


###################################################
### code chunk number 9: ch-exploration.rnw:85-87
###################################################
table(iris$Species)
pie(table(iris$Species))


###################################################
### code chunk number 10: ch-exploration.rnw:95-96
###################################################
barplot(table(iris$Species))


###################################################
### code chunk number 11: ch-exploration.rnw:105-109
###################################################
cov(iris$Sepal.Length, iris$Petal.Length)
cov(iris[,1:4])
cor(iris$Sepal.Length, iris$Petal.Length)
cor(iris[,1:4])


###################################################
### code chunk number 12: ch-exploration.rnw:114-115
###################################################
aggregate(Sepal.Length ~ Species, summary, data=iris)


###################################################
### code chunk number 13: ch-exploration.rnw:123-124
###################################################
boxplot(Sepal.Length~Species, data=iris)


###################################################
### code chunk number 14: ch-exploration.rnw:134-135
###################################################
with(iris, plot(Sepal.Length, Sepal.Width, col=Species, pch=as.numeric(Species)))



###################################################
### code chunk number 16: ch-exploration.rnw:159-160
###################################################
pairs(iris)


###################################################
### code chunk number 17: ch-exploration.rnw:177-179
###################################################
library(scatterplot3d)
scatterplot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)




###################################################
### code chunk number 23: ch-exploration.rnw:257-259
###################################################
library(MASS)
parcoord(iris[1:4], col=iris$Species)


###################################################
### code chunk number 24: ch-exploration.rnw:269-271
###################################################
library(lattice)
parallelplot(~iris[1:4] | Species, data=iris)


###################################################
### code chunk number 25: ch-exploration.rnw:283-285
###################################################
library(ggplot2)
library("farver")
qplot(Sepal.Length, Sepal.Width, data=iris, facets=Species ~.)


###################################################
### code chunk number 2: ch-decision-trees.rnw:23-28
###################################################
str(iris)
set.seed(1234) 
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]



myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

###################################################
### code chunk number 14: ch-decision-trees.rnw:169-174
###################################################

library(rpart)

iris_rpart <- rpart(myFormula, data=trainData, 
                    control = rpart.control(minsplit = 5))
attributes(iris_rpart)

print(iris_rpart)


###################################################
### code chunk number 10: ch-decision-trees.rnw:114-116
###################################################


library(rpart.plot)
prp(iris_rpart,extra=1)

trainPred <- predict(iris_rpart, newdata = trainData, type="class")
table(trainPred, trainData$Species)

testPred <- predict(iris_rpart, newdata = testData, type="class")
table(testPred, testData$Species)





library(randomForest)
rf <- randomForest(Species ~ ., data=trainData, ntree=100, proximity=TRUE)
table(predict(rf), trainData$Species)
print(rf)
attributes(rf)
###################################################
### code chunk number 16: ch-decision-trees.rnw:193-195
###################################################
importance(rf)
varImpPlot(rf)


###################################################
### code chunk number 17: ch-decision-trees.rnw:205-208
###################################################
irisPred <- predict(rf, newdata=testData)
table(irisPred, testData$Species)
plot(margin(rf, testData$Species))



###################################################
### code chunk number 3: ch-clustering.rnw:22-25
###################################################
iris2 <- iris
iris2$Species <- NULL
(kmeans.result <- kmeans(iris2, 7)) 
kmeans.result$totss
kmeans.result$withinss
kmeans.result$tot.withinss
kmeans.result$betweenss



###################################################
### code chunk number 4: ch-clustering.rnw:30-31
###################################################
table(iris$Species, kmeans.result$cluster)



###################################################
### code chunk number 5: ch-clustering.rnw:38-42
###################################################
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
# plot cluster centers
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, 
       pch = 8, cex=2)


###################################################
### code chunk number 10: ch-clustering.rnw:131-132
###################################################
set.seed(2835)


###################################################
### code chunk number 11: ch-clustering.rnw:134-138
###################################################
idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method="ave")


###################################################
### code chunk number 12: ch-clustering.rnw:144-148
###################################################

plot(hc, labels=iris$Species[idx])
# cut tree into 3 clusters
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)

###################################################
### Application of naive bayes
###################################################


library(e1071)
model=naiveBayes(Species ~ .,data=trainData)
attributes(model)

trainPredBayes <- predict(model, newdata = trainData)
table(trainPredBayes, trainData$Species)
testPredBayes <-predict(model,newdata=testData) 
table(testPredBayes, testData$Species)