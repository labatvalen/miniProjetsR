library("mlbench")
library("e1071")
library(ROCR)

data(HouseVotes84, package = "mlbench")
model = naiveBayes(Class ~ ., data = HouseVotes84) # construit le mod?le Na?ve Bayes
pred = predict(model, HouseVotes84, type = "raw") # calcul les pr?visions
proba.pos=pred[,1] # probabilit? d'appartenir ? la classe positive
cl.pos=as.numeric(HouseVotes84[,1]=="democrat") # classe positive = democrat
prev=prediction(proba.pos,cl.pos)
perf= performance(prev,"tpr","fpr")
plot(perf)
