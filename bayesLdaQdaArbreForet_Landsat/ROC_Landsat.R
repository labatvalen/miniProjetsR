# Installations
install.packages("e1071")
install.packages("MASS")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("ROCR")

# Importations
library(e1071)
library(MASS)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

tab=read.table("Landsat.txt",header=T)

# R?duction du jeu de donn?es ? 30%
n=nrow(tab)
idx=sample(1:n,floor(n*0.3),replace=F)
tab=tab[idx,]

tab$classes=as.factor(tab$classes)

# Bases d'apprentissage et de test
n=nrow(tab)
ntrain=floor(2*n/3)
idx=sample(1:n,ntrain,replace=F)
train=tab[idx,]
test=tab[-idx,]

# Affichage de la courbe de ROC
cible=levels(tab$classes) # vecteur avec les noms des modalit?s de la variable cible

# Naive Bayes
modeleNB=naiveBayes(classes~.,data=train)
predNB = predict(modeleNB, test, type = "raw") # calcule les probabilit?s des deux classes
# LDA et QDA
modeleLDA=lda(classes~., data=train)
predLDA = predict(modeleLDA, test, type = "raw")$posterior
modeleQDA=qda(classes~., data=train)
predQDA = predict(modeleQDA, test, type = "raw")$posterior

# Arbre de d?sicion
modeleTree=rpart(classes~.,data=train,control=rpart.control(minsplit=5,cp=0))
plotcp(modeleTree)
modeleTree=prune(modeleTree,cp=0.0033)
prp(modeleTree,extra=1)
predTree = predict(modeleTree, test, type = "prob") # calcule les probabilit?s des deux classes

# Foret al?atoire
modeleRF=tune.randomForest(classes~.,data=train,mtry=c(6,8,10))$best.model
predRF = predict(modeleRF, test, type = "prob") # calcule les probabilit?s des deux classes

par(mfcol=c(3,2)) # Param?trage graphique pour diviser la fen?tre graphique en 3 lignes et 2 colonnes

for (i in 1:6) # boucle sur les modalit?s
{
  cl.pos=as.numeric(test[,37]==cible[i]) # classe positive = cotton crop
  # NB
  proba.posNB=predNB[,i] # probabilit? d'appartenir ? la classe positive pour NB
  prevNB=prediction(proba.posNB,cl.pos)
  perfNB= performance(prevNB,"tpr","fpr")
  plot(perfNB,main=cible[i]) 
  # lda
  proba.posLDA=predLDA[,i] # probabilit? d'appartenir ? la classe positive pour LDA
  prevLDA=prediction(proba.posLDA,cl.pos)
  perfLDA= performance(prevLDA,"tpr","fpr")
  plot(perfLDA,main=cible[i],add=T,lty="dashed") 
  # qda
  proba.posQDA=predQDA[,i] # probabilit? d'appartenir ? la classe positive pour QDA
  prevQDA=prediction(proba.posQDA,cl.pos)
  perfQDA= performance(prevQDA,"tpr","fpr")
  plot(perfQDA,main=cible[i],add=T,lty="dotted") 
  # Tree
  proba.posTree=predTree[,i] # probabilit? d'appartenir ? la classe positive pour Tree
  prevTree=prediction(proba.posTree,cl.pos)
  perfTree= performance(prevTree,"tpr","fpr")
  plot(perfTree,main=cible[i],add=T,col="red") 
  # RF
  proba.posRF=predRF[,i] # probabilit? d'appartenir ? la classe positive pour Tree
  prevRF=prediction(proba.posRF,cl.pos)
  perfRF= performance(prevRF,"tpr","fpr")
  plot(perfRF,main=cible[i],add=T,col="green") 
}

