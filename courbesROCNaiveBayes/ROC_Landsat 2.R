# Installations
install.packages("naivebayes")
install.packages("e1071")
install.packages("ROCR")

# Importations
library(naivebayes)
library(e1071)
library(ROCR)

tab=read.table("Landsat.txt",header=T)
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

par(mfcol=c(3,2)) # Param?trage graphique pour diviser la fen?tre graphique en 3 lignes et 2 colonnes

for (i in 1:6) # boucle sur les modalit?s
{
  cl.pos=as.numeric(test[,37]==cible[i]) # classe positive = cotton crop
  proba.pos=predNB[,i] # probabilit? d'appartenir ? la classe positive pour NB
  prev=prediction(proba.pos,cl.pos)
  perf= performance(prev,"tpr","fpr")
  plot(perf,main=cible[i]) 
}

