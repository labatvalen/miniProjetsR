data(iris) # les donn?es iris sont d?j? dans R
attributes(iris) # pour avoir des info sur le jeu de donn?es (ex.noms des variables)
summary(iris) # r?sum? num?rique de chaque variable
# pour la classe setosa
c.setosa=subset(iris, Species=="setosa")
mean(c.setosa$Petal.Length)
var(c.setosa$Petal.Length)

pp=function(x1,x2,espece)
{
  # Entr?es :
  # x1 = longueur du p?tal
  # x2 = largeur du p?tal
  # espece = subset de iris en fonction de setosa, versicolor, virginica
  # sortie : la "probabilit?" (simplifi?e) d'appartenir ? la classe de espece pour le nouvel iris (x1,x2)
  
  mu1=mean(espece$Petal.Length)
  mu2=mean(espece$Petal.Width)
  sigma1=var(espece$Petal.Length)
  sigma2=var(espece$Petal.Width)
  exp(-((x1-mu1)^2)/(2*sigma1)-((x2-mu2)^2)/(2*sigma2))/(sqrt(sigma1*sigma2))
}

#pour la classe setosa
hist(c.setosa$Petal.Length,proba=T) # histogramme
x=seq(min(c.setosa$Petal.Length),max(c.setosa$Petal.Length),by=0.01) # vecteur de valeurs pour la longueur des petals avec un pas de 0.01

shapiro.test(c.setosa$Petal.Length)

cor(c.setosa[,3:4]) # matrice de corr?lation entre les colonnes 3 et 4

