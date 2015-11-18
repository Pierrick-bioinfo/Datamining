# TD 4 Elati
# 04/11/15

# Arbres de décision


setwd("/home/etudiant/Bureau")
library(rpart)
install.packages(rpart)

data(kyphosis)
dim(kyphosis)

?kyphosis
kyphosis
class(kyphosis)
head(kyphosis)
names(kyphosis)
colnames(kyphosis)
head(kyphosis$Kyphosis)
class(kyphosis$Kyphosis)

head(kyphosis$Age)
class(kyphosis$Age)

#Construction de l'arbre de donées
tree = rpart(Kyphosis ~ Age + Number+Start,data=kyphosis)
print(tree)
plot(tree)
text(tree, use.n=TRUE)
post(tree) # produit un fichier postscript (ps)


# Avec le paramètre mintsplit : 
# The minimum number of observations that must exist in a node in order for a split to be attempted.
tree = rpart(Kyphosis ~ Age + Number+Start, data=kyphosis, control=rpart.control(minsplit=10) )
plot(tree,compress=T)
text(tree, use.n=TRUE)

# Sur toutes les data
tree =  rpart(Kyphosis ~ .,data=kyphosis)
plot(tree,compress=T)
text(tree, use.n=TRUE)

# Données prédict
?predict.rpart
nouvellesDonnees = data.frame(Age=c(56,90), Start=c(15,3),Number=c(3,2),Kyphosis=c("absent","absent"))
predict(tree,newdata= nouvellesDonnees,type="class")
predict(tree,newdata= nouvellesDonnees,type="prob")

# Evaluation sur les données d'apprentissage
tree = rpart(Kyphosis ~ Age + Number+Start,data=kyphosis)
pred = predict(tree,newdata=kyphosis,type="class")
# On crée la matrice de confusion
matriceConfusion = table(kyphosis$Kyphosis,pred)

?predict
Tn = matriceConfusion[1,1]
Fn = matriceConfusion[1,2]
Fp = matriceConfusion[2,1]
Tp = matriceConfusion[2,2]

# On crée les fonctions sensibilite, accuracy, specificite
sensibilite = function(Tp, Fn)
{
  return(Tp/ (Tp + Fn))
}
sensibilite(Tp, Fn)

accuracy = function(Tn, Fn, Fp, Tp)
{
  return((Tp + Tn)/ (Tp + Fn + Fp + Tn))
}
accuracy(Tp, Fn, Fp, Tp)

specificite = function(Tn, Fp)
{
  return(Tn/ (Tn + Fp))
}
specificite(Tn, Fp)

# Classifier bayesien naif

install.packages("e1071")
library(e1071)

classifierNB = naiveBayes(Kyphosis ~ Age + Number+Start,data=kyphosis)
predict(classifierNB,newdata=kyphosis)
predict(classifierNB,newdata=kyphosis, type="raw")
?predict

# Installation du package ROCR
install.packages("ROCR")
library(ROCR)

# Donne le modèle selon les classifiers 
# Doit être le plus proche de 0 pour les Fp et plus proche de 1 pour les Tp
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

# Validation croisée, courbe moyenne avec les boxplots
data(ROCR.xval)
pred <- prediction(ROCR.xval$predictions, ROCR.xval$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,col="grey82",lty=3)
plot(perf,lwd=3,avg="vertical",spread.estimate="boxplot",add=TRUE)

# Construction de classifieurs dans les cancers
data <- read.table("data1.tab",colClass="factor")
