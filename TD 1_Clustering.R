# Lecture de données
# Utiliser Markdown pour le code

setwd("/home/etudiant/Bureau/TDR/") # On défini le répertoire de travail
getwd() # On vérifie que le répertoire de travail est le bon

# Read.table : lecture du fichier qu'il va stocker dans une data frame
expression <- as.matrix(read.table("expression.tab",as.is=T, header=TRUE, sep="")) 
annotation <- read.table("annotation.tab",as.is=T)

class(expression) # On vérifie que c'est une matrice
class(annotation) # On vérifie que c'est un data frame

str(expression) # Structure de notre objet
summary(expression)

str(annotation)
summary(annotation)

# Plot de l'expression des 2 genes
plot(expression[,1:2], col=kmeans(expression[,1:2],2)$cluster)

DDXY3<-expression[,"DDX3Y"]
RPS4Y1<-expression[,"RPS4Y1"]
DDXY3_RPS4Y1<-expression[,c("DDX3Y","RPS4Y1")]

str(DDXY3_RPS4Y1)

#png("DDXBY et DDX3Y.png") # Quand on crée un graphique on peut l'enregistrer dans un fichier (ex .png)
#plot(DDXY3, type="p", col="blue", xlab="expression")
#par(new = TRUE)
#plot(RPS4Y1, type="p", col="green", xlab="expression")
#devoff() # On arrête le processus

?kmeans # Classification en classes, on ne connait pas à l'avance le nombre de classes

visual<-kmeans(DDXY3_RPS4Y1, centers = 1)$betweenss
str(visual)
plot(visual, type = "p", col="red", xlab = "clusters")

# Graphique de la distance interclasse entre les différents nombres de clusters

inter = c(0)
for (i in 2:56) # Pour toutes nos valeurs de genes on fait un kmean avec x cluster  en regardant la distance intergene
{
  inter=c(inter, kmeans(DDXY3_RPS4Y1,i)$betweenss)
  
}
plot(inter, col="blue", xlab = "Nombre de clusters")

# Fonction pour faire une graphique avec le resultat du clustering
# Syntaxe d'une fonction : nomfonction = Function (arg1, arg2...) { paramètres }
# Pour appeler une fonction, nom de la fonction avec les arguments :  nomfonction(arg)


graphStade = function (data) {
km =  kmeans(data, centers = 7)
plot(DDXY3_RPS4Y1, pch=km$cluster, col = factor(annotation$grade))
}
graphStade(DDXY3_RPS4Y1)

# Clustering hiérarchique
?dist()
dist (expression)
#hclust prend en argument une matrice de distance (ce que expression ne donne pas), "dist" la calcule
?hclust
x<-hclust(dist(expression))
plot(x, main="Dendrogramme selon grade", labels = factor(annotation$grade))
plot(x, main="Dendrogramme selon stade", labels = factor(annotation$stade))
?plot


# Heatmap

?heatmap
#rc<-rainbow(nrow(annotation), start = 0, end = 0.9)
#heatmap(expression, RowSideColors = rc)

greenred = colorRampPalette(c("green","black","red")) # Palette de couleur qui varie entre les verts, noirs et rouges selon le profil d'expression
heatmap(expression, col=greenred(32)) # Greenred32 : palette des couleur
heatmap(expression, RowSideColors = c("G0"="green", "G1"="red", "G2"="black", "G3"="blue")[annotation$grade], col = greenred(32))

# Supplementaries
km=kmeans(expression,5)
km$size
km$tot.withinss

plot(km, col=factor(annotation$stade))
