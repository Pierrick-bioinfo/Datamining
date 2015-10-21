#  Data mining 2 - Apprentissage non supervisé
# Itemsets fréquents

# 21/10/2015

# Préparation

install.packages("arules")
library(arules)


# Motif fréquent maximum, frontière des motifs fréquents, n'existe pas un surmotif fréquent
# sup (r1) = sup (A,B) : support (probabilité) d'observer A et B enesmbles
# confiance (r1) = sup(A,B)/sup(A) = P(B|A)  : Probabilité d'bserver B sachant A
# La confiance doit généralement être très élevée, 0.8, 0.9
# On peut avoir des motifs rares "intéressants" avec des probabilités faibles
# On crée une matrice binaire avec les transactions rélisées
# On fait l'arbre des possibilités en regardant seulement ceux avec une occurence supérieure à notre seuil
# Motif M={a, b, e, ab}
# Motifs fréquents maximums : {ab, e} (on prend e en compte aussi vu que c'est le dernier)
# Construire les règles d'association avec une confiance de 0.7
# r1 : a => b : conf (r1) = 5/7 / 5/7 = 1 : sup à 0.7, donc on garde
# r2 : b => a : conf (r2) = 5/7 / 6/7 = 5/6 : sup à 0.7, donc on garde

# Motif fréquent


# Construction de transactions à la main

a_list <- list(
  c("a","b","c"),
  c("a","b"),
  c("a","b","d"),
  c("c","e"),
  c("a","b","d","e"),
  c("e","b","d"),
  c("a","b","e")
  )
#trans = read.transactions("a", "b", "c")
names(a_list)<-paste("Tr",c(1:7), sep="")
a_list
trans <-as(a_list, "transactions")
itemFrequencyPlot(trans, support = 0.7)
summary(trans)
image(trans)

# Ensembles de bases pour des transactions :
transitemfreq = apriori(trans,parameter=list(support = 0.3,target="frequent itemsets") ) # Utiliser  closed frequent itemsets pour supprimer les proba identiques (ex on suprime A à 5/7 car on a AB à 5/7)
transitemfreq
summary(transitemfreq)
write(transitemfreq)

# En faisant varier le support à 0.5
transitemfreq = apriori(trans,parameter=list(support = 0.7,target="frequent itemsets") )
transitemfreq
write(transitemfreq)

# En faisant varier le support à 0.7 en regardant les motifs maximaux
transitemfreq = apriori(trans,parameter=list(support = 0.7,target="maximally frequent itemsets") )
transitemfreq
write(transitemfreq)

# En faisant varier le support à 0.7 en regardant les règles d'association
transitemfreq = apriori(trans,parameter=list(support = 0.7,target="rules") )
transitemfreq
write(transitemfreq)

# En faisant varier le support à 0.7 en regardant les motifs maximaux de longueur min de 2
transitemfreq = apriori(trans,parameter=list(support = 0.7,target="maximally frequent itemsets", minlen = 2) )
transitemfreq
write(transitemfreq)
# Items set fermés max les plus fréquents : ab et e. Cela pouvait se voir à l'oeil nu sur l'image.

rul<-c("frequent itemsets","maximally frequent itemsets","rules","maximally frequent itemsets")
for( i in 1:length(rul)) 
{
  for( j in (seq(0.3,1, 0.2)))
       {
  transitemfreq = apriori(trans,parameter=list(support = j ,target=rul[i], minlen = 2) )
  write(transitemfreq)
  }
}

# 2) Apriori sur données d'annotation clininques
getwd()
setwd("/home/etudiant/Bureau")

# Changement du tableau en transactions
b_list <-read.table("annotation.tab")
trans <- as(b_list, "transactions")
image(trans)
write(trans)

# A priori sur les transactions. création d'une fonction
aprifonc =function(param)
{
  for( i in 1:length(rul)) 
  {
    for( j in (seq(0.3,1, 0.2)))
    {
      transitemfreq = apriori(param,parameter=list(support = j ,target=rul[i], minlen = 2) )
      write(transitemfreq)
    }
  }
}
aprifonc(trans)

# On a un itemsef fréquent sexe = M et grade = 3

transitemfreq = apriori(param,parameter=list(support = 0.2 ,target="rules"))
write(transitemfreq)

# 3) Apriori sur les données d'expression
# On charge les données, on les transforme en character, et on les discrétise
c_list <-read.table("expression.tab", as.is=T)
apply(c_list)
?apply
head(c_list)
