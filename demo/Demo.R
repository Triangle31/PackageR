


remove.packages("Equipe4.Chi2")
devtools::install("C:\\Users\\samfo\\OneDrive\\Bureau\\R\\Projet 2\\Equipe4.Chi2")
library(Equipe4.Chi2)



#ollama_generate
#ollama_generate("Bonjour!")


data<-HairEyeColor
data_test<-margin.table(data, c(1,2)) #La fonction s'applique à une table de contingene ou matrice

#Pour tester les méthodes
fonction_context<-chisq_table_context(x=data_test) #Définition de la fonction contextuelle

print(fonction_context)
summary(fonction_context)

plot(fonction_context,"mosaique")
plot(fonction_context,"mosaique2")

plot(fonction_context,"heatmap")
plot(fonction_context,"heatmap2")

plot(fonction_context,"barplot")
plot(fonction_context,"balloon")


## KNN

set.seed(123)

groupe <- factor(rep(c("A", "B", "C"), each = 50))

# Moyennes par groupe
moyennes <- c(A = 10, B = 20, C = 30)

# Générer var1 corrélée au groupe
var1 <- rnorm(150, mean = moyennes[groupe], sd = 2)

# Générer var2 corrélée à var1 (relation linéaire + bruit faible)
var2 <- 0.8 * var1 + rnorm(150, mean = 0, sd = 1)

df <- data.frame(var1, var2, groupe)

df


testKnn <- knn_context(df,groupe,var1,var2)


plot(testKnn)




