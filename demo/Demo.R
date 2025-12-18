

          # Installation du package

devtools::install("chemin vers le package")
library(Equipe4.Chi2)



          # Démo du chisq_table


# 1. Importation des données
data<-HairEyeColor
data_test<-margin.table(data, c(1,2)) 


# 2. On crée notre objet "fonction_context" qui est notre test chi2
fonction_context<-chisq_table_context(x=data_test)


# 3. Voici un print
print(fonction_context)


# 4. Voici un summary
summary(fonction_context)


# 5. Voici les différents plot possibles
plot(fonction_context,"mosaique")
plot(fonction_context,"mosaique2")
plot(fonction_context,"heatmap")
plot(fonction_context,"heatmap2")
plot(fonction_context,"barplot")
plot(fonction_context,"balloon")





        ## Démo du KNN



# 1. Création d'un jeu données

set.seed(123)
groupe <- factor(rep(c("A", "B", "C"), each = 50))
moyennes <- c(A = 10, B = 20, C = 30)
var1 <- rnorm(150, mean = moyennes[groupe], sd = 2)
var2 <- 0.8 * var1 + rnorm(150, mean = 0, sd = 1)
df <- data.frame(var1, var2, groupe)


# 2. On crée un objet "testKnn"
testKnn <- knn_context(df,groupe,var1,var2)


# 3. Résultat d'un print
print(testKnn)

# 4. Résultat d'un plot
plot(testKnn)




