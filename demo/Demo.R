
remove.packages("Equipe4.Chi2")
devtools::install("C:\\Users\\samfo\\OneDrive\\Bureau\\PackageR")
library(Equipe4.Chi2)



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


# 5. Voici les différents plot possibles. À noter que le mode LLM est uniquement possible pour le heatmp2.
# Un avertissement sera produit si le type de graphique n'est pas heatmap2. Vous pourrez tester 

plot(fonction_context, type_graphique = "mosaique", mode="llm")
plot(fonction_context, type_graphique = "mosaique", mode="normal")
plot(fonction_context, type_graphique = "mosaique2")

plot(fonction_context, type_graphique = "heatmap")
plot(fonction_context, type_graphique = "heatmap2")
plot(fonction_context, type_graphique = "heatmap2", mode = "llm")
plot(fonction_context, type_graphique = "barplot")
plot(fonction_context, type_graphique = "balloon")

plot(fonction_context, type_graphique = "heatmap", mode="llm")


# Exemple additionnel
data(Titanic)

# Conversion en data.frame long (2201 lignes)
df_titanic <- as.data.frame(Titanic)

# Exemple : Survie × Classe
tab_titanic <- xtabs(Freq ~ Survived + Class, data = df_titanic)

obj_titanic <- chisq_table_context(tab_titanic)

plot(obj_titanic, type_graphique = "mosaique", mode = "normal")
plot(obj_titanic, type_graphique = "heatmap2", mode = "llm")  



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




