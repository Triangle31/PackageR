
## Télécharger Ollama (version gemma3:4b)

https://ollama.com/

Ouvrir l'app et télécharger la version gemma3:4b. Important d'avoir cette version.


## Installation du package

devtools::install("chemin vers Equipe4.Chi2")


## Utilisation du package

library(Equipe4.Chi2)

## Documentation de chisq_table_context()

chisq_table_context(data)

data = un tableau de contingence (matrice ou table)

retourne un objet de classe "chisq_table_context", qui est une liste contenant les résultats du test et les éléments contextuels

  ### méthodes S3 de chisq_table_context()
print.chisq_table_context() retourne la statistique du test, la p-value et le degré de liberté

summary.chisq_table_context() retourne les résultats du test de façon un peu plus détaillée, accompagnée d'une interprétation contextuelle du LLM Ollama

plot.chisq_table_context(type de graphique) retourne un graphique

Les type de graphiques possibles sont : "heatmap", "heatmap2", "mosaique", "mosaique2", "barplot", "balloon"
  

## Documentation de knn_context()

knn_context <- function(data, class_var, x1, x2, k = 5)

data = data.frame contenant la variable de classe et les prédicteurs

class_var = nom de la variable de classe

x1, x2 = noms des deux variables numériques

k = nombre de voisins

retourne un objet de classe "knn_context"

  ### méthodes S3 de knn_context()

print.knn_context() retourne les paramètres du test et l'exactitude apparente

plot.knn_context() retourne un graphique des points distingués selon leur classe réelle et leur classe prédite par l’algorithme k-NN.




