

# Projet2_Equipe4

## Télécharger Ollama

https://ollama.com/

Ouvrir l'app et télécharger la version gemma3:4b en bas à droite

## Pour commencer (dans la console du projet *Equipe4.Chi2*) :

install.packages("devtools")

library(devtools)

devtools::document()

devtools::load_all()


## Après, dans le script R de test :

devtools::install("CHEMIN/vers/Equipe4.Chi2")

library(Equipe4.Chi2)


## Après modifications de notre package:

devtools::load_all() dans le projet

ferme et ouvre le fichier de test

remove.packages("Equipe4.Chi2") dans test

devtools::install("CHEMIN/vers/Equipe4.Chi2") dans test

library(Equipe4.Chi2) dans test


## Le fichier de test est sur teams
