#' Effectue le test du Chi-2 sur un tableau de contingence et prépare un objet
#' pour l'analyse S3 pour impression (print), résumé (summary) et graphique (plot)
#'
#' @param object Un tableau de contingence (matrice ou 'table').
#' @param ... Des arguments additionnels.
#' @return Les résultats du test de khi-2 de façon un peu plus détaillée
#' @export
#' @author Samira Amani & Samuel Forcier & Taous Sifouane
# #' @examples
# #' summary.chisq_table_context(x=mtcars)

#On utilise object comme argument car summary est défine comme summary(object,...) contrairement à
#print(x,...) et plot(x,...)
summary.chisq_table_context <- function(object, ...) {
  x<-object
  cat("=== Résultat numérique ===\n")
  cat("Le test du Khi-deux de Pearson donne comme resultat :\n" )
  cat("Statistique de test: ", x$statistic[[1]], "\n" )
  cat("P-value: ", x$p.value[[1]], "\n" )
  cat("ddl :", x$parametre, "\n")

  if (x$p.value[[1]] < 0.5){
    cat("Interpretation du test: Le test du Khi-deux est significatif. On rejette Ho." )
  } else {
    cat("Interpretation du test: Le test du Khi-deux n'est pas significatif. On ne rejette pas Ho. \n\n" )
  }

  # === Partie Ollama ===

  cat("\n\n\n === Interprétation contextuelle (LLM) ===\n\n")

  prompt <- sprintf(
    "Voici les résultats d'un test du Chi-deux. Explique ces résultats de manière brève et claire.\n
    Ne fait pas une phrase d'introduction. \n
Distribution des données en tableau croisé :
%s

La statistique du chi-deux : %.7f
Le parametre du degré de liberté : %d
La p-value : %.15e

Tableau croisé des valeurs expected :
%s

Tableau croisé des résidus :
%s",
    capture.output(print(x$data_og)) |> paste(collapse = "\n"),
    x$statistic[[1]],
    x$parametre,
    x$p.value[[1]],
    capture.output(print(x$expected)) |> paste(collapse = "\n"),
    capture.output(print(x$residus)) |> paste(collapse = "\n")
  )


  # --- Envoi au LLM ---

  #cat("\n === PROMPT ENVOYÉ ===\n")
  #cat(prompt)
  #cat("\n==============================\n")

  llm_output <- ollama_generate(prompt)

  cat(llm_output, "\n")

  invisible(x)
}
