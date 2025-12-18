#' Effectue le test du Chi-2 sur un tableau de contingence et prépare un objet
#' pour l'analyse S3 pour impression (print), résumé (summary) et graphique (plot)
#'
#' @param x Un tableau de contingence (matrice ou 'table').
#' @param ... Des arguments additionnels.
#' @return Les résultats du test de khi-2 de façon très concise
#' @export
#' @author Samira Amani & Samuel Forcier & Taous Sifouane
# #' @examples
# #' print.chisq_table_context(x=mtcars)

print.chisq_table_context <- function(x, ...) {
  cat("La statistique de test du khi-2 est", x$statistic, "avec",x$parametre, "degres de liberte et une p-value de",x$p.value )
  invisible(x)
}
 
