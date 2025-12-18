#' Test du Chi-2 et analyse contextuelle
#'
#' Effectue le test du Chi-2 sur un tableau de contingence et prépare un objet
#' pour l'analyse S3 pour impression (print), résumé (summary) et graphique (plot)
#'
#' @param x Un tableau de contingence (matrice ou 'table').
#' @return Un objet de classe "chisq_table_context", qui est une liste contenant les résultats du test et les éléments contextuels
#' @export
#' @author Samira Amani & Samuel Forcier & Taous Sifouane

chisq_table_context <- function(x) {
  set.seed(222)
  # Verification de base pour s'assurer que 'x' est une table
  if (!(is.matrix(x) ||is.data.frame(x))) {
    stop("L'argument 'x' doit etre une matrice ou une table de contingence.")
  }

  #1. Effectuer le test de Chi-2 standard
  test_resultat <- stats::chisq.test(x)

  #2. Construire l'objet resultat
  resultat <- list(
    method = "Test du Chi-2",
    data_og = test_resultat$observed,
    data_name = deparse(substitute(x)),
    statistic = test_resultat$statistic,
    parametre = test_resultat$parameter,
    p.value = test_resultat$p.value,

    #Elements necessaires pour les methodes summary et plot
    expected = test_resultat$expected,
    residus = test_resultat$residuals
  )

  #4. ASSIGNER LA CLASSE S3
  class(resultat) <- "chisq_table_context"
  return(resultat)
}
 
