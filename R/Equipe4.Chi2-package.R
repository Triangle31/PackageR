#' @keywords internal
"_PACKAGE"

#' Couleur des yeux et des cheveux
#'
#' Le jeu de données est la distribution conjointe entre la couleur des cheveux
#'  et celle des yeux d'un groupe d'étudiants en statistique
#'  de la forme \code{Hair × Eye × Sex}.
#'
#' * une couleur de cheveux (\code{Hair}),
#' * une couleur d’yeux (\code{Eye}),
#' * le nombre d’individus observés dans cette combinaison (\code{Freq}).
#'
#' @format Un \code{data.frame} de 592 observations et 3 variables :
#' \describe{
#'   \item{\code{Hair}}{Couleur des cheveux (facteur).
#'     Modalités : \code{"Black"}, \code{"Brown"}, \code{"Red"}, \code{"Blond"}.}
#'
#'   \item{\code{Eye}}{Couleur des yeux (facteur).
#'     Modalités : \code{"Brown"}, \code{"Blue"}, \code{"Hazel"}, \code{"Green"}.}
#'
#'   \item{\code{Freq}}{Effectif (nombre d’individus observés). Entier positif.}
#' }
#'
#' #' @source
#' Données originales : \code{datasets::HairEyeColor} de la librarie GLMData
#'
#' @examples
#' data(HairEyeColor)
#'
#' # Table de contingence Hair × Eye
#' table_contingence <- xtabs(Freq ~ Hair + Eye, data = HairEyeColor)
#'
#' # Application du test du Chi-deux via ce package
#' donnees <- chisq_table_context(table_contingence)
#' summary(donnees)
#' plot(donnees, type_graphique = "heatmap")
"HairEyeColor"

