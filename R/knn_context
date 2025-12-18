#' Contexte pour une classification k-NN avec LLM
#'
#' Cette fonction entraîne un modèle de classification
#' k plus proches voisins (k-NN) sur deux variables
#' numériques et une variable de classe, puis demande
#' à un LLM (Ollama) d'interpréter le résultat.
#'
#' @param data data.frame contenant la variable de classe et les prédicteurs.
#' @param class_var Nom de la variable de classe (non quoted, ex: Species).
#' @param x1,x2 Noms des deux variables numériques (non quoted).
#' @param k Nombre de voisins k.
#' @param model_ollama Nom du modèle Ollama à utiliser.
#'
#' @return Un objet de classe \code{knn_context}.
#' @export
knn_context <- function(data, class_var, x1, x2,
                        k = 5,
                        model_ollama = "llama3") {

  class_var <- rlang::ensym(class_var)
  x1 <- rlang::ensym(x1)
  x2 <- rlang::ensym(x2)

  class_name <- rlang::as_string(class_var)
  x1_name    <- rlang::as_string(x1)
  x2_name    <- rlang::as_string(x2)

  # Sélection des colonnes + gestion des NA
  df <- data |>
    dplyr::select(
      class = !!class_var,
      x1    = !!x1,
      x2    = !!x2
    ) |>
    dplyr::mutate(class = as.factor(class)) |>
    stats::na.omit()

  # matrice des prédicteurs
  X <- as.matrix(df[, c("x1", "x2")])
  y <- df$class

  # apprentissage + prédiction sur tout le jeu (apparent error rate)
  preds <- class::knn(
    train = X,
    test  = X,
    cl    = y,
    k     = k
  )

  accuracy <- mean(preds == y)

  # prompt pour le LLM
  prompt <- paste(
    "Tu es un(e) statisticien(ne) qui vulgarise pour un public non spécialiste.",
    "On a entraîné un modèle de classification k-plus-proches-voisins (k-NN).",
    sprintf("La variable de classe s'appelle %s et comporte %d niveaux.",
            class_name, length(levels(y))),
    sprintf("Les prédicteurs utilisés sont %s et %s.",
            x1_name, x2_name),
    sprintf("On a obtenu un taux de bonne classification (exactitude apparente) de %.2f%%.",
            100 * accuracy),
    "Explique en quelques phrases ce que fait un modèle k-NN,",
    "comment interpréter ce taux de bonne classification,",
    "et rappelle que ce taux est calculé sur les mêmes données que l'entraînement,",
    "donc qu'il peut être optimiste s'il n'y a pas de validation croisée."
  )

  interpretation <- tryCatch(
    call_ollama(prompt, model = model_ollama),
    error = function(e) paste("Erreur lors de l'appel au LLM :", e$message)
  )

  res <- list(
    data       = df,
    k          = k,
    preds      = preds,
    accuracy   = accuracy,
    class_var  = class_name,
    x1_name    = x1_name,
    x2_name    = x2_name,
    llm_prompt = prompt,
    llm_text   = interpretation
  )

  class(res) <- "knn_context"
  res
}

#' @export
print.knn_context <- function(x, ...) {
  cat("### Modèle k-NN\n")
  cat("Variable de classe :", x$class_var, "\n")
  cat("Prédicteurs        :", x$x1_name, "et", x$x2_name, "\n")
  cat("k                  :", x$k, "\n")
  cat("Exactitude apparente :",
      sprintf("%.2f%%", 100 * x$accuracy), "\n")
  invisible(x)
}

#' @export
plot.knn_context <- function(x, ...) {
  df_plot <- x$data |>
    dplyr::mutate(pred = x$preds)

  g <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = x1, y = x2,
                 color = pred,
                 shape = class)
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(
      title = sprintf("Classification k-NN (k = %d)", x$k),
      subtitle = sprintf("Exactitude apparente : %.2f%%",
                         100 * x$accuracy),
      x = x$x1_name,
      y = x$x2_name,
      color = "Classe prédite",
      shape = "Classe réelle"
    ) +
    ggplot2::theme_minimal()

  print(g)

  cat("\n### Interprétation (LLM) :\n")
  cat(x$llm_text, "\n")

  invisible(x)
}
