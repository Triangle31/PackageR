#' Visualisation des résultats du test du Chi-deux (objet chisq_table_context)
#'
#' Génère un graphique à partir d'un objet de classe \code{"chisq_table_context"}.
#' En mode \code{"normal"}, le type de graphique est choisi via \code{type_graphique}.
#' En mode \code{"llm"}, le graphique est automatiquement une heatmap des résidus standardisés
#' avec annotations (titres/sous-titres/légendes) générées via Ollama si disponible
#' (avec repli sur des annotations déterministes sinon).
#'
#' @param x Objet de classe \code{"chisq_table_context"} (résultat de \code{chisq_table_context()}).
#' @param type_graphique Type de graphique en mode \code{"normal"} :
#'   \code{"heatmap"}, \code{"heatmap2"}, \code{"mosaique"}, \code{"mosaique2"},
#'   \code{"barplot"} ou \code{"balloon"}.
#' @param mode Mode d'exécution : \code{"normal"} ou \code{"llm"}.
#' @param modele Modèle Ollama (utilisé si \code{mode = "llm"}).
#' @param temperature Température (0-1) pour Ollama (utilisé si \code{mode = "llm"}).
#' @param digits Nombre de décimales pour l'affichage des valeurs dans la heatmap (mode llm).
#' @param show_labels Si \code{TRUE}, affiche les valeurs dans les cases (mode llm).
#' @param ... Arguments additionnels (passés à certains graphiques base R / vcd si applicable).
#' @author Samira Amani & Samuel Forcier & Taous Sifouane
#'
#' @return Retourne \code{invisible(x)} après avoir affiché le graphique.
#' @export
#'
#' @examples
#' tab <- table(mtcars$cyl, mtcars$gear)
#' obj <- chisq_table_context(tab)
#' plot(obj, type_graphique = "mosaique", mode = "normal")
#' \dontrun{
#' plot(obj, mode = "llm")
#' }
#plot.chisq_table_context <- function(x, ...) { }

plot.chisq_table_context <- function(x,
                                     type_graphique = c("heatmap", "heatmap2",
                                                        "mosaique", "mosaique2",
                                                        "barplot", "balloon"),
                                     mode = c("normal", "llm"),
                                     modele = "mistral",
                                     temperature = 0.2,
                                     digits = 3,
                                     show_labels = TRUE,
                                     ...) {
  
  mode <- match.arg(mode)
  type_graphique <- match.arg(type_graphique)
  
  #Avertissement si mode LLM + graphique non-heatmap
  if (mode == "llm" && type_graphique != "heatmap2") {
    warning(
      paste0(
        "mode = 'llm' produit uniquement une heatmap des résidus standardisés.\n",
        "Le type_graphique = '", type_graphique,
        "' a été ignoré et remplacé par 'heatmap2'."
      ),
      call. = FALSE
    )
    type_graphique <- "heatmap2"
  }
  if (!inherits(x, "chisq_table_context")) {
    stop("x doit être un objet de classe 'chisq_table_context'.")
  }
  
  #MODE NORMAL
  if (mode == "normal") {
    return(.ctx_chi2_build_normal(x, type_graphique = type_graphique, ...))
  }
  
  #MODE LLM : heatmap fixe des résidus standardisés + annotations
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Le package 'ggplot2' est requis (Imports recommandé).")
  }
  
  # Données long format
  df_res <- .ctx_chi2_residuals_long(x, standardized = TRUE, digits = digits)
  
  # Annotations : fallback auto si LLM échoue
  ann_auto <- .ctx_chi2_build_mode(x, digits = digits)
  ann_auto$fill_lab <- "Résidus standardisés"
  
  lab_llm <- ctx_llm_chi2_labels(x, modele = modele, temperature = temperature)
  ann <- if (is.null(lab_llm)) ann_auto else lab_llm
  
  # Graphique
  p <- ggplot2::ggplot(df_res, ggplot2::aes(x = varX, y = varY, fill = resid)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::theme_minimal()
  
  if (isTRUE(show_labels)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = label), size = 3)
  }
  
  p <- p + ggplot2::labs(
    title = ann$title,
    subtitle = ann$subtitle,
    x = ann$xlab,
    y = ann$ylab,
    fill = ann$fill_lab,
    caption = ann$caption
  )
  
  print(p)
  invisible(x)
}

#Pour le mode sans llm
.ctx_chi2_build_normal <- function(x,
                                   type_graphique = c("heatmap", "heatmap2",
                                                      "mosaique", "mosaique2",
                                                      "barplot", "balloon"),
                                   ...) {
  
  #Convertir les données originales en data.frame
  df_og <- as.data.frame(x$data_og)
  
  #Convertir les résidus en data.frame et renommer les colonnes
  df_res <- as.data.frame(x$residus)
  names(df_res) <- c("varX", "varY", "varF")
  
  #Pour pouvoir afficher les noms des colonnes dans les graphiques
  varx <- names(df_res)[1]
  vary <- names(df_res)[2]
  varf <- names(df_res)[3]
  type_graphique <- match.arg(type_graphique)
  
  #Plot 1: heatmap des fréquences
  if (type_graphique == "heatmap") {
    graph <- ggplot2::ggplot(df_og, ggplot2::aes(df_og[,1], df_og[,2], fill = Freq)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = Freq )) +
      ggplot2::labs(x = names(df_og)[1],
                    y = names(df_og)[2]) +
      ggplot2::scale_fill_gradient2(
        mid    = "white",
        high   = "salmon",
        midpoint = 0
      )+
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Heatmap en fonction de la frequence")
    
    print(graph)
    return(invisible(x))
  }
  
  #Plot 2: Heatmap des résidus
  if (type_graphique == "heatmap2") {
    graph <- ggplot2::ggplot(df_res, ggplot2::aes(varX, varY, fill = varF)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = round(varF, 2)), size = 5) +
      ggplot2::scale_fill_gradient2(
        mid    = "white",
        high   = "salmon",
        midpoint = 0
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Heatmap en fonction des residus standardises du test du khi-2")
    
    print(graph)
    return(invisible(x))
  }
  
  #Plot 3: Mosaique
  if (type_graphique == "mosaique") {
    graphics::mosaicplot(x$data_og,
                         main = paste("Mosaique pour", x$data_name),
                         col = c("lightblue", "salmon"))
  }
  
  #Plot 4: Mosaique avec les residus
  if (type_graphique == "mosaique2") {
    vcd::mosaic(x$data_og,
                main = "Mosaique en fonction des residus standardises du test du khi-2",
                shade = TRUE)
  }
  
  #Plot 5: Balloon plot
  if (type_graphique == "balloon" ) {
    graph <- ggplot2::ggplot(df_res, ggplot2::aes(varX, varY)) +
      ggplot2::geom_point(ggplot2::aes(size = df_res[,3]), color = "steelblue") +
      ggplot2::scale_size(range = c(3,12)) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Balloon plot des frequences observees",
                    x = varx,
                    y = vary,
                    size  = "Size")
    
    print(graph)
    return(invisible(x))
  }
  
  #Plot 6: Bar plot empile
  if (type_graphique == "barplot" ) {
    graph <- ggplot2::ggplot(df_res, ggplot2::aes(x = df_res[,1], y = df_res[,3], fill = varF)) +
      ggplot2::geom_col(position = "fill") +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Barplot de la distribution relative des effetifs",
                    x = varx,
                    y = vary
      )+
      ggplot2::scale_fill_gradient2(
        midpoint = 0,
        mid = "#EEE5DE",
        high = "salmon"
      )
    
    print(graph)
    return(invisible(x))
  }
  
  invisible(x)
}


#Pour le mode LLM
.ctx_chi2_build_mode <- function(x, digits = 3) {
  tab <- x$data_og
  dims <- dim(tab)
  n <- sum(tab)
  
  stat <- as.numeric(x$statistic)
  ddl  <- as.numeric(x$parametre)
  pval <- as.numeric(x[["p.value"]])
  
  varx <- if (!is.null(colnames(tab))) "Colonnes" else "Variable X"
  vary <- if (!is.null(rownames(tab))) "Lignes" else "Variable Y"
  
  p_txt <- if (is.na(pval)) {
    "p = NA"
  } else if (pval < 0.001) {
    "p < 0.001"
  } else {
    paste0("p = ", format(round(pval, digits), nsmall = digits))
  }
  
  title <- paste0("Test du Chi-deux d’indépendance (", x$data_name, ")")
  subtitle <- paste0(
    "Tableau ", dims[1], "×", dims[2],
    " | n = ", n,
    " | X² = ", round(stat, digits),
    " | ddl = ", ddl,
    " | ", p_txt
  )
  
  list(
    title = title,
    subtitle = subtitle,
    caption = "Résidus standardisés : > 0 sur-représentation ; < 0 sous-représentation.",
    xlab = varx,
    ylab = vary,
    fill_lab = "Résidus"
  )
}


# Résidus standardisés en format long (ggplot)

.ctx_chi2_residuals_long <- function(x, standardized = TRUE, digits = 3) {
  tab <- x$data_og
  expected <- x$expected
  
  if (standardized) {
    resid <- (tab - expected) / sqrt(expected)
  } else {
    resid <- x$residus
  }
  
  df <- as.data.frame(as.table(resid))
  names(df) <- c("varY", "varX", "resid")
  
  df$label <- format(round(df$resid, digits), nsmall = digits)
  
  if (!is.null(colnames(tab))) df$varX <- factor(df$varX, levels = colnames(tab))
  if (!is.null(rownames(tab))) df$varY <- factor(df$varY, levels = rev(rownames(tab)))
  
  df
}

#' @keywords internal
ctx_llm_chi2_labels <- function(x,
                                modele = "mistral",
                                temperature = 0.2) {
  
  if (!inherits(x, "chisq_table_context")) {
    stop("x doit être un objet 'chisq_table_context'.")
  }
  
  # garde-fou : si ollama pas installé => NULL
  if (!nzchar(Sys.which("ollama"))) return(NULL)
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  
  tab <- x$data_og
  dims <- dim(tab)
  n <- sum(tab)
  
  stat <- as.numeric(x$statistic)
  ddl  <- as.numeric(x$parametre)
  pval <- as.numeric(x[["p.value"]])
  
  contexte <- paste0(
    "Nom des données: ", x$data_name, "\n",
    "Dimensions: ", dims[1], "x", dims[2], "\n",
    "Effectif total n=", n, "\n",
    "X2=", signif(stat, 5), " ; ddl=", ddl, " ; p=", signif(pval, 5), "\n",
    "Modalités lignes: ", paste(rownames(tab), collapse = ", "), "\n",
    "Modalités colonnes: ", paste(colnames(tab), collapse = ", "), "\n"
  )
  
  prompt <- paste0(
    "Génère des annotations COURTES et FACTUELLES pour une heatmap de résidus standardisés d'un test du Chi-deux.\n",
    "Réponds en JSON STRICT, avec exactement ces clés:\n",
    "title, subtitle, caption, xlab, ylab, fill_lab\n",
    "Contraintes:\n",
    "- Français, neutre.\n",
    "- Aucune interprétation causale.\n",
    "- title <= 90 caractères, subtitle <= 170, caption <= 220.\n\n",
    "Contexte:\n", contexte
  )
  
  out <- tryCatch({
    system2(
      command = "ollama",
      args = c("run", modele),
      input = prompt,
      stdout = TRUE,
      stderr = TRUE
    )
  }, error = function(e) NULL)
  
  if (is.null(out) || length(out) == 0) return(NULL)
  
  txt <- paste(out, collapse = "\n")
  
  lab <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
  if (is.null(lab)) return(NULL)
  
  needed <- c("title","subtitle","caption","xlab","ylab","fill_lab")
  if (!all(needed %in% names(lab))) return(NULL)
  
  for (k in needed) {
    if (!is.character(lab[[k]]) || length(lab[[k]]) != 1 || nchar(lab[[k]]) == 0) {
      return(NULL)
    }
  }
  
  lab
}
