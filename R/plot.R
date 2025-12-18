#' Effectue le test du Chi-2 sur un tableau de contingence et prépare un objet
#' pour l'analyse S3 pour impression (print), résumé (summary) et graphique (plot)
#'
#' @param x Un tableau de contingence (matrice ou 'table').
#' @param ... Des arguments additionnels.
#' @param type_graphique Le type de graphique à afficher (exemple: Heatmap ou mosaique).
#' @return Les résultats du test de khi-2 et le graphique spécifié en entrée
#' @export
#' @author Samira Amani & Samuel Forcier & Taous Sifouane
# #' @examples
# #' test<-cbind(c(2,3,4,1,3,4),c(1,1,1,1,2,3))
# #' plot.chisq_table_context(x=test, type_graphique="heatmap")

plot.chisq_table_context  <- function(x, type_graphique=c("heatmap", "heatmap2",
                                                          "mosaique", "mosaique2",
                                                          "barplot", "balloon"), ...) {
  #Convertir les données originales en data.frame
  df_og<-as.data.frame(x$data_og)


  #Convertir les résidus en data.frame et renommer les colonnes
  df_res<-as.data.frame(x$residus)
  names(df_res)<-c("varX", "varY", "varF")

  #Pour pouvoir afficher les noms des colonnes dans les graphiques
  varx <- names(df_res)[1]
  vary <- names(df_res)[2]
  varf <- names(df_res)[3]
  type_graphique <- match.arg(type_graphique)

  #Plot 1: heatmap des fréquences
  if (type_graphique == "heatmap") {
    graph<-ggplot2::ggplot(df_og, ggplot2::aes(df_og[,1], df_og[,2], fill = Freq)) +
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
    graph<-ggplot2::ggplot(df_res, ggplot2::aes(varX, varY, fill = varF)) +
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
      graph<-ggplot2::ggplot(df_res, ggplot2::aes(varX, varY)) +
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
      graph<-ggplot2::ggplot(df_res, ggplot2::aes(x = df_res[,1], y = df_res[,3], fill = varF)) +
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
} 
