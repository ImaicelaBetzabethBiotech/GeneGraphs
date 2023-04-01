#' plot_pca
#'
#'Esta función toma un conjunto de datos con
#'variables cuantitativas y genera un gráfico PCA de dos dimensiones.
#'
#' @param data Conjunto de datos a utilizar para generar el gráfico
#'
#' @return El gráfico PCA de dos dimensiones.
#' @export
#'
#' @examples
#' expr <- data a utilizar
#' plot_pca(expr)
#'
plot_pca <- function(data) {
  library(FactoMineR)
  library(factoextra)
  library(dplyr)

  # Seleccionamos solo las variables cuantitativas
  expr_num <- select_if(data, is.numeric)

  if (ncol(expr_num) == 0) {
    stop("Error: Los datos no contienen ninguna variable cuantitativa.")
  }

  # Realizamos el análisis de componentes principales
  res.pca <- PCA(expr_num, graph = FALSE)

  # Grafico de individuos
  fviz_pca_ind(res.pca, geom.ind = "point", pointshape = 21,
               pointsize = 2,
               fill.ind = data$dataset,
               col.ind = "black",
               palette = "jco",
               addEllipses = TRUE,
               label = "var",
               col.var = "black",
               repel = TRUE,
               legend.title = "Cohort") +
    ggtitle("2D PCA-plot from expr cohort") +
    theme(plot.title = element_text(hjust = 0.5))+
    theme_grey()
}
