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
