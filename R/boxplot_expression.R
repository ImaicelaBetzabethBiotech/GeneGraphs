boxplot_expression <- function(data, x_var, y_var) {
  library(ggpubr)
  if (!x_var %in% colnames(data)) {
    stop(paste("La variable especificada para el eje x (", x_var, ") no se encuentra en el conjunto de datos.", sep = ""))
  }
  if (y_var != "all" & !all(y_var %in% colnames(data))) {
    stop(paste("Una o más de las variables especificadas para el eje y (", paste(y_var, collapse = ", "), ") no se encuentran en el conjunto de datos.", sep = ""))
  }
  if (y_var == "all") {
    y_vars <- colnames(data)[!colnames(data) %in% c(x_var, "bcr_patient_barcode")]
  } else {
    y_vars <- y_var
  }
  ggboxplot(data, x = x_var,
            y = y_vars,
            merge = "flip",
            ylab = "Expression",
            xlab = "Dataset",
            color = x_var) +
    scale_fill_manual(values = c("#A9A9A9", "#808080", "#696969")) +
    theme_grey() +
    theme(legend.position = "top")
}
