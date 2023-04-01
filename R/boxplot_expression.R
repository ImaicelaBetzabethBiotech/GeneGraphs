#' boxplot_expression
#'
#' Esta función genera un gráfico de caja para visualizar la distribución de
#' los valores de las variables del eje y para cada valor de la variable del eje x.
#' Si se especifica el valor "all" para el argumento y_var, la función incluirá todas las columnas
#' del conjunto de datos que no sean la variable del eje x ni la columna "bcr_patient_barcode".
#'
#' @param data Conjunto de datos a utilizar para generar el gráfico
#' @param x_var una variable para el eje x (x_var), dataset de la data, para el caso "tipos de cancer"
#' @param y_var una variable para el eje y (y_var), nombre de Gen
#'
#' @return  un boxplot de la expresion de genes en diferentes tipos de cancer
#' @export
#'
#' @examples
#'  expr <- data a utilizar
#' "dataset" <- Tipos de cancer
#' XBP1 = Gen
#' boxplot_expression(expr, "dataset", "XBP1") Genera un gráfico de caja para un solo gen
#' boxplot_expression(expr, "dataset", "all") Genera un gráfico de caja para todos los genes disponibles en el conjunto de datos
#'
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
