#' jiterplot_expression
#'
#' Esta función genera un gráfico de caja con puntos jitter para
#' visualizar la distribución de los valores de las variables del eje
#' y para cada valor de la variable del eje x.
#' El gráfico incluye facetas para cada variable del eje y.
#' si se especifica el valor "all" para el argumento y_vars,
#' la función incluirá todas las columnas del conjunto de datos que
#' no sean la variable del eje x ni la columna "bcr_patient_barcode".
#' La función verifica si las variables especificadas para los ejes x
#' e y se encuentran en el conjunto de datos
#'
#' @param data Conjunto de datos a utilizar para generar el gráfico
#' @param x_var una variable para el eje x (x_var), dataset de la data, para el caso "tipos de cancer"
#' @param y_var una variable para el eje y (y_var), nombre de Gen
#'
#' @return  un boxplot con puntos de disperion, de la expresion de genes en diferentes tipos de cancer
#' @export
#'
#' @examples
#'  expr <- data a utilizar
#' "dataset" <- Tipos de cancer
#' XBP1 = Gen
#' jiterplot_expression(expr, "dataset", "XBP1") Genera un gráfico de caja para un solo gen
#' jiterplot_expression(expr, "dataset", "all") Genera un gráfico de caja para todos los genes disponibles en el conjunto de datos

jiterplot_expression <- function(data, x_var, y_vars) {
  library(ggpubr)
  library(reshape2)
  if (!x_var %in% colnames(data)) {
    stop(paste("La variable especificada para el eje x (", x_var, ") no se encuentra en el conjunto de datos.", sep = ""))
  }
  if (y_vars != "all" & !all(y_vars %in% colnames(data))) {
    stop(paste("Una o más de las variables especificadas para el eje y (", paste(y_vars, collapse = ", "), ") no se encuentran en el conjunto de datos.", sep = ""))
  }
  if (y_vars == "all") {
    y_vars <- colnames(data)[!colnames(data) %in% c(x_var, "bcr_patient_barcode")]
  }
  data_long <- reshape2::melt(data, id.vars = x_var, measure.vars = y_vars)
  ggplot(data_long, aes_string(x = x_var, y = "value", color = x_var)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, size = 0.1) +
    scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
    labs(x = "Cohort type", y = "Expresión", color = "Cohort") +
    theme(legend.position = "bottom") +
    stat_compare_means(comparisons = list(c("BRCA", "OV"), c("OV", "LUSC"))) +
    theme_grey() +
    facet_wrap(~ variable)
}
