#' plotdensity_cancer
#'
#'Esta función genera un gráfico de densidad para visualizar la distribución de los valores de expresión génica para un tipo de cáncer específico
#'o para todos los tipos de cáncer disponibles en el conjunto de datos.
#' @param data un objeto data.frame que contiene la expresión data
#' @param dataset_col una cadena que indica el nombre de la columna que contiene la información del conjunto de datos
#' @param cancer_type una cadena que indica el tipo de cáncer a trazar o “all” para trazar todos los tipos de cáncer
#'
#' @return un gráfico de densidad para los datos de expresión del tipo de cáncer especificado
#' @export
#'
#' @examples
#'
#' expr <- data a utilizar
#' "dataset" <- Tipos de cancer
#' XBP1 = Gen
#' plotdensity_cancer(expr, "dataset",  "BRCA") Genera un gráfico de caja para un solo gen
#' plotdensity_cancer(expr, "dataset", "all") Genera un gráfico de caja para todos los genes disponibles en el conjunto de datos
plotdensity_cancer <- function(data, dataset_col, cancer_type) {
  library(ggplot2)
  library(reshape2)

  if (!dataset_col %in% colnames(data)) {
    stop(paste("La variable especificada para el conjunto de datos (", dataset_col, ") no se encuentra en el conjunto de datos.", sep = ""))
  }

  if (cancer_type != "all" & !cancer_type %in% unique(data[[dataset_col]])) {
    stop(paste("El tipo de cáncer especificado (", cancer_type, ") no se encuentra en el conjunto de datos.", sep = ""))
  }

  if (cancer_type != "all") {
    data <- subset(data, data[[dataset_col]] == cancer_type)
  }

  # Convertir los datos a formato largo
  expr_long <- melt(data, id.vars = dataset_col, measure.vars = c("GATA3", "PTEN", "XBP1", "MUC1", "ESR1"))

  ggplot(expr_long, aes(x = value, color = variable, fill = variable)) +
    geom_density(alpha = 0.5) +
    geom_rug() +
    geom_vline(aes(xintercept = median(value)), linetype = "dashed") +
    scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#C0C0C0", "#FFD700"))+
    scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#C0C0C0", "#FFD700")) +
    labs(x = "Expression", y = "Count", color = "Gene", fill = "Gene") +
    theme_grey()+
    facet_wrap(as.formula(paste("~", dataset_col)))
}
