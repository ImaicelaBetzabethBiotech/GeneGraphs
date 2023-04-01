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
