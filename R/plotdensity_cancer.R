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
