#' anova_tukey_tests
#'
#' Esta funciòn permite realizar pruebas ANOVA y Tukey en los datos,
#'toma un conjunto de datos como argumento y  permite especificar qué análisis deseas realizar.
#'
#' @param data Conjunto de datos a utilizar
#'
#'
#' @return una tabla con el analisis de las pruebas ANOVA y/o Tukey en los datos
#' @export
#'
#' @examples
#'  expr <- data a utilizar
#' anova_tukey_tests(expr, 1) Para realizar el Test ANOVA
#' anova_tukey_tests(expr, 2) para realizar el Test Tukey
anova_tukey_tests <- function(data) {
  # Cargamos los paquetes necesarios
  library(carData)
  library(car)
  library(multcomp)

  # Preguntamos al usuario qué análisis desea realizar
  analysis_choice <- readline(prompt = "Ingrese 1 para realizar el Test ANOVA o 2 para el Test Tukey: ")

  if (analysis_choice == "1") {
    # ANOVA
    # Realizamos la prueba ANOVA para cada gen
    for (gen in colnames(data)[-c(1,2)]) {
      formula <- as.formula(paste(gen, "~ dataset"))
      anova_result <- Anova(aov(formula, data = data))
      print(anova_result)
    }
  } else if (analysis_choice == "2") {
    # Prueba Tukey
    data$dataset <- factor(data$dataset)

    # Realizamos la prueba de Tukey para cada gen
    for (gen in colnames(data)[-c(1,2)]) {
      formula <- as.formula(paste(gen, "~ dataset"))
      tukey_result <- glht(aov(formula, data = data), linfct = mcp(dataset = "Tukey"))
      print(summary(tukey_result))
    }
  } else {
    stop("Error: Opción no válida. Ingrese 1 para realizar el Test ANOVA o 2 para el Test Tukey.")
  }
}
