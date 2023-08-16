#' Resumen de características de variables en un conjunto de datos
#'
#' Esta función calcula varias características descriptivas para cada variable en el conjunto de datos.
#'
#' @param data Un dataframe o matriz de datos.
#'
#' @return Un dataframe con el resumen de características de cada variable.
#'
#' @examples
#' data <- data.frame(
#'   Var1 = c(1, 2, 3, NA, 5),
#'   Var2 = c("A", "B", "A", "C", "B"),
#'   Var3 = c(10.2, 9.5, 11.1, 10.8, NA)
#' )
#' result <- resumen(data)
#' print(result)
#'
#' @keywords estadísticas descriptivas variables resumen
#'
#' @export
resumen <- function(data) {
  # Crea un data frame vacío para almacenar las características de las variables
  df <- data.frame(variable = character(),
                   tipo = character(),
                   media = numeric(),
                   mediana = numeric(),
                   desviacion_estandar = numeric(),
                   minimo = numeric(),
                   maximo = numeric(),
                   valores_unicos = numeric(),
                   longitud = integer(),
                   valores_faltantes = integer(),
                   porcentaje_faltantes = numeric(),
                   stringsAsFactors = FALSE)

  # Obtener las características de cada variable del conjunto de datos y agregarlas al data frame
  for(i in 1:ncol(data)){
    var_name <- colnames(data)[i]
    var_type <- class(data[[i]])
    var_mean <- ifelse(var_type == "numeric", round(mean(data[[i]], na.rm = TRUE), 2), "NA")
    var_median <- ifelse(var_type == "numeric", round(median(data[[i]], na.rm = TRUE), 2), "NA")
    var_sd <- ifelse(var_type == "numeric", round(sd(data[[i]], na.rm = TRUE), 2), "NA")
    var_min <- ifelse(var_type == "numeric", round(min(data[[i]], na.rm = TRUE), 2), "NA")
    var_max <- ifelse(var_type == "numeric", round(max(data[[i]], na.rm = TRUE), 2), "NA")
    var_unique <- length(unique(data[[i]]))
    var_length <- length(data[[i]])
    var_missing <- sum(is.na(data[[i]]))
    var_missing_pct <- paste(round(var_missing / var_length * 100, 3), "%", sep = "")

    # Agregar las características de la variable al data frame
    df[i,] <- data.frame(var_name,
                         var_type,
                         var_mean,
                         var_median,
                         var_sd,
                         var_min,
                         var_max,
                         var_unique,
                         var_length,
                         var_missing,
                         var_missing_pct)
  }
  # Devolver el data frame
  return(df)
}

#' Convertir nombres de columnas a mayúsculas
#'
#' Esta función toma un data frame y convierte los nombres de las columnas a letras mayúsculas.
#'
#' @param data Un data frame. El conjunto de datos cuyas columnas se convertirán a mayúsculas.
#' @return Un nuevo data frame con los mismos datos que el original, pero con los nombres de las columnas en mayúsculas.
#'
#' @examples
#' # Supongamos que tenemos el siguiente data frame
#' #   nombre  edad  ciudad
#' #0  Alice    25  Madrid
#' #1    Bob    30   Paris
#' #2  Carol    22  Berlin
#'
#' # Llamamos a la función para convertir los nombres de las columnas a mayúsculas
#' nuevo_df <- convertir_nombres_mayusculas(datos)
#'
#' # El nuevo data frame tendrá los nombres de las columnas en mayúsculas
#' #   NOMBRE  EDAD  CIUDAD
#' #0  Alice    25  Madrid
#' #1    Bob    30   Paris
#' #2  Carol    22  Berlin
#'
#' @export
convertir_nombres_mayusculas <- function(data) {
  nombres_originales <- colnames(data)
  nombres_mayusculas <- toupper(nombres_originales)
  colnames(data) <- nombres_mayusculas
  return(data)
}



#' Convertir factores de una base de datos a cadenas de texto
#'
#' Esta función toma como entrada una base de datos y convierte todas las columnas que
#' sean de tipo factor a cadenas de texto (character). Los factores son un tipo especial
#' de datos en R que representan variables categóricas con un conjunto finito de niveles
#' posibles. La conversión a cadenas de texto facilita su manipulación y análisis posterior,
#' ya que muchas operaciones y gráficas en R trabajan mejor con datos en formato de caracteres.
#'
#' @param data Un objeto de tipo data.frame o tibble que contiene los datos a procesar.
#'
#' @return Un nuevo objeto de tipo data.frame o tibble con las mismas dimensiones que el
#'   objeto de entrada, pero con las columnas que eran factores convertidas a cadenas de texto.
#'   El objeto devuelto tendrá las mismas columnas y nombres que el objeto de entrada.
#'
#' @examples
#' data_ejemplo <- data.frame(
#'   edad = c(25, 30, 22),
#'   genero = factor(c("masculino", "femenino", "masculino")),
#'   ciudad = factor(c("New York", "Los Angeles", "Chicago"))
#' )
#'
#' data_ejemplo_convertido <- factores_a_string(data_ejemplo)
#' str(data_ejemplo_convertido)
#'
#' @importFrom methods is
#' @export
#'
factores_a_string <- function(data) {
  columnas <- names(data)

  for (col in columnas) {
    if (is.factor(data[[col]])) {
      data[[col]] <- as.character(data[[col]])
    }
  }

  return(data)
}

# Función para convertir fechas de una base de datos a cadenas de texto
fechas_a_string <- function(data, formato = "%Y-%m-%d") {
  # Obtener los nombres de las columnas
  columnas <- names(data)

  # Iterar sobre cada columna
  for (col in columnas) {
    if (is.Date(data[[col]])) {
      data[[col]] <- format(data[[col]], formato)
    }
  }


  return(data)
}


#' Convierte columnas de un data frame en factores
#'
#' Esta función toma un data frame y convierte las columnas especificadas en factores.
#'
#' @param data_frame Un data frame.
#' @param nombres_columnas Una lista de nombres de columnas a convertir en factores.
#' @return Un data frame con las columnas convertidas en factores.
#' @examples
#' datos <- data.frame(
#'   Nombre = c("Juan", "María", "Pedro", "Ana"),
#'   Ciudad = c("Madrid", "Barcelona", "Valencia", "Sevilla"),
#'   Genero = c("Masculino", "Femenino", "Masculino", "Femenino"),
#'   Edad = c(25, 30, 28, 22)
#' )
#'
#' columnas_a_convertir <- c("Nombre", "Ciudad", "Genero")
#' nuevos_datos <- convertir_columnas_a_factores(datos, columnas_a_convertir)
#' @export
convertir_columnas_a_factores <- function(data_frame, nombres_columnas) {
  if (!is.data.frame(data_frame)) {
    stop("El primer argumento debe ser un data frame")
  }

  for (nombre_col in nombres_columnas) {
    if (!is.character(data_frame[[nombre_col]])) {
      stop(paste("La columna", nombre_col, "no es de tipo cadena (character)"))
    }

    data_frame[[nombre_col]] <- factor(data_frame[[nombre_col]])
  }

  return(data_frame)
}


#' Dicotomiza una variable numérica en función de un umbral
#'
#' Esta función toma una variable numérica y un umbral y devuelve una variable
#' dicotómica donde los valores por encima del umbral son 1 y los valores por debajo
#' del umbral son 0.
#'
#' @param variable Un vector numérico a dicotomizar.
#' @param umbral El umbral para la dicotomización.
#' @return Un vector dicotómico.
#' @examples
#' edades <- c(20, 35, 45, 28, 19, 60)
#' umbral_edad <- 30
#' edades_dicotomizadas <- dicotomizar_variable_num(edades, umbral_edad)
#' @export
dicotomizar_variable_num <- function(variable, umbral) {
  if (!is.numeric(variable)) {
    stop("La variable debe ser numérica")
  }

  dicotomizada <- ifelse(variable >= umbral, 1, 0)
  return(dicotomizada)
}

#' Dicotomizar Variables Categóricas
#'
#' Esta función permite dicotomizar variables categóricas (cadenas o factores) en un data frame.
#'
#' @param data Un data frame en el que se desea dicotomizar las variables.
#' @param columnas Un vector de nombres de columnas o factores que se desean dicotomizar.
#'
#' @return Un data frame con nuevas columnas dicotomizadas para cada valor único en las columnas especificadas.
#'
#' @details La función verifica los argumentos proporcionados y realiza la dicotomización de las variables categóricas. Crea nuevas columnas con valores binarios para cada valor único en las columnas factores.
#'
#' @examples
#' # Crear un data frame de ejemplo
#' data <- data.frame(
#'   Genero = c("Masculino", "Femenino", "Masculino", "Femenino"),
#'   Ciudad = c("A", "B", "A", "C")
#' )
#'
#' # Dicotomizar las variables "Genero" y "Ciudad"
#' columnas_a_dicotomizar <- c("Genero", "Ciudad")
#' nuevo_data <- dicotomizar_variables_cat(data, columnas_a_dicotomizar)
#'
#' @importFrom base is.data.frame is.character is.factor
#' @importFrom base lapply paste
#' @importFrom base levels as.integer
#' @importFrom base colnames
#' @importFrom base missing
#' @importFrom base stop
#'
#' @export
dicotomizar_variables_cat <- function(data, columnas) {
  if (missing(data) || missing(columnas)) {
    stop("Todos los argumentos deben ser especificados: data y columnas")
  }

  if (!is.data.frame(data)) {
    stop("El primer argumento debe ser un data frame")
  }

  if (!is.character(columnas) && !is.factor(columnas)) {
    stop("El segundo argumento debe ser un vector de nombres de columnas o factores")
  }

  # Convertir columnas a factores si son cadenas
  if (is.character(columnas)) {
    data[columnas] <- lapply(data[columnas], factor)
  }

  # Dicotomizar columnas factores
  for (col in colnames(data)) {
    if (col %in% columnas) {
      unique_vals <- levels(data[[col]])

      for (val in unique_vals) {
        new_col_name <- paste(col, val, sep = "_")
        data[[new_col_name]] <- as.integer(data[[col]] == val)
      }
    }
  }

  return(data)
}


#' Estandarizar variables numéricas en una base de datos
#'
#' Esta función toma como entrada una base de datos y estandariza las variables numéricas
#' presentes en ella. La estandarización consiste en transformar cada variable numérica
#' para que tenga media cero y desviación estándar igual a uno. Esta transformación es útil
#' cuando se desean comparar variables que tienen diferentes escalas, ya que coloca todas
#' las variables en una misma escala relativa.
#'
#' @param data Un objeto de tipo data.frame o tibble que contiene los datos a procesar.
#'
#' @return Un nuevo objeto de tipo data.frame o tibble con las mismas dimensiones que el
#'   objeto de entrada, pero con las variables numéricas estandarizadas. Las columnas numéricas
#'   originales son reemplazadas por las versiones estandarizadas y renombradas agregando el
#'   sufijo "_estandarizada" al nombre original.
#'
#' @examples
#' data_ejemplo <- data.frame(
#'   edad = c(25, 30, 22),
#'   ingreso = c(50000, 60000, 48000),
#'   gasto = c(2000, 2500, 1800)
#' )
#'
#' data_ejemplo_estandarizado <- estandarizar_variables(data_ejemplo)
#' str(data_ejemplo_estandarizado)
#'
#' @importFrom stats scale
#' @export
#'
estandarizar_variables <- function(data) {
  columnas_numericas <- sapply(data, is.numeric)
  data_numericas <- data[, columnas_numericas]

  data_estandarizada <- scale(data_numericas)

  nombres_columnas <- colnames(data_numericas)
  nuevos_nombres <- paste0(nombres_columnas, "_estandarizada")
  colnames(data_estandarizada) <- nuevos_nombres

  data[, columnas_numericas] <- data_estandarizada

  return(data)
}


#' Detectar Valores Atípicos en Variables Numéricas
#'
#' Esta función permite detectar valores atípicos en variables numéricas de un conjunto de datos.
#'
#' @param data Un data frame que contiene las variables numéricas a analizar.
#' @return Un data frame que muestra las variables, los índices de las filas con valores atípicos y los valores atípicos detectados.
#' @details La función calcula los límites usando el rango intercuartílico (IQR) y detecta valores atípicos que caen fuera de estos límites.
#'          También genera un boxplot para cada variable numérica con los valores atípicos resaltados en rojo.
#'
#' @examples
#' data_ejemplo <- data.frame(
#'   edad = c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70),
#'   ingreso = c(500, 1000, 1200, 1500, 2000, 2200, 2300, 2500, 2800, 3000)
#' )
#' detectar_valores_atipicos(data_ejemplo)
#'
#' @import stats
#' @export
detectar_valores_atipicos <- function(data) {
  valores_atipicos <- data.frame(Variable = character(),
                                 Indice = integer(),
                                 Valor = numeric(),
                                 stringsAsFactors = FALSE)

  for (columna in colnames(data)) {
    if (is.numeric(data[[columna]])) {
      # Calcular los límites para detectar valores atípicos
      Q1 <- quantile(data[[columna]], 0.25)
      Q3 <- quantile(data[[columna]], 0.75)
      IQR <- Q3 - Q1

      # Encontrar los índices de filas con valores atípicos
      indices_atipicos <- which(data[[columna]] < (Q1 - 1.5 * IQR) | data[[columna]] > (Q3 + 1.5 * IQR))

      # Almacenar los valores atípicos en el dataframe
      valores_atipicos <- rbind(valores_atipicos, data.frame(Variable = rep(columna, length(indices_atipicos)),
                                                             Indice = indices_atipicos,
                                                             Valor = data[[columna]][indices_atipicos],
                                                             stringsAsFactors = FALSE))

      # Generar el boxplot con los valores atípicos resaltados
      boxplot(data[[columna]], main = columna)
      points(indices_atipicos, data[[columna]][indices_atipicos], col = "red", pch = 16)
    }
  }

  return(valores_atipicos)
}

#' Prueba de normalidad  de Shapiro-Wilk
#'
#' Performs the Shapiro-Wilk test for normality on a numeric vector.
#'
#' @param data A numeric vector.
#' @param alpha The significance level for the test (default is 0.05).
#' @return A list with the test statistic, p-value, and normality assessment.
#' @author [OSCAR GONZALEZ FRUTOS]
#' @seealso \code{\link{shapiro.test}}
#'
#' @examples
#' data <- c(1.2, 2.5, 3.1, 4.3, 5.6, 6.8, 7.2, 8.4, 9.7, 10.9)
#' shapiro_wilk_result <- test_shapiro_wilk(data)
#' print(shapiro_wilk_result)
#'
#' @importFrom stats shapiro.test
#' @export
test_shapiro_wilk <- function(data, alpha = 0.05) {
  # Perform Shapiro-Wilk test
  result <- shapiro.test(data)

  # Create a summary of the test results
  summary <- list(
    statistic = result$statistic,
    p_value = result$p.value,
    normality = ifelse(result$p.value > alpha, "Es Normal", "No es Normal")
  )
  # Return the summary
  return(summary)
}

#' Prueba de Shapiro-Wilk para variables numéricas
#'
#' Esta función aplica la prueba de Shapiro-Wilk a todas las variables numéricas
#' en un conjunto de datos y genera un resumen de los resultados de la prueba
#' para cada variable.
#'
#' @param data Un conjunto de datos en formato de marco de datos.
#' @param alpha Nivel de significancia para determinar normalidad.
#' @return Un marco de datos con los resultados de la prueba de Shapiro-Wilk para
#'   cada variable numérica. Incluye la estadística de la prueba, el valor p y una
#'   indicación de si los datos son considerados normales o no.
#' @examples
#' data <- data.frame(
#'   var1 = c(1.2, 3.4, 2.1, 4.5, 5.6),
#'   var2 = c(2.3, 4.5, 1.9, 3.8, 6.7)
#' )
#' result <- apply_shapiro_wilk_all(data)
#' print(result)
#' @export
shapiro_wilk_all <- function(data, alpha = 0.05) {
  # Filtrar las columnas numéricas
  numeric_columns <- sapply(data, is.numeric)

  # Crear una lista de resúmenes para cada columna numérica
  results <- lapply(data[, numeric_columns], test_shapiro_wilk, alpha = alpha)

  # Combinar los resultados en un data frame
  results_df <- data.frame(
    variable = names(results),
    statistic = sapply(results, function(x) round(x$statistic, 3)),
    p_value = sapply(results, function(x) round(x$p_value, 3)),
    normality = sapply(results, function(x) x$normality)
  )

  return(results_df)
}



#' Prueba de normalidad  de Lilliefors
#'
#' Performs the Lilliefors test (Kolmogorov-Smirnov test for normality) on a numeric vector.
#'
#' @param data A numeric vector.
#' @param alpha The significance level for the test (default is 0.05).
#' @return A list with the test statistic, p-value, and normality assessment.
#' @author [OSCAR GONZALEZ FRUTOS]
#' @seealso \code{\link{nortest::lillie.test}}
#'
#' @examples
#' data <- c(1.2, 2.5, 3.1, 4.3, 5.6, 6.8, 7.2, 8.4, 9.7, 10.9)
#' lilliefors_result <- test_lilliefors(data)
#' print(lilliefors_result)
#'
#' @importFrom nortest lillie.test
#' @export

test_lilliefors <- function(data, alpha = 0.05) {
  # Load necessary package
  if (!requireNamespace("nortest", quietly = TRUE)) {
    install.packages("nortest")
  }
  library(nortest)

  # Perform Lilliefors test
  result <- lillie.test(data)

  # Create a summary of the test results
  summary <- list(
    statistic = result$statistic,
    p_value = result$p.value,
    normality = ifelse(result$p.value > alpha,
                       "Es Normal",
                       "No es Normal")
  )
  return(summary)
}

#' Prueba de Lilliefors para todas las variables numéricas
#'
#' Esta función aplica la prueba de Lilliefors a todas las variables numéricas
#' en un conjunto de datos y genera un resumen de los resultados de la prueba
#' para cada variable.
#'
#' @param data Un conjunto de datos en formato de marco de datos.
#' @param alpha Nivel de significancia para determinar normalidad.
#' @return Un marco de datos con los resultados de la prueba de Lilliefors para
#'   cada variable numérica. Incluye la estadística de la prueba, el valor p y una
#'   indicación de si los datos son considerados normales o no.
#' @examples
#' data <- data.frame(
#'   var1 = c(1.2, 3.4, 2.1, 4.5, 5.6),
#'   var2 = c(2.3, 4.5, 1.9, 3.8, 6.7)
#' )
#' result <- test_lilliefors_all(data)
#' print(result)
#' @importFrom nortest lillie.test
#' @export
test_lilliefors_all <- function(data, alpha = 0.05) {
  if (!requireNamespace("nortest", quietly = TRUE)) {
    install.packages("nortest")
  }

  library(nortest)

  results <- list()

  for (column in colnames(data)) {
    if (is.numeric(data[[column]])) {
      # Realiza la prueba de Lilliefors
      result <- lillie.test(data[[column]])

      # Determina la normalidad
      normality <- ifelse(result$p.value > alpha, "Es Normal", "No es Normal")

      # Almacena los resultados
      results[[column]] <- data.frame(Estadistica = round(result$statistic, 3),
                                      Valor_p = round(result$p.value, 3),
                                      Normalidad = normality,
                                      stringsAsFactors = FALSE)
    }
  }

  results_df <- do.call(rbind, results)

  return(results_df)
}

#' Detectar valores ausentes en un conjunto de datos
#'
#' Esta función detecta y reporta la cantidad y el porcentaje de valores ausentes en cada
#' variable de un conjunto de datos.
#'
#' @param data Un data frame o matriz que contiene los datos a analizar.
#'
#' @return Un data frame con las siguientes columnas:
#' \describe{
#'   \item{Variable}{Nombre de la variable en la que se detectaron valores ausentes.}
#'   \item{Cantidad_Ausentes}{Cantidad total de valores ausentes en la variable.}
#'   \item{Porcentaje_Ausentes}{Porcentaje de valores ausentes en la variable (redondeado a 2 decimales).}
#' }
#'
#' @examples
#' # Crear un data frame de ejemplo
#' data <- data.frame(
#'   Col1 = c(1, NA, 3, 4),
#'   Col2 = c(NA, 2, 3, NA),
#'   Col3 = c(NA, NA, NA, NA)
#' )
#'
#' # Aplicar la función para detectar valores ausentes
#' resultados <- detectar_valores_ausentes(data)
#'
#' @importFrom base is.na
#' @importFrom base sum
#'
#' @export
detectar_valores_ausentes <- function(data) {
  valores_ausentes <- data.frame(Variable = character(),
                                 Cantidad_Ausentes = integer(),
                                 Porcentaje_Ausentes = character(),
                                 stringsAsFactors = FALSE)

  for (columna in colnames(data)) {
    cantidad_ausentes <- sum(is.na(data[[columna]]) | data[[columna]] == "")
    total_registros <- length(data[[columna]])
    porcentaje_ausentes <- paste(round(cantidad_ausentes / total_registros * 100, 2), "%", sep = "")

    valores_atributos <- data.frame(Variable = columna,
                                    Cantidad_Ausentes = cantidad_ausentes,
                                    Porcentaje_Ausentes = porcentaje_ausentes,
                                    stringsAsFactors = FALSE)

    valores_ausentes <- rbind(valores_ausentes, valores_atributos)

  }

  return(valores_ausentes)
}

#' Reemplazar Valores en una Columna de una Base de Datos
#'
#' Esta función permite reemplazar valores en una columna específica de una base de datos.
#'
#' @param data Un data frame que representa la base de datos en la que se realizará el reemplazo.
#' @param columna El nombre de la columna en la que se reemplazarán los valores.
#' @param valor_original El valor que se desea reemplazar en la columna.
#' @param valor_nuevo El nuevo valor con el que se reemplazará el valor original.
#'
#' @return Un data frame con los valores reemplazados en la columna especificada.
#'
#' @details La función busca el valor original en la columna especificada y lo reemplaza por el valor nuevo.
#'
#' @examples
#' # Crear un data frame de ejemplo
#' data <- data.frame(
#'   Nombre = c("Ana", "Juan", "María", "Pedro"),
#'   Edad = c(25, 30, 22, 30)
#' )
#'
#' # Reemplazar el valor 30 en la columna "Edad" por 31
#' nuevo_data <- reemplazar_valores(data, "Edad", 30, 31)
#'
#' @importFrom base colnames
#' @importFrom base missing
#' @importFrom base stop
#'
#' @export

reemplazar_valores <- function(data, columna, valor_original, valor_nuevo) {
  if (missing(data) || missing(columna) || missing(valor_original) || missing(valor_nuevo)) {
    stop("Todos los argumentos deben ser especificados: data, columna, valor_original y valor_nuevo")
  }

  if (!columna %in% colnames(data)) {
    stop("La columna especificada no existe en la base de datos")
  }

  data[[columna]][data[[columna]] == valor_original] <- valor_nuevo

  return(data)
}


#' Calcular Kurtosis y Skewness con Gráficos
#'
#' Esta función calcula la kurtosis y skewness de un vector de datos numéricos
#' y genera histogramas y boxplots para visualización. También determina si los
#' datos se desvían de una distribución normal basándose en umbrales predefinidos.
#'
#' @param data Un vector numérico o un data frame que contiene los datos a analizar.
#' @return Un data frame que contiene la kurtosis, skewness y si los datos se desvían
#'         de una distribución normal para cada variable.
#' @export
#' @importFrom moments kurtosis skewness
#' @importFrom ggplot2 ggplot geom_histogram geom_boxplot labs theme_minimal
#' @examples
#' data <-  c(2.3, 1.5, 3.7, 2.8, 4.2, 5.6)
#'
#' calculate_kurtosis_skewness(data)
calculate_kurtosis_skewness <- function(data) {
  if (!requireNamespace("moments", quietly = TRUE)) {
    install.packages("moments")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  library(moments)
  library(ggplot2)
  library(dplyr)

  kurt <- kurtosis(data)
  skew <- skewness(data)

  hist_plot <- ggplot(data.frame(x = data), aes(x)) +
    geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
    labs(title = "Histograma", x = "Valor") +
    theme_minimal()

  boxplot_plot <- ggplot(data.frame(x = data), aes(y = x)) +
    geom_boxplot(fill = "lightgreen") +
    labs(title = "Diagrama de Caja", y = "Valor") +
    theme_minimal()

  normal_test <- ifelse(abs(skew) > 2 | abs(kurt - 3) > 2, "Sí", "No")

  result_df <- data.frame(
    Kurtosis = kurt,
    Skewness = skew,
    SeDesvíaDeNormal = normal_test
  )

  cat("Kurtosis:", kurt, "\n")
  cat("Skewness:", skew, "\n")
  cat("Se desvía de la Distribución Normal:", normal_test, "\n")

  print(hist_plot)
  print(boxplot_plot)

  return(result_df)
}


#' Calcular Kurtosis y Skewness para Todas las Variables Numéricas
#'
#' Esta función toma un data frame y calcula la kurtosis y skewness de todas las variables
#' numéricas en él. Además, genera histogramas y boxplots para visualizar las distribuciones
#' de las variables numéricas y determina si los datos se desvían de una distribución normal.
#'
#' @param data Un data frame que contiene las variables a analizar.
#' @return Un data frame que contiene la kurtosis, skewness y si los datos se desvían
#'         de una distribución normal para cada variable numérica.
#' @export
#' @importFrom moments kurtosis skewness
#' @importFrom ggplot2 ggplot geom_histogram geom_boxplot labs theme_minimal
#' @importFrom dplyr select_if bind_rows
#' @examples
#' data <- data.frame(
#'   var1 = c(2.3, 1.5, 3.7, 2.8, 4.2, 5.6),
#'   var2 = c(7.8, 6.4, 8.2, 7.1, 9.5, 10.2),
#'   var3 = c(12.1, 14.8, 15.6, 13.2, 16.7, 11.9)
#' )
#' calculate_kurtosis_skewness_all(data)
calculate_kurtosis_skewness_all <- function(data) {
  if (!requireNamespace("moments", quietly = TRUE)) {
    install.packages("moments")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  library(moments)
  library(ggplot2)
  library(dplyr)

  numeric_vars <- data %>%
    select_if(is.numeric)

  results_list <- lapply(numeric_vars, calculate_kurtosis_skewness)

  combined_results <- bind_rows(results_list, .id = "Variable")

  return(combined_results)
}

