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



# Función para convertir objetos POSIXct de una base de datos a cadenas de texto
posixct_a_string <- function(data, formato = "%Y-%m-%d %H:%M:%S") {
  # Obtener los nombres de las columnas
  columnas <- names(data)

  # Iterar sobre cada columna
  for (col in columnas) {
    if (is.POSIXct(data[[col]])) {
      data[[col]] <- format(data[[col]], formato)
    }
  }

  return(data)
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

#' Prueba de normalidad Lilliefors (Kolmogorov-Smirnov) en una muestra
#'
#' Esta función implementa la prueba de normalidad Lilliefors, que es una versión modificada
#' del test de Kolmogorov-Smirnov para probar si una muestra proviene de una distribución normal.
#' La función calcula la estadística de prueba y el valor p asociado a la prueba.
#'
#' @param x Un vector numérico que representa la muestra para la cual se quiere realizar
#'   la prueba de normalidad.
#'
#' @return Un objeto de clase "htest" que contiene los siguientes componentes:
#'   \describe{
#'     \item{statistic}{Un vector con el valor de la estadística de prueba. En este caso,
#'     el valor corresponde a la máxima discrepancia entre la distribución empírica de los datos
#'     y la distribución normal estándar.}
#'     \item{p.value}{El valor p asociado a la prueba de normalidad.}
#'     \item{method}{Una cadena de texto que indica el método utilizado para la prueba. En este
#'     caso, el valor es "Lilliefors (Kolmogorov-Smirnov) normality test".}
#'     \item{data.name}{Una cadena de texto que indica el nombre de la variable o muestra para la
#'     cual se realizó la prueba.}
#'   }
#'
#' @examples
#' data_ejemplo <- c(2.3, 3.1, 2.8, 3.5, 2.6, 2.9, 3.3, 2.7, 3.0, 3.4)
#' resultado_prueba <- TEST_lilliefors(data_ejemplo)
#' print(resultado_prueba)
#'
#' @importFrom stats pnorm
#' @export
#'

estandarizar_variables <- function(data) {
  # Filtrar las columnas numéricas
  columnas_numericas <- sapply(data, is.numeric)
  data_numericas <- data[, columnas_numericas]

  # Estandarizar las variables numéricas
  data_estandarizada <- scale(data_numericas)

  # Renombrar las columnas estandarizadas
  nombres_columnas <- colnames(data_numericas)
  nuevos_nombres <- paste0(nombres_columnas, "_estandarizada")
  colnames(data_estandarizada) <- nuevos_nombres

  # Reemplazar las columnas numéricas originales por las estandarizadas
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

  #' Prueba de normalidad de Lilliefors para variables numéricas
  #'
  #' Esta función realiza la prueba de normalidad de Lilliefors en cada columna numérica del conjunto de datos proporcionado.
  #'
  #' @param data Un dataframe o matriz que contiene las variables a ser probadas.
  #' @param alpha Nivel de significancia para la prueba. El valor predeterminado es 0.05.
  #'
  #' @return Un dataframe con los resultados de la prueba de normalidad para cada columna numérica.
  #' La columna 'Estadistica' contiene los valores de la estadística de prueba,
  #' la columna 'Valor_p' contiene los valores p asociados,
  #' y la columna 'Normalidad' indica si la variable se considera normal o no.
  #'
  #' @importFrom nortest lillie.test
  #' @importFrom utils install.packages
  #'
  #' @examples
  #' data <- data.frame(
  #'   Var1 = rnorm(100),
  #'   Var2 = rnorm(100),
  #'   Var3 = rpois(100, lambda = 3)
  #' )
  #' result <- test_lilliefors_all(data)
  #' print(result)
  #'
  #' @seealso
  #' \link{nortest::lillie.test}
  #'
  #' @keywords prueba normalidad estadísticas
  #'
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
        results[[column]] <- data.frame(Estadistica = result$statistic,
                                        Valor_p = result$p.value,
                                        Normalidad = normality,
                                        stringsAsFactors = FALSE)
      }
    }

    results_df <- do.call(rbind, results)

    return(results_df)
  }


