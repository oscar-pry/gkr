#' Mi función
#'
#' Esta función realiza una tarea específica.
#'
#' @param x Un vector numérico.
#' @return El resultado de la función.
#' @examples
#' mi_funcion(1:10)
mi_funcion <- function(x) {
  # Implementación de la función
  return(sum(x))
}


convertir_nombres_mayusculas <- function(data) {
  nombres_originales <- colnames(data)
  nombres_mayusculas <- toupper(nombres_originales)
  colnames(data) <- nombres_mayusculas
  return(data)
}



consistencia <- function(data){
  # Crea un data frame vacío para almacenar las características de las variables
  df <- data.frame(variable = character(),
                   tipo = character(),
                   media = numeric(),
                   desviacion_estandar = numeric(),
                   minimo = numeric(),
                   maximo = numeric(),
                   valores_unicos = numeric(),
                   longitud = integer(),
                   valores_faltantes = integer(),
                   porcentaje_valores_faltantes = numeric(),
                   stringsAsFactors = FALSE)

  # Obtener las características de cada variable del conjunto de datos y agregarlas al data frame
  for(i in 1:ncol(data)){
    var_name <- colnames(data)[i]
    var_type <- class(data[[i]])
    var_mean <- ifelse(var_type == "numeric", mean(data[[i]], na.rm = TRUE), NA)
    var_sd <- ifelse(var_type == "numeric", sd(data[[i]], na.rm = TRUE), NA)
    var_min <- ifelse(var_type == "numeric", min(data[[i]], na.rm = TRUE), NA)
    var_max <- ifelse(var_type == "numeric", max(data[[i]], na.rm = TRUE), NA)
    var_unique <- length(unique(data[[i]]))
    var_length <- length(data[[i]])
    var_missing <- sum(is.na(data[[i]]))
    var_missing_pct <- paste(round(var_missing / var_length * 100, 3), "%", sep = "")

    # Agregar las características de la variable al data frame
    df[i,] <- data.frame(var_name,
                         var_type,
                         var_mean,
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

reemplazar_celdas_vacias <- function(data) {
  data[data == ""] <- NA
  return(data)
}


# Función para convertir factores de una base de datos a cadenas de texto
factores_a_string <- function(data) {
  # Obtener los nombres de las columnas
  columnas <- names(data)

  # Iterar sobre cada columna
  for (col in columnas) {
    if (is.factor(data[[col]])) {
      data[[col]] <- as.character(data[[col]])
    }
  }

  return(data)
}


# Función para convertir cadenas de texto de una base de datos a mayúsculas
strings_a_mayusculas <- function(data) {
  # Obtener los nombres de las columnas
  columnas <- names(data)

  # Iterar sobre cada columna
  for (col in columnas) {
    if (is.character(data[[col]])) {
      data[[col]] <- toupper(data[[col]])
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


# Función para convertir objetos POSIXct de una base de datos a fechas
posixct_a_fecha <- function(data) {
  # Obtener los nombres de las columnas
  columnas <- names(data)

  # Iterar sobre cada columna
  for (col in columnas) {
    if (is.POSIXct(data[[col]])) {
      data[[col]] <- as.Date(data[[col]])
    }
  }

  return(data)
}

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


TEST_lilliefors <- function(x) {
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if (n < 5)
    stop("sample size must be greater than 4")
  p <- pnorm((x - mean(x))/sd(x))
  Dplus <- max(seq(1:n)/n - p)
  Dminus <- max(p - (seq(1:n) - 1)/n)
  K <- max(Dplus, Dminus)
  if (n <= 100) {
    Kd <- K
    nd <- n
  }
  else {
    Kd <- K * ((n/100)^0.49)
    nd <- 100
  }
  pvalue <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 *
                  Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598/sqrt(nd) +
                  1.67997/nd)
  if (pvalue > 0.1) {
    KK <- (sqrt(n) - 0.01 + 0.85/sqrt(n)) * K
    if (KK <= 0.302) {
      pvalue <- 1
    }
    else if (KK <= 0.5) {
      pvalue <- 2.76773 - 19.828315 * KK + 80.709644 *
        KK^2 - 138.55152 * KK^3 + 81.218052 * KK^4
    }
    else if (KK <= 0.9) {
      pvalue <- -4.901232 + 40.662806 * KK - 97.490286 *
        KK^2 + 94.029866 * KK^3 - 32.355711 * KK^4
    }
    else if (KK <= 1.31) {
      pvalue <- 6.198765 - 19.558097 * KK + 23.186922 *
        KK^2 - 12.234627 * KK^3 + 2.423045 * KK^4
    }
    else {
      pvalue <- 0
    }
  }
  RVAL <- list(statistic = c(D = K), p.value = pvalue, method = "Lilliefors (Kolmogorov-Smirnov) normality test",
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}
TEST_lilliefors(iris$Sepal.Width)




library(data.table)

detectar_valores_ausentes_vacios <- function(data) {
  # Convertir el dataframe a un objeto de tipo data.table
  dt <- as.data.table(data)

  # Calcular la cantidad de valores NA y celdas vacías "" por variable
  conteo_na <- dt[, lapply(.SD, function(x) sum(is.na(x) | x == "")), .SDcols = names(dt)]

  # Convertir el resultado a un dataframe y organizarlo por variables
  resultado <- as.data.frame(t(conteo_na))
  colnames(resultado) <- c("NA_Count", "Blank_Count")

  return(resultado)
}




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



