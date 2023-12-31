% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Funciones.R
\name{dicotomizar_variables_cat}
\alias{dicotomizar_variables_cat}
\title{Dicotomizar Variables Categóricas}
\usage{
dicotomizar_variables_cat(data, columnas)
}
\arguments{
\item{data}{Un data frame en el que se desea dicotomizar las variables.}

\item{columnas}{Un vector de nombres de columnas o factores que se desean dicotomizar.}
}
\value{
Un data frame con nuevas columnas dicotomizadas para cada valor único en las columnas especificadas.
}
\description{
Esta función permite dicotomizar variables categóricas (cadenas o factores) en un data frame.
}
\details{
La función verifica los argumentos proporcionados y realiza la dicotomización de las variables categóricas. Crea nuevas columnas con valores binarios para cada valor único en las columnas factores.
}
\examples{
# Crear un data frame de ejemplo
data <- data.frame(
  Genero = c("Masculino", "Femenino", "Masculino", "Femenino"),
  Ciudad = c("A", "B", "A", "C")
)

# Dicotomizar las variables "Genero" y "Ciudad"
columnas_a_dicotomizar <- c("Genero", "Ciudad")
nuevo_data <- dicotomizar_variables_cat(data, columnas_a_dicotomizar)

}
