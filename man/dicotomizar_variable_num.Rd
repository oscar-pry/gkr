% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Funciones.R
\name{dicotomizar_variable_num}
\alias{dicotomizar_variable_num}
\title{Dicotomiza una variable numérica en función de un umbral}
\usage{
dicotomizar_variable_num(variable, umbral)
}
\arguments{
\item{variable}{Un vector numérico a dicotomizar.}

\item{umbral}{El umbral para la dicotomización.}
}
\value{
Un vector dicotómico.
}
\description{
Esta función toma una variable numérica y un umbral y devuelve una variable
dicotómica donde los valores por encima del umbral son 1 y los valores por debajo
del umbral son 0.
}
\examples{
edades <- c(20, 35, 45, 28, 19, 60)
umbral_edad <- 30
edades_dicotomizadas <- dicotomizar_variable_num(edades, umbral_edad)
}
