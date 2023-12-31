% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlation_matrix.R
\name{cor_matrix}
\alias{cor_matrix}
\title{CORRELATION MATRIX}
\usage{
cor_matrix(data = ., variables, digits = 3)
}
\arguments{
\item{data}{A dataframe object. This should be a structured dataset where
each column represents a variable and each row represents an observation.}

\item{variables}{A required vector of column names representing their 
manifest variables. For latent and composite variables that are comprised of
multiple indicators, use the desc_wrapper() function and specify a named
list of variables in the \code{varlist} argument.}

\item{digits}{Number of decimal places for the correlation matrix. Default
is 3 decimal points. If not specified, the function will use the default
value.}
}
\value{
A correlation matrix dataframe with variable mean and standard
deviation. The dataframe can be exported as an excel workbook to be copy
and pasted into a word document or LaTeX.
}
\description{
Create a correlation matrix with variable mean and standard deviation.
The 'engine' of the correlation matrix is derived from Stefan Engineering's
code (Stefan Engineering, 2018).
}
\examples{
variables <- c("age", "education", "income")
cor_matrix(data = data, variables = variables, digits = 3)

}
\references{
Stefan Engineering (2018). Create an APA style correlation
table with R. \url{https://stefaneng.github.io/apa_correlation_table/}.
}
