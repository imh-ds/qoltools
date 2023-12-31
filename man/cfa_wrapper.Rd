% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confirmatory_factor_analysis_wrapper.R
\name{cfa_wrapper}
\alias{cfa_wrapper}
\title{CONFIRMATORY FACTOR ANALYSIS WRAPPER}
\usage{
cfa_wrapper(
  data = .,
  model,
  mi_groups = NULL,
  estimator = "ML",
  se = NULL,
  missing = "listwise"
)
}
\arguments{
\item{data}{A dataframe object. This should be a structured dataset where
each column represents a variable and each row represents an observation.}

\item{model}{`Lavaan` defined confirmatory factor model. Refer to \code{lavaan}
documentation for a description of the user-specified model.}

\item{mi_groups}{A required vector of column names representing the 
categorical group variables to test measurement invariance. Default is NULL
to not run measurement invariance. If specified, then the function will
run measurement invariance on the groups and return fit metrics.}

\item{estimator}{Estimator to be used in the confirmatory factor analysis.
Default is maximum likelihood (ML). Alternative esimators are limited to
those offered by \code{lavaan}, e.g., "GLS", "WLS", "DWLS", "ULS", "DLS", and
"PML". Refer to \code{lavaan} documentation for additional details.}

\item{se}{Parameter to determine how to compute standard errors. Default
is set to NULL. Set to "robust" to use either "robust.sem" or
"robust.huber.white" depending on the estimator used. Refer to lavaan
documentation for additional details.}

\item{missing}{Parameter to determine how to handle missingness. Default
is set to "listwise" where all observations with missing values are deleted
prior to analysis. As detailed in \code{lavaan}'s documentation, this is only
recommended when data are missing completely at random (MCAR). Another
options is "ml" for full information maximum likelihood approach (fiml).
Refer to lavaan documentation for additional details.}
}
\value{
A list of dataframes to be easily exported as an Excel workbook.
Basic output includes the goodness of fit metrics and model estimations.
If measurement invariance parameters were specified, then fit indices and
goodness of fit comparison tables are also included in the returned output.
}
\description{
Runs confirmatory factor analysis (CFA) through the \code{lavaan} package
(Rosseel, 2012) and automates the exporting of commonly used results.
}
\examples{
model <- 'anxiety =~ x1 + x2 + x3
          sleep   =~ z1 + z2'
cfa_wrapper(data, model, mi_groups = c("gender", "ethnicity"))


}
\references{
Yves Rosseel (2012). lavaan: An R Package for Structural
Equation Modeling. \emph{Journal of Statistical Software}, 48(2), 1-36.
\url{https://doi.org/10.18637/jss.v048.i02}.
}
