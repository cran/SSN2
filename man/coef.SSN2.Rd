% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coef.R
\name{coef.SSN2}
\alias{coef.SSN2}
\alias{coef.ssn_lm}
\alias{coefficients.ssn_lm}
\alias{coef.ssn_glm}
\alias{coefficients.ssn_glm}
\title{Extract fitted model coefficients}
\usage{
\method{coef}{ssn_lm}(object, type = "fixed", ...)

\method{coefficients}{ssn_lm}(object, type = "fixed", ...)

\method{coef}{ssn_glm}(object, type = "fixed", ...)

\method{coefficients}{ssn_glm}(object, type = "fixed", ...)
}
\arguments{
\item{object}{A fitted model object from \code{\link[=ssn_lm]{ssn_lm()}} or \code{\link[=ssn_glm]{ssn_glm()}}.}

\item{type}{\code{"fixed"} for fixed effect coefficients, \code{"tailup"} for
tailup covariance parameter coefficients, \code{"taildown"} for
taildown covariance parameter coefficients, \code{"euclid"} for
Euclidean covariance parameter coefficients, \code{"nugget"} for
nugget covariance parameter coefficients, \code{"dispersion"} for
the dispersion parameter coefficient (\code{ssn_glm()} objects), \code{"randcov"} for random effect
variance coefficients, or \code{"ssn"} for all of the tailup, taildown,
Euclidean, nugget, and dispersion (\code{ssn_glm()} objects) parameter coefficients.
Defaults to \code{"fixed"}.}

\item{...}{Other arguments. Not used (needed for generic consistency).}
}
\value{
A named vector of coefficients.
}
\description{
\code{coef} extracts fitted model coefficients from fitted model objects.
\code{coefficients} is an alias for it.
}
\examples{
# Copy the mf04p .ssn data to a local directory and read it into R
# When modeling with your .ssn object, you will load it using the relevant
# path to the .ssn data on your machine
copy_lsn_to_temp()
temp_path <- paste0(tempdir(), "/MiddleFork04.ssn")
mf04p <- ssn_import(temp_path, overwrite = TRUE)

ssn_mod <- ssn_lm(
  formula = Summer_mn ~ ELEV_DEM,
  ssn.object = mf04p,
  tailup_type = "exponential",
  additive = "afvArea"
)
coef(ssn_mod)
coef(ssn_mod, type = "tailup")
coefficients(ssn_mod)
}
