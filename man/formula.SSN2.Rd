% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formula.R
\name{formula.SSN2}
\alias{formula.SSN2}
\alias{formula.ssn_lm}
\alias{formula.ssn_glm}
\title{Model formulae}
\usage{
\method{formula}{ssn_lm}(x, ...)

\method{formula}{ssn_glm}(x, ...)
}
\arguments{
\item{x}{A fitted model object from \code{\link[=ssn_lm]{ssn_lm()}} or \code{\link[=ssn_glm]{ssn_glm()}}.}

\item{...}{Other arguments. Not used (needed for generic consistency).}
}
\value{
The formula used by a fitted model object.
}
\description{
Return formula used by a fitted model object.
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
formula(ssn_mod)
}
