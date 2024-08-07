% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssn_subset.R
\name{ssn_subset}
\alias{ssn_subset}
\title{Subset an \code{SSN} object}
\usage{
ssn_subset(ssn, path, subset, clip = FALSE, overwrite = FALSE)
}
\arguments{
\item{ssn}{An \code{SSN} object.}

\item{path}{The filepath to the .ssn folder, in string format,
where the subset \code{SSN} will be saved.}

\item{subset}{A logical expression indicating which features to keep.}

\item{clip}{If \code{TRUE}, create a subset of the edges and
prediction sites, based on the same logical expression used to
subset the observed sites.  Default = \code{FALSE}.}

\item{overwrite}{If \code{TRUE}, overwrite the folder specified in
\code{path} if it exists. Default = FALSE.}
}
\value{
an object of class \code{SSN}, which is stored locally in the .ssn
directory specified in \code{path}. It also creates and
stores an SQLite database, binaryID.db, within the .ssn
directory.
}
\description{
Returns an \code{SSN} object that has been subset based on a
logical expression.
}
\details{
This function creates a subset of the original \code{SSN}
object based on a logical expression defined in the \code{subset}
argument. The \code{subset} argument is treated as an expression
within \code{ssn_subset()} and so the full argument is not a
string; although values in factor or character format will still
require quotes (see examples). If \code{clip = TRUE}, then the
columns referred to in \code{subset} must be present in the edges
and all of the prediction datasets (if present in the \code{SSN}
object). Note that features with missing values in the \code{subset}
expression are treated as false and are not included in the
subset \code{SSN} object.

Once the subset \code{SSN} object has been written to the local
directory, it is re-imported using
\code{\link[SSN2]{ssn_import}}. During this process, the
binaryID.db is recreated. If distance matrices exist in the
original \code{SSN} object, they are not copied or recalculated
for the new \code{SSN} object. Users will need to run the
\code{\link[SSN2]{ssn_create_distmat}} to create the distance
matrices before fitting models to the data in the subset
\code{SSN}.
}
\examples{
## Import SSN object
copy_lsn_to_temp() ## Only needed for this example
mf04p <- ssn_import(paste0(tempdir(), "/MiddleFork04.ssn"),
  predpts = "pred1km",
  overwrite = TRUE
)

## Subset SSN observations, edges, and prediction sites on network 1
ssn.sub1 <- ssn_subset(mf04p,
  path = paste0(tempdir(), "/subset1.ssn"),
  subset = netID == 1, clip = TRUE,
  overwrite = TRUE
)

## Subset SSN observations, removing two sites
ssn.sub2 <- ssn_subset(mf04p,
  path = paste0(tempdir(), "/subset2.ssn"),
  subset = !COMID \%in\% c("23519461", "23519365"),
  overwrite = TRUE
)
}
