% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssn_split_predpts.R
\name{ssn_split_predpts}
\alias{ssn_split_predpts}
\title{Split a prediction dataset in an \code{SSN} object}
\usage{
ssn_split_predpts(
  ssn,
  predpts,
  size_predpts,
  by,
  subset,
  id_predpts,
  keep = TRUE,
  drop_levels = FALSE,
  overwrite = FALSE
)
}
\arguments{
\item{ssn}{An \code{SSN} object.}

\item{predpts}{A character string representing the name of the
prediction dataset.}

\item{size_predpts}{numeric value representing the size of the new
prediction sets. The existing prediction set is split equally to
produce multiple prediction sets of this size}

\item{by}{character string representing the column name of type
factor, integer, character or logical that the split will be
based on}

\item{subset}{logical expression indicating which elements or rows
to keep; missing values are taken as \code{FALSE}}

\item{id_predpts}{character string representing the new prediction
dataset name. This value is only specified when the subset method is
used}

\item{keep}{logical value indicating whether the original
prediction dataset should be retained in the \code{SSN}
object. Default is \code{TRUE}}

\item{drop_levels}{logical value indicating whether empty factor
levels should be dropped in the \code{by} column when the new
prediction dataset(s) are created. Default is \code{FALSE}}

\item{overwrite}{logical indicating whether the new prediction
dataset geopackage should be deleted in the .ssn directory if it
already exists. Default = \code{FALSE}}
}
\value{
returns the \code{SSN} specified in \code{ssn}, with one or more new prediction
sets. Geopackages of the new prediction sets are written to the
.ssn directory designated in ssn$path.
}
\description{
The \command{splitPrediction} function is used to
split prediction sets in an \code{SSN} object into smaller
prediction sets. It returns a \code{SSN} object with additional
prediction sets based on equal interval splits, a factor,
integer, character or logical column stored within the prediction
set, or a logical expression.
}
\details{
Three methods have been provided to split prediction sets:
size, by, and subset. The
size method is used to split the existing prediction
set into multiple equally-sized prediction sets using the \code{size_predpts} argument. Note that the
final prediction set may be smaller in size than the others if
the total number of predictions is not evenly divisible by
\code{size_predpts}. The by method is used if the prediction
set is to be split into multiple new prediction sets based on an
existing column of type factor, integer, character, or
logical specified using the argument \code{by}. The subset method is used to create one new
prediction set based on a logical expression defined in \code{subset}.

When more than one prediction dataset is created the prediction
dataset names will be appended with a hyphen and prediction
dataset number if more than one prediction dataset is
created. For example, when "preds" is split using
\code{size_predpts}, the new names will be "preds-1", "preds-2", and
so forth.

When \code{keep=FALSE}, the prediction dataset is removed from
the \code{SSN} object stored in memory, but is not deleted from
the .ssn directory specified in \code{ssn$path}.

Note that, only one
method may be specified when the \command{ssn_split_predpts}
function is called. The distance matrices for the new prediction
datasets must be created using the \code{ssn_create_distmat} before
predictions can be made.
}
\examples{
## Import SSN object
copy_lsn_to_temp() ## Only needed for this example
ssn <- ssn_import(paste0(tempdir(), "/MiddleFork04.ssn"),
  predpts = c("pred1km", "CapeHorn"),
  overwrite = TRUE
)

## Split predictions based on 'size' method
ssn1 <- ssn_split_predpts(ssn, "CapeHorn",
  size_predpts = 200,
  keep = FALSE, overwrite = TRUE
)
names(ssn1$preds)
nrow(ssn1$preds[["CapeHorn-1"]])

## Split predictions using 'by' method
ssn$preds$pred1km$net.fac <- as.factor(ssn$preds$pred1km$netID)
ssn2 <- ssn_split_predpts(ssn, "pred1km",
  by = "net.fac",
  overwrite = TRUE
)
names(ssn2$preds)

## Split predictions using 'subset' method
ssn3 <- ssn_split_predpts(ssn, "pred1km",
  subset = ratio > 0.5,
  id_predpts = "RATIO_05", overwrite = TRUE
)
names(ssn3$preds)
}
