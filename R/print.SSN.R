#' Print SSN object
#'
#' @description Print information about the data found in an SSN object.
#'
#' @param x An SSN object.
#' @param ... Other arguments. Not used (needed for generic consistency).
#'
#' @return Print summary to console
#'
#' @name print.SSN
#' @method print SSN
#' @export
print.SSN <- function(x, ...) {
  cat("Object of class SSN\n\n")

  nobs <- dim(x$obs)
  # no observed data
  if (is.null(nobs)) {
    nobs <- c(0, 0)
  }
  nobs <- matrix(nobs, 1, )
  np <- length(x$preds)
  if (np > 0) {
    for (i in seq_len(np)) {
      nobs <- rbind(nobs, dim(x$preds[[i]]))
    }
  }

  cat("Object includes observations on", nobs[1, 2], "variables across", nobs[1, 1], "sites within the bounding box\n")
  print(st_bbox(x$edges))
  cat("\n")

  if (np == 1) {
    cat("Object also includes", 1, "set of prediction points with", sum(nobs[, 1]) - nobs[1, 1], "locations\n\n")
  } else if (np > 1) {
    cat("Object also includes", np, "sets of prediction points with a total of", sum(nobs[, 1]) - nobs[1, 1], "locations\n\n")
  }
  cat("Variable names are (found using ssn_names(object)):\n")
  ## print(names(x$preds))
  ssn_names(x)
}
