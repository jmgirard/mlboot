## S3 Class Constructors
new_s3_list <- function(x, ..., class) {
  stopifnot(is.list(x))
  stopifnot(is.character(class))
  structure(x, ..., class = class)
}

## S3 Class Methods
#' @export
print.mlboot <- function(x, digits = 3, ...) {
  # Print header
  cat(
    "mlboot Results\n\n",
    "Sample:      \tN=", x$n_total, ", Clusters=", x$n_cluster, "\n",
    "Bootstrap:   \tBCa, R=", x$n_boot, ", CI=", x$interval, "\n",
    "Metric:      \t", x$metric, "\n\n",
    sep = ""
  )
  # Print results
  v <- c(x$score_obs, x$score_cil, x$score_ciu)
  m <- round(matrix(v, ncol = 3), digits)
  rownames(m) <- x$score_lab
  colnames(m) <- c("Estimate", "Lower CI", "Upper CI")
  print.default(m, print.gap = 3L, na.print = "")
  cat("\n")
}
