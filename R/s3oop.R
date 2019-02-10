## S3 Class Constructors
new_s3_list <- function(x, ..., class) {
  stopifnot(is.list(x))
  stopifnot(is.character(class))
  structure(x, ..., class = class)
}

## S3 Class Methods
#' @export
print.mlboot <- function(x, ...) {
  cat(
    "mlboot Results\n",
    "========================================\n",
    "Sample:      \tN=", x$ntotal, ", Clusters=", x$ncluster, "\n",
    "Bootstrap:   \tBCa, R=", x$nboot, ", CI=", x$interval, "\n",
    "Metric:      \t", x$metric, "\n", 
    "========================================\n",
    "y_pred1:     \t", sprintf(
      "%.3f [%.3f, %.3f]", x$score_obs[[1]], x$score_cil[[1]], x$score_ciu[[1]]
    ), "\n", 
    sep = ""
  )
  if (x$type == "compare") {
    cat(
      "y_pred2:   \t", sprintf(
        "%.3f [%.3f, %.3f]", x$score_obs[[2]], x$score_cil[[2]], x$score_ciu[[2]]
      ), "\n",
      "Difference:\t", sprintf(
        "%.3f [%.3f, %.3f]", x$score_obs[[3]], x$score_cil[[3]], x$score_ciu[[3]]
      ), "\n", 
      sep = "")
  }
  cat("========================================\n")
}
