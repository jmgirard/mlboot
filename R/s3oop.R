## S3 Class Constructors
new_s3_list <- function(x, ..., class) {
  stopifnot(is.list(x))
  stopifnot(is.character(class))
  structure(x, ..., class = class)
}

## S3 Class Methods
#' @export
print.mlboot <- function(x, digits = 3, star = TRUE, ...) {
  # Print header
  cat(
    "mlboot Results\n\n",
    "Sample:      \tN=", x$n_total, ", Clusters=", x$n_cluster, "\n",
    "Bootstrap:   \tBCa, R=", x$n_boot, ", CI=", x$interval, "\n",
    "Metric:      \t", x$metric, "\n\n",
    sep = ""
  )
  # Print results
  out <- data.frame(
    Estimate = round(x$score_obs, digits),
    Lower.CI = round(x$score_cil, digits),
    Upper.CI = round(x$score_ciu, digits)
  )
  
  if (star == TRUE) {
    out <- dplyr::mutate(out, `Sig.` = sig_star(Lower.CI, Upper.CI))
  }
  rownames(out) <- x$score_lab
  
  print.data.frame(out, print.gap = 3L, na.print = "")
  cat("\n")
}

sig_star <- function(lb, ub) {
  contains_zero <- (lb <= 0) & (ub >= 0)
  dplyr::if_else(contains_zero, "", "*")
}
