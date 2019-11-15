#' Calculate bootstrap confidence intervals for a performance metric
#'
#' Calculate the performance scores for one or two predictive models (and their
#' difference) on a given testing set using an arbitrary performance metric and
#' estimate bootstrap confidence intervals around these scores. Bootstrapping
#' can be customized to be basic nonparametric or cluster nonparameter, etc.
#'
#' @param y_true Required. A vector containing "true" or trusted labels for a
#'   set of testing examples.
#' @param y_pred1 Required. A vector containing a predicted label for each
#'   testing example in \code{y_true} from a first predictive model.
#' @param y_pred2 Optional. A vector containing a predicted label for each
#'   testing example in \code{y_true} from a second predictive model (default =
#'   NULL).
#' @param metric Required. A function that takes in at least two arguments (for
#'   trusted labels and predicted labels, plus any additional customization
#'   arguments) and returns a single number indicating performance. A number of
#'   scoring/metric functions are built into the package and custom functions
#'   can be developed as well.
#' @param cluster Optional. A vector containing integers or strings indicating
#'   the cluster membership of each testing example in \code{y_true}, such as
#'   which person or group the testing example comes from (default = NULL).
#' @param nboot Optional. A positive integer indicating how many bootstrap
#'   resamples the confidence intervals should be estimated from (default =
#'   2000).
#' @param interval Optional. A number between 0 and 1 indicating the confidence
#'   level of the confidence intervals to be estimated, such that 0.95 yields
#'   95% confidence intervals (default = 0.95).
#' @return A list containing the results and a description of the analysis.
#'   \item{type}{A string indicating whether a single predictive model was
#'   examined or two models were compared} \item{metric}{A string indicating the
#'   name of the performance metric function used} \item{ntotal}{An integer
#'   indicating the total number of examples in the test set} \item{ncluster}{An
#'   integer indicating the number of clusters present in the test set}
#'   \item{nboot}{An integer indicating the number of bootstrap resamples used
#'   to estimate confidence intervals} \item{interval}{The confidence level of
#'   the confidence intervals} \item{score_obs}{A vector containing the observed
#'   performance score for the first model and, if applicable, the second model
#'   and their difference} \item{score_cil}{A vector containing the lower bounds
#'   of the confidence intervals corresponding to the observed performance
#'   scores} \item{score_ciu}{A vector containing the upper bounds of the
#'   confidence intervals corresponding to the observed performance scores}
#'   \item{score_pval}{A vector containing p-values for the performance scores}
#'   \item{resamples}{A matrix containing the performance scores and, if
#'   applicable, their difference in each bootstrap resample}
#' @export
mlboot <- function(y_true, y_pred1, y_pred2 = NULL, metric, cluster = NULL, 
                   nboot = 2000, interval = 0.95, ...) {
  
  assertthat::assert_that(is.vector(y_true))
  assertthat::assert_that(is.vector(y_pred1))
  assertthat::assert_that(length(y_true) == length(y_pred1))
  assertthat::assert_that(assertthat::is.count(nboot))
  assertthat::assert_that(interval > 0, interval < 1)
  #check that metric function exists
  
  ## Combine the labels and first set of predictions into a tibble
  type <- "single"
  ntotal <- length(y_true)
  bs_data <- dplyr::tibble(y_true, y_pred1, y_pred2 = NA, cluster = NA)
  
  ## If a second set of predictions exists, append them to the tibble
  if (!is.null(y_pred2)) {
    assertthat::assert_that(is.vector(y_pred2))
    assertthat::assert_that(length(y_true) == length(y_pred2))
    type <- "compare"
    bs_data <- dplyr::select(bs_data, -y_pred2)
    bs_data <- dplyr::mutate(bs_data, y_pred2 = y_pred2)
  }
  
  ## If clustering, calculate scores per cluster and bootstrap scores
  if (!is.null(cluster)) {
    assertthat::assert_that(length(y_true) == length(cluster))
    bs_data <- dplyr::select(bs_data, -cluster)
    bs_data <- dplyr::mutate(bs_data, cluster = cluster)
    bs_results <- clusterboot(bs_data, metric, nboot, interval, ...)
    ncluster <- length(unique(cluster))
  } else {
    ## If not clustering, bootstrap rows and then calculate scores
    bs_results <- singleboot(bs_data, metric, nboot, interval, ...)
    ncluster <- 1
  }
  
  ## Get bootstrap confidence intervals
  
  nout <- ifelse(type == "single", 1, 3)
  score_obs <- rep(NA, nout)
  score_cil <- rep(NA, nout)
  score_ciu <- rep(NA, nout)
  score_pval <- rep(NA, nout)
  for (i in 1:nout) {
    bs_ci <- boot::boot.ci(
      boot.out <- bs_results,
      conf = interval,
      type = "bca",
      index = i
    )
    score_obs[[i]] <- bs_ci$t0
    score_cil[[i]] <- bs_ci$bca[[4]]
    score_ciu[[i]] <- bs_ci$bca[[5]]
    t_null <- bs_results$t[, i] - mean(bs_results$t[, i])
    score_pval[[i]] <- mean(abs(t_null) > abs(bs_results$t0[[i]]))
  }
  
  ## Create output object
  output <- new_s3_list(
    list(
      type = type,
      metric = as.character(substitute(metric)),
      ntotal = ntotal,
      ncluster = ncluster,
      nboot = nboot,
      interval = interval,
      score_obs = score_obs,
      score_cil = score_cil,
      score_ciu = score_ciu,
      score_pval = score_pval,
      resamples = bs_results$t
    ),
    class = "mlboot"
  )
  
  output
}

## Function to get results of clustered bootstrap
clusterboot <- function(bs_data, metric, nboot, interval, ...) {
  bs_data <- dplyr::group_by(bs_data, cluster)
  bs_data <- dplyr::summarize(bs_data, 
    n = n(),
    score1 = metric(y_true, y_pred1, ...),
    score2 = metric(y_true, y_pred2, ...),
    difference = score2 - score1
  )
  boot::boot(
    data = bs_data,
    statistic = clusterboot_stat,
    R = nboot
  )
}

clusterboot_stat <- function(data, index) {
  resample <- data[index, ]
  results <- colMeans(resample)[3:5]
  results
}

## Function to get results of non-clustered bootstrap
singleboot <- function(bs_data, metric, nboot, interval, ...) {
  boot::boot(
    data = bs_data,
    statistic = singleboot_stat,
    R = nboot,
    metric = metric,
    ...
  )
}

singleboot_stat <- function(data, index, metric, ...) {
  resample <- data[index, ]
  r_true <- dplyr::pull(resample[, "y_true"])
  r_pred1 <- dplyr::pull(resample[, "y_pred1"])
  r_pred2 <- dplyr::pull(resample[, "y_pred2"])
  score1 <- metric(r_true, r_pred1, ...)
  score2 <- metric(r_true, r_pred2, ...)
  results <- c(score1, score2, score2 - score1)
  results
}
