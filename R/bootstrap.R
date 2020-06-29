#' Calculate bootstrap confidence intervals for a performance metric
#'
#' Calculate the performance scores for one or two predictive models (and their
#' difference) on a given testing set using an arbitrary performance metric and
#' estimate bootstrap confidence intervals around these scores. Bootstrapping
#' can be customized to be basic nonparametric or cluster nonparameter, etc.
#'
#' @param .data Required. A dataframe containing trusted labels and predicted
#'   labels where each row is a single object/observation and each column is a
#'   variable describing that object/observation.
#' @param trusted Required. The name of a single variable in \code{.data} that
#'   contains trusted labels.
#' @param predicted Required. A vector of names of one or more variables in
#'   \code{.data} that contains predicted labels.
#' @param metric Required. A function that takes in at least two arguments (for
#'   trusted labels and predicted labels, plus any additional customization
#'   arguments) and returns a single number indicating performance. A number of
#'   scoring/metric functions are built into the package and custom functions
#'   can be developed as well.
#' @param cluster Optional. The name of a single variable in \code{.data} that
#'   contains the cluster membership of each object/observation.
#' @param pairwise Optional. A logical indicating whether to estimate the
#'   difference between all pairs of predicted labels (default = TRUE).
#' @param n_boot Optional. A positive integer indicating how many bootstrap
#'   resamples the confidence intervals should be estimated from (default =
#'   2000).
#' @param interval Optional. A number between 0 and 1 indicating the confidence
#'   level of the confidence intervals to be estimated, such that 0.95 yields
#'   95% confidence intervals (default = 0.95).
#' @param null Optional. A single number to compare the bootstrap estimate to
#'   when calculating p-values (default = 0).
#' @param ... Optional. Additional arguments to pass along to the \code{metric}
#'   function.
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
mlboot <- function(.data, trusted, predicted, metric, cluster, 
                   pairwise = TRUE, n_boot = 2000, interval = 0.95, 
                   null = 0, ...) {
  
  assertthat::assert_that(assertthat::is.count(n_boot))
  assertthat::assert_that(interval > 0, interval < 1)
  assertthat::assert_that(is.function(metric))
  assertthat::assert_that(assertthat::is.flag(pairwise))
  assertthat::assert_that(assertthat::is.number(null))
  
  bs_data <- dplyr::select(.data, {{trusted}}, {{predicted}}, {{cluster}})
  
  n_total <- nrow(.data)
  
  # Get the trusted variable's name
  trusted_name <- colnames(dplyr::select(bs_data, {{trusted}}))
  # Count and validate the number of trusted variables
  n_trusted <- length(trusted_name)
  assertthat::assert_that(n_trusted == 1)
  
  # Get the predicted variables' names
  predicted_names <- colnames(dplyr::select(bs_data, {{predicted}}))
  # Count and validate the number of predicted variables
  n_predicted <- length(predicted_names)
  assertthat::assert_that(n_predicted >= 1)
  
  ## If clustering, calculate scores per cluster and bootstrap scores
  if (rlang::quo_is_missing(rlang::enquo(cluster)) == FALSE &&
      rlang::quo_is_null(rlang::enquo(cluster)) == FALSE) {
    # Get the name of the clustering variable for later printing
    cluster_name <- colnames(dplyr::select(bs_data, {{cluster}}))
    # Count the number of clusters present in the data
    n_cluster <- length(unique(dplyr::pull(bs_data, {{cluster}})))
    # Nest the data by cluster to allow clustered bootstrapping
    bs_data <- tidyr::nest(bs_data, data = c(-{{cluster}}))
    # Perform the clustered bootstrap procedure
    bs_results <- clusterboot(
      bs_data = bs_data, 
      metric = metric, 
      pairwise = pairwise,
      n_boot = n_boot, 
      interval = interval, 
      trusted_name = trusted_name,
      predicted_names = predicted_names,
      ...
    )
  } else {
    ## If not clustering, bootstrap rows and then calculate scores
    bs_results <- singleboot(
      bs_data = bs_data, 
      metric = metric, 
      pairwise = pairwise,
      n_boot = n_boot, 
      interval = interval, 
      trusted_name = trusted_name,
      predicted_names = predicted_names,
      ...
    )
    n_cluster <- NA
  }
  
  ## Get bootstrap confidence intervals
  
  nout <- length(bs_results$t0)
  score_obs <- rep(NA_real_, nout)
  score_cil <- rep(NA_real_, nout)
  score_ciu <- rep(NA_real_, nout)
  score_lab <- names(bs_results$t0)
  for (i in 1:nout) {
    bs_ci <- boot::boot.ci(
      boot.out = bs_results,
      conf = interval,
      type = "perc",
      index = i
    )
    score_obs[[i]] <- bs_ci$t0
    score_cil[[i]] <- bs_ci$perc[[4]]
    score_ciu[[i]] <- bs_ci$perc[[5]]
  }
  
  ## Create output object
  output <- new_s3_list(
    list(
      metric = as.character(substitute(metric)),
      n_total = n_total,
      n_cluster = n_cluster,
      n_boot = n_boot,
      interval = interval,
      null = null,
      score_lab = score_lab,
      score_obs = score_obs,
      score_cil = score_cil,
      score_ciu = score_ciu,
      pvalue = pvalue(bs_results$t, null),
      resamples = bs_results$t
    ),
    class = "mlboot"
  )
  
  output
}

## Function to get results of clustered bootstrap
clusterboot <- function(bs_data, metric, n_boot, interval, pairwise,
                        trusted_name, predicted_names, ...) {
  boot::boot(
    data = bs_data,
    statistic = clusterboot_stat,
    R = n_boot,
    metric = metric,
    pairwise = pairwise,
    trusted_name = trusted_name,
    predicted_names = predicted_names,
    ...
  )
}

clusterboot_stat <- function(df, index, metric, pairwise,
                             trusted_name, predicted_names, ...) {
  # Create clustered bootstrap resample
  resample <- df[index, ]
  resample <- tidyr::unnest(resample, cols = data)
  # Preallocate results vector
  k <- length(predicted_names)
  if (pairwise == TRUE) {
    results <- rep(NA_real_, k + choose(k, 2))
  } else {
    results <- rep(NA_real_, k)
  }
  # Calculate metric for each source of predicted labels
  for (i in seq_along(predicted_names)) {
    results[[i]] <- metric(
      dplyr::pull(resample, trusted_name), 
      dplyr::pull(resample, predicted_names[[i]]), 
      ...
    )
  }
  names(results)[1:k] <- predicted_names
  # If requested, add all pairwise differences
  if (k > 1 && pairwise == TRUE) {
    pair_diff <- outer(results[1:k], results[1:k], "-")
    pair_diff <- pair_diff[lower.tri(pair_diff)] * -1
    results[(k + 1):length(results)] <- pair_diff
    names(results)[(k + 1):length(results)] <- apply(
      combn(predicted_names, 2), 
      MARGIN = 2,
      FUN = paste,
      collapse = " - "
    )
  }
  results
}

## Function to get results of non-clustered bootstrap
singleboot <- function(bs_data, metric, n_boot, interval, pairwise,
                       trusted_name, predicted_names, ...) {
  boot::boot(
    data = bs_data,
    statistic = singleboot_stat,
    R = n_boot,
    metric = metric,
    pairwise = pairwise,
    trusted_name = trusted_name,
    predicted_names = predicted_names,
    ...
  )
}

singleboot_stat <- function(data, index, metric, pairwise,
                            trusted_name, predicted_names, ...) {
  # Create bootstrap resample
  resample <- data[index, ]
  # Preallocate results vector
  k <- length(predicted_names)
  if (pairwise == TRUE) {
    results <- rep(NA_real_, k + choose(k, 2))
  } else {
    results <- rep(NA_real_, k)
  }
  # Calculate metric for each source of predicted labels
  for (i in seq_along(predicted_names)) {
    results[[i]] <- metric(
      dplyr::pull(resample, trusted_name), 
      dplyr::pull(resample, predicted_names[[i]]), 
      ...
    )
  }
  names(results)[1:k] <- predicted_names
  # If requested, add all pairwise differences
  if (k > 1 && pairwise == TRUE) {
    pair_diff <- outer(results[1:k], results[1:k], "-")
    pair_diff <- pair_diff[lower.tri(pair_diff)] * -1
    results[(k + 1):length(results)] <- pair_diff
    names(results)[(k + 1):length(results)] <- apply(
      combn(predicted_names, 2), 
      MARGIN = 2,
      FUN = paste,
      collapse = " - "
    )
  }
  results
}

pvalue <- function(t, null) {
  sapply(1:ncol(t), function(x) {
    distribution <- ecdf(t[, x])
    qt0 <- distribution(null)
    if (qt0 < 0.5) {
      2 * qt0
    }  else {
      2 * (1 - qt0)
    }
  })
}
