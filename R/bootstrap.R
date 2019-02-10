## Function to do the bootstrap 
#' @export
mlboot <- function(y_true, y_pred1, y_pred2 = NULL, metric, cluster = NULL, 
                   nboot = 2000, interval = 0.95) {
  
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
    bs_results <- clusterboot(bs_data, metric, nboot, interval)
    ncluster <- length(unique(cluster))
  } else {
    ## If not clustering, bootstrap rows and then calculate scores
    bs_results <- singleboot(bs_data, metric, nboot, interval)
    ncluster <- 1
  }
  
  ## Get bootstrap confidence intervals
  if (type == "single") nout <- 1 else nout <- 3
  score_obs <- rep(NA, nout)
  score_cil <- rep(NA, nout)
  score_ciu <- rep(NA, nout)
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
      resamples = bs_results$t
    ),
    class = "mlboot"
  )
  
  output
}

## Function to get results of clustered bootstrap
clusterboot <- function(bs_data, metric, nboot, interval) {
  bs_data <- dplyr::group_by(bs_data, cluster)
  bs_data <- dplyr::summarize(bs_data, 
    n = n(),
    score1 = metric(y_true, y_pred1),
    score2 = metric(y_true, y_pred2),
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
singleboot <- function(bs_data, metric, nboot, interval) {
  boot::boot(
    data = bs_data,
    statistic = singleboot_stat,
    R = nboot,
    metric = metric
  )
}

singleboot_stat <- function(data, index, metric, ...) {
  resample <- data[index, ]
  r_true <- resample[, "y_true"]
  r_pred1 <- resample[, "y_pred1"]
  r_pred2 <- resample[, "y_pred2"]
  score1 <- metric(r_true, r_pred1, ...)
  score2 <- metric(r_true, r_pred2, ...)
  results <- c(score1, score2, score2 - score1)
  results
}
