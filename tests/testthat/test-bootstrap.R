context("test-bootstrap")

test_that("non-cluster bootstrap works", {
  
  set.seed(2019)
  
  ## Simulate binary data
  labs <- rbinom(n = 1000, size = 1, prob = .70)
  pred1 <- rbinom(n = 1000, size = 1, prob = .60)
  pred2 <- rbinom(n = 1000, size = 1, prob = .50)
  
  ## Test on simulated data
  sr <- mlboot(y_true = labs, y_pred1 = pred1, metric = accuracy_score)
  cr <- mlboot(y_true = labs, y_pred1 = pred1, y_pred2 = pred2, metric = accuracy_score)
  
  testthat::expect_equal(sr$score_obs, 0.556)
  testthat::expect_equal(sr$score_cil, 0.524)
  testthat::expect_equal(sr$score_ciu, 0.585)
  
  testthat::expect_equal(round(cr$score_obs, 3), c(0.556, 0.522, -0.034))
  testthat::expect_equal(round(cr$score_cil, 3), c(0.526, 0.492, -0.078))
  testthat::expect_equal(round(cr$score_ciu, 3), c(0.586, 0.552, 0.009))
  
})

test_that("cluster bootstrap works", {
  
  set.seed(2019)
  
  ## Simulate binary data with clusters
  labs <- rbinom(n = 1000, size = 1, prob = .70)
  pred1 <- rbinom(n = 1000, size = 1, prob = .60)
  pred2 <- rbinom(n = 1000, size = 1, prob = .50)
  cluster <- rep(1:50, 20)
  
  ## Test on simulated data
  sr <- mlboot(y_true = labs, y_pred1 = pred1, metric = accuracy_score, cluster = cluster)
  cr <- mlboot(y_true = labs, y_pred1 = pred1, y_pred2 = pred2, metric = accuracy_score, cluster = cluster)
  
  testthat::expect_equal(sr$score_obs, 0.556)
  testthat::expect_equal(sr$score_cil, 0.528)
  testthat::expect_equal(sr$score_ciu, 0.584)
  
  testthat::expect_equal(cr$score_obs, c(0.556, 0.522, -0.034))
  testthat::expect_equal(cr$score_cil, c(0.527, 0.489, -0.072))
  testthat::expect_equal(cr$score_ciu, c(0.582, 0.554, 0.007))
})
