context("test-metrics")

test_that("metrics match expected values", {
  y_true <- c(0, 1, 2, 3)
  y_pred <- c(0, 2, 1, 3)
  testthat::expect_equal(accuracy_score(y_true, y_pred), 1/2)
  
  y_true <- c(0, 1, 0, 1)
  y_pred <- c(0, 1, 0, 0)
  testthat::expect_equal(f1_score(y_true, y_pred), 2/3)
  
  y_true <- c(1, 1, 1, 0)
  y_pred <- c(1, 0, 1, 1)
  testthat::expect_equal(matthews_corrcoef(y_true, y_pred), -1/3)
  
  y_true <- c(3, -0.5, 2, 7)
  y_pred <- c(2.5, 0, 2, 8)
  testthat::expect_equal(mean_absolute_error(y_true, y_pred), 1/2)
  testthat::expect_equal(mean_squared_error(y_true, y_pred), 0.375)
  testthat::expect_equal(round(r2_score(y_true, y_pred), 3), 0.949)
  
})
