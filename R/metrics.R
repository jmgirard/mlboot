## CLASSIFICATION

# Accuracy / Agreement
#' @export
accuracy_score <- function(y_true, y_pred) {
  stopifnot(length(y_true) == length(y_pred))
  mean(y_true == y_pred)
}

# F1 Score
#' @export
f1_score <- function(y_true, y_pred, pos = 1, neg = 0) {
  stopifnot(length(y_true) == length(y_pred))
  tp <- sum(y_true == pos & y_pred == pos)
  fp <- sum(y_true == neg & y_pred == pos)
  fn <- sum(y_true == pos & y_pred == neg)
  tn <- sum(y_true == neg & y_pred == neg)
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  2 * precision * recall / (precision + recall)
}

# Matthews Correlation Coefficient
#' @export
matthews_corrcoef <- function(y_true, y_pred, pos = 1, neg = 0) {
  stopifnot(length(y_true) == length(y_pred))
  tp <- sum(y_true == pos & y_pred == pos)
  fp <- sum(y_true == neg & y_pred == pos)
  fn <- sum(y_true == pos & y_pred == neg)
  tn <- sum(y_true == neg & y_pred == neg)
  (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
}

## REGRESSION

# Explained Variance Score
#' @export
explained_variance_score <- function(y_true, y_pred) {
  stopifnot(length(y_true) == length(y_pred))
  stopifnot(is.numeric(y_true))
  stopifnot(is.numeric(y_pred))
  1 - var(y_true - y_pred) / var(y_true)
}

# Mean Absolute Error
#' @export
mean_absolute_error <- function(y_true, y_pred) {
  stopifnot(length(y_true) == length(y_pred))
  stopifnot(is.numeric(y_true))
  stopifnot(is.numeric(y_pred))
  mean(abs(y_true - y_pred))
}

# Mean Squared Error
#' @export
mean_squared_error <- function(y_true, y_pred) {
  stopifnot(length(y_true) == length(y_pred))
  stopifnot(is.numeric(y_true))
  stopifnot(is.numeric(y_pred))
  mean((y_true - y_pred)^2)
}

# R-squared / Coefficient of Determination
#' @export
r2_score <- function(y_true, y_pred) {
  stopifnot(length(y_true) == length(y_pred))
  stopifnot(is.numeric(y_true))
  stopifnot(is.numeric(y_pred))
  1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_pred))^2)
}