## CLASSIFICATION

# Accuracy / Agreement
#' @export
accuracy_score <- function(y_true, y_pred, na.rm = TRUE) {
  stopifnot(length(y_true) == length(y_pred))
  mean(y_true == y_pred, na.rm = na.rm)
}

# F1 Score
#' @export
f1_score <- function(y_true, y_pred, pos = 1, neg = 0, na.rm = TRUE) {
  stopifnot(length(y_true) == length(y_pred))
  tp <- sum(y_true == pos & y_pred == pos, na.rm = na.rm)
  fp <- sum(y_true == neg & y_pred == pos, na.rm = na.rm)
  fn <- sum(y_true == pos & y_pred == neg, na.rm = na.rm)
  tn <- sum(y_true == neg & y_pred == neg, na.rm = na.rm)
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  2 * precision * recall / (precision + recall)
}

# Matthews Correlation Coefficient
#' @export
matthews_corrcoef <- function(y_true, y_pred, pos = 1, neg = 0, na.rm = TRUE) {
  stopifnot(length(y_true) == length(y_pred))
  tp <- sum(y_true == pos & y_pred == pos, na.rm = na.rm)
  fp <- sum(y_true == neg & y_pred == pos, na.rm = na.rm)
  fn <- sum(y_true == pos & y_pred == neg, na.rm = na.rm)
  tn <- sum(y_true == neg & y_pred == neg, na.rm = na.rm)
  (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
}

## REGRESSION

# Explained Variance Score
#' @export
explained_variance_score <- function(y_true, y_pred, na.rm = TRUE) {
  stopifnot(length(y_true) == length(y_pred))
  1 - var(y_true - y_pred, na.rm = na.rm) / var(y_true, na.rm = na.rm)
}

# Mean Absolute Error
#' @export
mean_absolute_error <- function(y_true, y_pred, na.rm = TRUE) {
  stopifnot(length(y_true) == length(y_pred))
  mean(abs(y_true - y_pred), na.rm = na.rm)
}

# Mean Squared Error
#' @export
mean_squared_error <- function(y_true, y_pred, na.rm = TRUE) {
  stopifnot(length(y_true) == length(y_pred))
  mean((y_true - y_pred)^2, na.rm = na.rm)
}

# R-squared / Coefficient of Determination
#' @export
r2_score <- function(y_true, y_pred, na.rm = TRUE) {
  stopifnot(length(y_true) == length(y_pred))
  1 - sum((y_true - y_pred)^2, na.rm = na.rm) / 
    sum((y_true - mean(y_pred, na.rm = na.rm))^2, na.rm = na.rm)
}

# Pearson Correlation
#' @export
cor_score <- function(y_true, y_pred, na.rm = TRUE) {
  stopifnot(length(y_true) == length(y_pred))
  use <- ifelse(na.rm == TRUE, "pairwise", "everything")
  cor(y_pred, y_true, use = use, method = "pearson")
}



