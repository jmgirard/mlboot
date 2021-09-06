
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mlboot

The goal of *mlboot* is to provide a powerful, flexible, and
user-friendly way to estimate and compare the performance of machine
learning (and other predictive) models using bootstrap resampling. It
was created in collaboration by [Jeffrey Girard](https://jmgirard.com/)
and [Zhun Liu](http://justin1904.github.io/); this version for R is
maintained by Jeffrey Girard and a similar version for Python is
maintained by Zhun Liu at https://github.com/Justin1904/mlboot-py.

## Installation

``` r
devtools::install_github("jmgirard/mlboot")
```

## Rationale

Bootstrapping is a good choice for estimating the performance of machine
learning models because it can be adapted to nearly any type of
performance metric, does not make parameteric assumptions about their
distributions, and can be quite accurate with relatively little data
(e.g., it has been suggested that bootstrapping is appropriate for
sample sizes as low as 20, although larger samples obviously confer many
benefits). Furthermore, bootstrap confidence intervals provide easily
understood information about the precision and reliability of
performance estimates and readily extend to statistical comparison.

## Usage

### Estimating the performance of a single model

We can simulate some simple labels and predictions to demonstrate this.
Let’s say we have data from 1000 videos and we are trying to predict
ratings of each video’s perceived sentiment (i.e.,
positivity-versus-negativity) on a scale from 0 to 100. We train a
machine learning model on separate data and then generate predictions
for each of the 1000 videos just described. We can calculate the
performance of this model as the mean absolute error (MAE) across all
1000 videos. However, it would also be nice to know how precise or
reliable this estimate is, i.e., how much it is likely to vary as a
function of sampling error. To estimate this precision, we can construct
a confidence interval around the observed MAE value using bootstrap
resampling.

``` r
# Load the mlboot package
library(mlboot)

# Set random seed for reproducible results
set.seed(2020)

# Generate random numbers to simulate trusted labels
ratings <- rnorm(n = 1000, mean = 50, sd = 10)

# Perturb the trusted labels to simulate predictions
model1 <- ratings + rnorm(n = 1000, mean = 10, sd = 10)

# Combine variables into a dataframe
dat <- data.frame(ratings, model1)

# Estimate performance of the simulated predictions using the MAE performance metric
results <- 
  mlboot(
    .data = dat,
    trusted = "ratings",
    predicted = "model1",
    metric = mean_absolute_error
  )

results
#> mlboot Results
#> 
#> Sample:          N=1000, Clusters=NA
#> Bootstrap:       Quantile, R=2000, CI=0.95
#> Metric:          mean_absolute_error, Null=0
#> 
#>          Estimate   Lower.CI   Upper.CI       p   p.signif
#> model1     11.661     11.149     12.157   0.000        ***
```

The output shows that the observed performance (MAE) in the simulated
sample was 12. This is quite close to the perturbation of 10 that we
added to create the labels, which is a good sign that our metric
function is working properly. The output also shows a 95% confidence
interval around the observed sample statistic; thus, we can be quite
confident that the “true” population value of the performance metric is
between 11.149 and 12.157. The p-value of 0.000 suggests that this MAE
scores is significantly different from zero.

### Estimating and comparing the performance of two models

Now let’s say we develop another model that is more accurate. We can use
a very similar approach (and indeed the same function call, with
additional arguments) to estimate the performance of this second model
and assess the degree to which the models differ in performance.

``` r
# Set random seed for reproducible results
set.seed(2020)

# Perturb the trusted labels to a lesser degree to simulate better predictions
model2 <- ratings + rnorm(n = 1000, mean = 8.5, sd = 10)

# Append to existing dataframe
dat2 <- cbind(dat, model2)

# Estimate performance of both models and compare them using the MAE metric
results2 <- 
  mlboot(
    .data = dat2,
    trusted = "ratings",
    predicted = c("model1", "model2"),
    metric = mean_absolute_error,
    pairwise = TRUE
  )

results2
#> mlboot Results
#> 
#> Sample:          N=1000, Clusters=NA
#> Bootstrap:       Quantile, R=2000, CI=0.95
#> Metric:          mean_absolute_error, Null=0
#> 
#>                   Estimate   Lower.CI   Upper.CI       p   p.signif
#> model1              11.661     11.150     12.164   0.000        ***
#> model2              10.694     10.220     11.192   0.000        ***
#> model1 - model2      0.967      0.266      1.624   0.006         **
```

The output shows the same observed performance for the first model,
although the confidence interval is slightly different due to the
stochastic nature of resampling. (If more consistent confidence interval
bounds are desired, additional bootstrap resamples can be requested
using the `nboot` argument.) The second model had an observed
performance score of 10.694 which is indeed lower than that of the first
model. To determine whether this difference is statistically
significant, we can estimate the average difference between the
performance scores of the models. The observed difference was 0.967 and
the confidence interval extends from 0.266 to 1.624. Because the
confidence interval does not include zero and the p-value is less than
0.05, we can conclude with 95% confidence that the second model has a
lower mean absolute error than the first model.

### Estimating and comparing the performance of many models

The same approach can be applied to any number of models. When `pairwise
= TRUE`, all pairs of models will be compared.

``` r
# Set random seed for reproducible results
set.seed(2020)

# Perturb the trusted labels to different degrees
model3 <- ratings + rnorm(n = 1000, mean = 10, sd = 10)
model4 <- ratings + rnorm(n = 1000, mean = 5, sd = 10)

# Append to existing dataframe
dat3 <- cbind(dat2, model3, model4)

# Estimate performance of both models and compare them using the MAE metric
results3 <- 
  mlboot(
    .data = dat3,
    trusted = "ratings",
    predicted = c("model1", "model2", "model3", "model4"),
    metric = mean_absolute_error,
    pairwise = TRUE
  )

results3
#> mlboot Results
#> 
#> Sample:          N=1000, Clusters=NA
#> Bootstrap:       Quantile, R=2000, CI=0.95
#> Metric:          mean_absolute_error, Null=0
#> 
#>                   Estimate   Lower.CI   Upper.CI       p   p.signif
#> model1              11.661     11.149     12.157   0.000        ***
#> model2              10.694     10.225     11.193   0.000        ***
#> model3              11.640     11.146     12.164   0.000        ***
#> model4               8.855      8.437      9.280   0.000        ***
#> model1 - model2      0.967      0.269      1.635   0.007         **
#> model1 - model3      0.021     -0.698      0.707   0.954           
#> model1 - model4      2.806      2.557      3.044   0.000        ***
#> model2 - model3     -0.946     -1.015     -0.874   0.000        ***
#> model2 - model4      1.839      1.241      2.500   0.000        ***
#> model3 - model4      2.784      2.164      3.472   0.000        ***
```

### Using the cluster bootstrap for hierarchical data

It is common in many areas of applied machine learning to have testing
sets that are hierarchical in structure. For example, there may be
multiple testing examples that are clustered (e.g., come from the same
individuals or groups) and therefore are not independent. Ignoring this
dependency would result in biased estimates, so we need to account for
it in some way. Although hierarchical resampling is an active area of
research, two recent studies (Field & Welsh, 2007; Ren et al. 2010)
suggest that the cluster bootstrap is an accurate and powerful approach
to this issue. By supplying a variable indicating cluster membership for
each testing example, `mlboot()` can implement the cluster bootstrap
procedure. Note that this approach may lead to inaccuracies when the
number of clusters is low (e.g., fewer than 20).

``` r
# Set random seed for reproducible results
set.seed(2020)

# Assume the examples come from 50 different clusters corresponding to persons
person <- rep(1:50, each = 20)

# Generate random numbers to simulate trusted labels
ratings2 <- rnorm(n = 1000, mean = 20 + person, sd = 10)

# Perturb the trusted labels to simulate predictions
model4 <- ratings2 + rnorm(n = 1000, mean = 10 - person, sd = 10)
model5 <- ratings2 + rnorm(n = 1000, mean = 9 - person, sd = 10)

# Combine variables into dataframe
dat4 <- data.frame(person, ratings2, model4, model5)

# Estimate and compare the models using the cluster bootstrap
results4 <- 
  mlboot(
    .data = dat4,
    trusted = "ratings2",
    predicted = c("model4", "model5"),
    metric = mean_absolute_error,
    cluster = person,
    pairwise = TRUE
  )

results4
#> mlboot Results
#> 
#> Sample:          N=1000, Clusters=50
#> Bootstrap:       Quantile, R=2000, CI=0.95
#> Metric:          mean_absolute_error, Null=0
#> 
#>                   Estimate   Lower.CI   Upper.CI       p   p.signif
#> model4              19.339     16.394     22.524   0.000        ***
#> model5              19.829     16.897     22.899   0.000        ***
#> model4 - model5     -0.490     -1.236      0.310   0.219
```

## Code of Conduct

Please note that the ‘mlboot’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.

## References

Efron, B., & Tibshirani, R. J. (1993). *An introduction to the
bootstrap.* New York, NY: Chapman and Hall.

Field, C. A., & Welsh, A. H. (2007). Bootstrapping clustered data.
*Journal of the Royal Statistical Society: Series B (Statistical
Methodology), 69*(3), 369–390. <https://doi.org/10/cqwx5p>

Ren, S., Lai, H., Tong, W., Aminzadeh, M., Hou, X., & Lai, S. (2010).
Nonparametric bootstrapping for hierarchical data. *Journal of Applied
Statistics, 37*(9), 1487–1498. <https://doi.org/10/dvfzcn>
