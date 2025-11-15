# Most Relevant Features Using Lasso Regression

Use Lasso regression to identify the most relevant variables that can
predict/identify another variable. You might want to compare with
[`corr_var()`](https://laresbernardo.github.io/lares/reference/corr_var.md)
and/or [`x2y()`](https://laresbernardo.github.io/lares/reference/x2y.md)
results to compliment the analysis No need to standardize, center or
scale your data. Tidyverse friendly.

## Usage

``` r
lasso_vars(
  df,
  variable,
  ignore = NULL,
  nlambdas = 100,
  nfolds = 10,
  top = 20,
  quiet = FALSE,
  seed = 123,
  ...
)
```

## Arguments

- df:

  Dataframe. Any dataframe is valid as `ohse` will be applied to process
  categorical values, and values will be standardize automatically.

- variable:

  Variable. Dependent variable or response.

- ignore:

  Character vector. Variables to exclude from study.

- nlambdas:

  Integer. Number of lambdas to be used in a search.

- nfolds:

  Integer. Number of folds for K-fold cross-validation (\>= 2).

- top:

  Integer. Plot top n results only.

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- seed:

  Numeric.

- ...:

  Additional parameters passed to
  [`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md).

## Value

List. Contains lasso model coefficients, performance metrics, the actual
model fitted and a plot.

## See also

Other Machine Learning:
[`ROC()`](https://laresbernardo.github.io/lares/reference/ROC.md),
[`conf_mat()`](https://laresbernardo.github.io/lares/reference/conf_mat.md),
[`export_results()`](https://laresbernardo.github.io/lares/reference/export_results.md),
[`gain_lift()`](https://laresbernardo.github.io/lares/reference/gain_lift.md),
[`h2o_automl()`](https://laresbernardo.github.io/lares/reference/h2o_automl.md),
[`h2o_predict_MOJO()`](https://laresbernardo.github.io/lares/reference/h2o_predict.md),
[`h2o_selectmodel()`](https://laresbernardo.github.io/lares/reference/h2o_selectmodel.md),
[`impute()`](https://laresbernardo.github.io/lares/reference/impute.md),
[`iter_seeds()`](https://laresbernardo.github.io/lares/reference/iter_seeds.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md),
[`model_preprocess()`](https://laresbernardo.github.io/lares/reference/model_preprocess.md),
[`msplit()`](https://laresbernardo.github.io/lares/reference/msplit.md)

Other Exploratory:
[`corr_cross()`](https://laresbernardo.github.io/lares/reference/corr_cross.md),
[`corr_var()`](https://laresbernardo.github.io/lares/reference/corr_var.md),
[`crosstab()`](https://laresbernardo.github.io/lares/reference/crosstab.md),
[`df_str()`](https://laresbernardo.github.io/lares/reference/df_str.md),
[`distr()`](https://laresbernardo.github.io/lares/reference/distr.md),
[`freqs()`](https://laresbernardo.github.io/lares/reference/freqs.md),
[`freqs_df()`](https://laresbernardo.github.io/lares/reference/freqs_df.md),
[`freqs_list()`](https://laresbernardo.github.io/lares/reference/freqs_list.md),
[`freqs_plot()`](https://laresbernardo.github.io/lares/reference/freqs_plot.md),
[`missingness()`](https://laresbernardo.github.io/lares/reference/missingness.md),
[`plot_cats()`](https://laresbernardo.github.io/lares/reference/plot_cats.md),
[`plot_df()`](https://laresbernardo.github.io/lares/reference/plot_df.md),
[`plot_nums()`](https://laresbernardo.github.io/lares/reference/plot_nums.md),
[`tree_var()`](https://laresbernardo.github.io/lares/reference/tree_var.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# CRAN
Sys.unsetenv("LARES_FONT") # Temporal
data(dft) # Titanic dataset

m <- lasso_vars(dft, Survived, ignore = c("Cabin"))
print(m$coef)
print(m$metrics)
plot(m$plot)
} # }
```
