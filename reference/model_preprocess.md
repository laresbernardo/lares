# Automate Data Preprocess for Modeling

Pre-process your data before training a model. This is the prior step on
the
[`h2o_automl()`](https://laresbernardo.github.io/lares/reference/h2o_automl.md)
function's pipeline. Enabling for other use cases when wanting too use
any other framework, library, or custom algorithm.

## Usage

``` r
model_preprocess(
  df,
  y = "tag",
  ignore = NULL,
  train_test = NA,
  split = 0.7,
  weight = NULL,
  target = "auto",
  balance = FALSE,
  impute = FALSE,
  no_outliers = TRUE,
  unique_train = TRUE,
  center = FALSE,
  scale = FALSE,
  thresh = 10,
  seed = 0,
  quiet = FALSE
)
```

## Arguments

- df:

  Dataframe. Dataframe containing all your data, including the dependent
  variable labeled as `'tag'`. If you want to define which variable
  should be used instead, use the `y` parameter.

- y:

  Character. Column name for dependent variable or response.

- ignore:

  Character vector. Force columns for the model to ignore

- train_test:

  Character. If needed, `df`'s column name with 'test' and 'train'
  values to split

- split:

  Numeric. Value between 0 and 1 to split as train/test datasets. Value
  is for training set. Set value to 1 to train with all available data
  and test with same data (cross-validation will still be used when
  training). If `train_test` is set, value will be overwritten with its
  real split rate.

- weight:

  Column with observation weights. Giving some observation a weight of
  zero is equivalent to excluding it from the dataset; giving an
  observation a relative weight of 2 is equivalent to repeating that row
  twice. Negative weights are not allowed.

- target:

  Value. Which is your target positive value? If set to `'auto'`, the
  target with largest `mean(score)` will be selected. Change the value
  to overwrite. Only used when binary categorical model.

- balance:

  Boolean. Auto-balance train dataset with under-sampling?

- impute:

  Boolean. Fill `NA` values with MICE?

- no_outliers:

  Boolean/Numeric. Remove `y`'s outliers from the dataset? Will remove
  those values that are farther than n standard deviations from the
  dependent variable's mean (Z-score). Set to `TRUE` for default (3) or
  numeric to set a different multiplier.

- unique_train:

  Boolean. Keep only unique row observations for training data?

- center, scale:

  Boolean. Using the base function scale, do you wish to center and/or
  scale all numerical values?

- thresh:

  Integer. Threshold for selecting binary or regression models: this
  number is the threshold of unique values we should have in `'tag'`
  (more than: regression; less than: classification)

- seed:

  Integer. Set a seed for reproducibility. AutoML can only guarantee
  reproducibility if max_models is used because max_time is resource
  limited.

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

## Value

List. Contains original data.frame `df`, an index to identify which
observations with be part of the train dataset `train_index`, and which
model type should be `model_type`.

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
[`lasso_vars()`](https://laresbernardo.github.io/lares/reference/lasso_vars.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md),
[`msplit()`](https://laresbernardo.github.io/lares/reference/msplit.md)

## Examples

``` r
data(dft) # Titanic dataset

model_preprocess(dft, "Survived", balance = TRUE)
#> - DEPENDENT VARIABLE: Survived
#> - MODEL TYPE: Classification
#> # A tibble: 2 × 5
#>   tag       n     p order  pcum
#>   <lgl> <int> <dbl> <int> <dbl>
#> 1 FALSE   549  61.6     1  61.6
#> 2 TRUE    342  38.4     2 100  
#> - MISSINGS: The following variables contain missing observations: Age (19.87%). Consider using the impute parameter.
#> - CATEGORICALS: There are 5 non-numerical features. Consider using ohse() or equivalent prior to encode categorical variables.
#> >>> Splitting data: train = 0.7 &&  test = 0.3
#> train_size  test_size 
#>        623        268 
#> - BALANCE: Training set balanced: 243 observations for each (2) category; using 78.01% of training data

model_preprocess(dft, "Fare", split = 0.5, scale = TRUE)
#> - DEPENDENT VARIABLE: Fare
#> - MODEL TYPE: Regression
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    0.00    7.91   14.45   32.20   31.00  512.33 
#> - MISSINGS: The following variables contain missing observations: Age (19.87%). Consider using the impute parameter.
#> - CATEGORICALS: There are 6 non-numerical features. Consider using ohse() or equivalent prior to encode categorical variables.
#> - TRANSFORMATIONS: All numerical features (4) were scaled
#> >>> Splitting data: train = 0.5 &&  test = 0.5
#> train_size  test_size 
#>        435        436 

model_preprocess(dft, "Pclass", ignore = c("Fare", "Cabin"))
#> - DEPENDENT VARIABLE: Pclass
#> - MODEL TYPE: Classification
#> # A tibble: 3 × 5
#>   tag       n     p order  pcum
#>   <fct> <int> <dbl> <int> <dbl>
#> 1 n_3     491  55.1     1  55.1
#> 2 n_1     216  24.2     2  79.4
#> 3 n_2     184  20.6     3 100  
#> - MISSINGS: The following variables contain missing observations: Age (19.87%). Consider using the impute parameter.
#> - SKIPPED: Ignored variables for training models: 'Fare', 'Cabin'
#> - CATEGORICALS: There are 4 non-numerical features. Consider using ohse() or equivalent prior to encode categorical variables.
#> >>> Splitting data: train = 0.7 &&  test = 0.3
#> train_size  test_size 
#>        623        268 

model_preprocess(dft, "Pclass", quiet = TRUE)
```
