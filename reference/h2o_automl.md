# Automated H2O's AutoML

This function lets the user create a robust and fast model, using H2O's
AutoML function. The result is a list with the best model, its
parameters, datasets, performance metrics, variables importance, and
plots. Read more about the `h2o_automl()` pipeline
[here](https://laresbernardo.github.io/lares/articles/machine-learning.html).

## Usage

``` r
h2o_automl(
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
  nfolds = 5,
  max_models = 3,
  max_time = 10 * 60,
  start_clean = FALSE,
  exclude_algos = c("StackedEnsemble", "DeepLearning"),
  include_algos = NULL,
  plots = TRUE,
  alarm = TRUE,
  quiet = FALSE,
  print = TRUE,
  save = FALSE,
  subdir = NA,
  project = "AutoML Results",
  model_name = NULL,
  verbosity = NULL,
  ...
)

# S3 method for class 'h2o_automl'
plot(x, ...)

# S3 method for class 'h2o_automl'
print(x, importance = TRUE, ...)
```

## Arguments

- df:

  Dataframe. Dataframe containing all your data, including the dependent
  variable labeled as `'tag'`. If you want to define which variable
  should be used instead, use the `y` parameter.

- y:

  Variable or Character. Name of the dependent variable or response.

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

- nfolds:

  Number of folds for k-fold cross-validation. Must be \>= 2; defaults
  to 5. Use 0 to disable cross-validation; this will also disable
  Stacked Ensemble (thus decreasing the overall model performance).

- max_models, max_time:

  Numeric. Max number of models and seconds you wish for the function to
  iterate. Note that max_models guarantees reproducibility and max_time
  not (because it depends entirely on your machine's computational
  characteristics)

- start_clean:

  Boolean. Erase everything in the current h2o instance before we start
  to train models? You may want to keep other models or not. To group
  results into a custom common AutoML project, you may use
  `project_name` argument.

- exclude_algos, include_algos:

  Vector of character strings. Algorithms to skip or include during the
  model-building phase. Set NULL to ignore. When both are defined, only
  `include_algos` will be valid.

- plots:

  Boolean. Create plots objects?

- alarm:

  Boolean. Ping (sound) when done. Requires `beepr`.

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- print:

  Boolean. Print summary when process ends?

- save:

  Boolean. Do you wish to save/export results into your working
  directory?

- subdir:

  Character. In which directory do you wish to save the results? Working
  directory as default.

- project:

  Character. Your project's name

- model_name:

  Character. Optional custom name for the model. If provided, this name
  will be used when saving the model with
  [`export_results()`](https://laresbernardo.github.io/lares/reference/export_results.md).
  If NULL (default), the H2O-generated model ID will be used.

- verbosity:

  Character. Verbosity of the backend messages printed during training.
  Must be one of NULL (live log disabled), "debug", "info", "warn",
  "error". Defaults to NULL.

- ...:

  Additional parameters on
  [`h2o::h2o.automl`](https://rdrr.io/pkg/h2o/man/h2o.automl.html)

- x:

  h2o_automl object

- importance:

  Boolean. Print important variables?

## Value

List. Trained model, predicted scores and datasets used, performance
metrics, parameters, importance data.frame, seed, and plots when
`plots=TRUE`.

## Details

For additional tutorials and examples:

- [Machine Learning with H2O
  Package](https://datascienceplus.com/machine-learning-with-r-h2o-package/)

- [Understanding ROC
  Curves](https://datascienceplus.com/understanding-roc-curves-with-lares/)

## List of algorithms

[-\> Read more
here](https://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html)

- DRF:

  Distributed Random Forest, including Random Forest (RF) and
  Extremely-Randomized Trees (XRT)

- GLM:

  Generalized Linear Model

- XGBoost:

  eXtreme Grading Boosting

- GBM:

  Gradient Boosting Machine

- DeepLearning:

  Fully-connected multi-layer artificial neural network

- StackedEnsemble:

  Stacked Ensemble

## Methods

- print:

  Use `print` method to print models stats and summary

- plot:

  Use `plot` method to plot results using
  [`mplot_full()`](https://laresbernardo.github.io/lares/reference/mplot_full.md)

## See also

Other Machine Learning:
[`ROC()`](https://laresbernardo.github.io/lares/reference/ROC.md),
[`conf_mat()`](https://laresbernardo.github.io/lares/reference/conf_mat.md),
[`export_results()`](https://laresbernardo.github.io/lares/reference/export_results.md),
[`gain_lift()`](https://laresbernardo.github.io/lares/reference/gain_lift.md),
[`h2o_predict_MOJO()`](https://laresbernardo.github.io/lares/reference/h2o_predict.md),
[`h2o_selectmodel()`](https://laresbernardo.github.io/lares/reference/h2o_selectmodel.md),
[`impute()`](https://laresbernardo.github.io/lares/reference/impute.md),
[`iter_seeds()`](https://laresbernardo.github.io/lares/reference/iter_seeds.md),
[`lasso_vars()`](https://laresbernardo.github.io/lares/reference/lasso_vars.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md),
[`model_preprocess()`](https://laresbernardo.github.io/lares/reference/model_preprocess.md),
[`msplit()`](https://laresbernardo.github.io/lares/reference/msplit.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# CRAN
data(dft) # Titanic dataset
dft <- subset(dft, select = -c(Ticket, PassengerId, Cabin))

# Classification: Binomial - 2 Classes
r <- h2o_automl(dft, y = Survived, max_models = 1, impute = FALSE, target = "TRUE", alarm = FALSE)

# Let's see all the stuff we have inside:
lapply(r, names)

# Classification: Multi-Categorical - 3 Classes
r <- h2o_automl(dft, Pclass, ignore = c("Fare", "Cabin"), max_time = 30, plots = FALSE)

# Regression: Continuous Values
r <- h2o_automl(dft, y = "Fare", ignore = c("Pclass"), exclude_algos = NULL, quiet = TRUE)
print(r)

# WITH PRE-DEFINED TRAIN/TEST DATAFRAMES
splits <- msplit(dft, size = 0.8)
splits$train$split <- "train"
splits$test$split <- "test"
df <- rbind(splits$train, splits$test)
r <- h2o_automl(df, "Survived", max_models = 1, train_test = "split")
} # }
```
