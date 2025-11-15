# Model Metrics and Performance

This function lets the user get a confusion matrix and accuracy, and for
for binary classification models: AUC, Precision, Sensitivity, and
Specificity, given the expected (tags) values and predicted values
(scores).

## Usage

``` r
model_metrics(
  tag,
  score,
  multis = NA,
  abc = TRUE,
  thresh = 10,
  auto_n = TRUE,
  thresh_cm = 0.5,
  target = "auto",
  type = "test",
  model_name = NA,
  plots = TRUE,
  quiet = FALSE,
  subtitle = NA
)
```

## Arguments

- tag:

  Vector. Real known label

- score:

  Vector. Predicted value or model's result

- multis:

  Data.frame. Containing columns with each category score (only used
  when more than 2 categories coexist)

- abc:

  Boolean. Arrange columns and rows alphabetically when categorical
  values?

- thresh:

  Integer. Threshold for selecting binary or regression models: this
  number is the threshold of unique values we should have in `'tag'`
  (more than: regression; less than: classification)

- auto_n:

  Add `n_` before digits when it's categorical and not numerical, even
  though seems numerical?

- thresh_cm:

  Numeric. Value to splits the results for the confusion matrix. Range
  of values: (0-1)

- target:

  Value. Which is your target positive value? If set to `'auto'`, the
  target with largest `mean(score)` will be selected. Change the value
  to overwrite. Only used when binary categorical model.

- type:

  Character. One of: "train", "test".

- model_name:

  Character. Model's name for reference.

- plots:

  Boolean. Create plots objects?

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- subtitle:

  Character. Subtitle for plots

## Value

List. Multiple performance metrics that vary depending on the type of
model (classification or regression). If `plot=TRUE`, multiple plots are
also returned.

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
[`model_preprocess()`](https://laresbernardo.github.io/lares/reference/model_preprocess.md),
[`msplit()`](https://laresbernardo.github.io/lares/reference/msplit.md)

Other Model metrics:
[`ROC()`](https://laresbernardo.github.io/lares/reference/ROC.md),
[`conf_mat()`](https://laresbernardo.github.io/lares/reference/conf_mat.md),
[`errors()`](https://laresbernardo.github.io/lares/reference/errors.md),
[`gain_lift()`](https://laresbernardo.github.io/lares/reference/gain_lift.md),
[`loglossBinary()`](https://laresbernardo.github.io/lares/reference/loglossBinary.md)

Other Calculus:
[`corr()`](https://laresbernardo.github.io/lares/reference/corr.md),
[`dist2d()`](https://laresbernardo.github.io/lares/reference/dist2d.md),
[`quants()`](https://laresbernardo.github.io/lares/reference/quants.md)

## Examples

``` r
data(dfr) # Results for AutoML Predictions
lapply(dfr, head)
#> $class2
#>     tag    scores
#> 1  TRUE 0.3155498
#> 2  TRUE 0.8747599
#> 3  TRUE 0.8952823
#> 4 FALSE 0.0436517
#> 5  TRUE 0.2196593
#> 6 FALSE 0.2816101
#> 
#> $class3
#>   tag score        n_1        n_2        n_3
#> 1 n_3   n_2 0.20343865 0.60825062 0.18831071
#> 2 n_2   n_3 0.17856154 0.07657769 0.74486071
#> 3 n_1   n_1 0.50516951 0.40168718 0.09314334
#> 4 n_3   n_2 0.30880713 0.39062151 0.30057135
#> 5 n_2   n_3 0.01956827 0.07069011 0.90974158
#> 6 n_2   n_3 0.07830017 0.15408720 0.76761264
#> 
#> $regr
#>       tag    score
#> 1 11.1333 25.93200
#> 2 30.0708 39.91900
#> 3 26.5500 50.72246
#> 4 31.2750 47.81292
#> 5 13.0000 30.12853
#> 6 26.0000 13.24153
#> 

# Metrics for Binomial Model
met1 <- model_metrics(dfr$class2$tag, dfr$class2$scores,
  model_name = "Titanic Survived Model",
  plots = FALSE
)
#> Target value: TRUE
print(met1)
#> $dictionary
#> [1] "AUC: Area Under the Curve"                                                             
#> [2] "ACC: Accuracy"                                                                         
#> [3] "PRC: Precision = Positive Predictive Value"                                            
#> [4] "TPR: Sensitivity = Recall = Hit rate = True Positive Rate"                             
#> [5] "TNR: Specificity = Selectivity = True Negative Rate"                                   
#> [6] "Logloss (Error): Logarithmic loss [Neutral classification: 0.69315]"                   
#> [7] "Gain: When best n deciles selected, what % of the real target observations are picked?"
#> [8] "Lift: When best n deciles selected, how much better than random is?"                   
#> 
#> $confusion_matrix
#>        Pred
#> Real    FALSE TRUE
#>   FALSE     9  156
#>   TRUE     68   35
#> 
#> $gain_lift
#> # A tibble: 10 × 10
#>    percentile value random target total  gain optimal  lift response score
#>    <fct>      <fct>  <dbl>  <int> <int> <dbl>   <dbl> <dbl>    <dbl> <dbl>
#>  1 1          TRUE    10.4     26    28  25.2    27.2 142.     25.2  93.3 
#>  2 2          TRUE    20.5     24    27  48.5    53.4 137.     23.3  79.7 
#>  3 3          TRUE    30.2     19    26  67.0    78.6 122.     18.4  46.2 
#>  4 4          TRUE    39.9     12    26  78.6   100    97.0    11.7  31.5 
#>  5 5          TRUE    50        9    27  87.4   100    74.8     8.74 19.8 
#>  6 6          TRUE    60.1      5    27  92.2   100    53.5     4.85 13.3 
#>  7 7          TRUE    69.8      3    26  95.1   100    36.4     2.91  9.31
#>  8 8          TRUE    79.9      2    27  97.1   100    21.6     1.94  7.49
#>  9 9          TRUE    89.9      3    27 100     100    11.2     2.91  6.14
#> 10 10         TRUE   100        0    27 100     100     0       0     3.19
#> 
#> $metrics
#>       AUC     ACC     PRC     TPR      TNR
#> 1 0.89467 0.16418 0.18325 0.33981 0.054545
#> 

# Metrics for Multi-Categorical Model
met2 <- model_metrics(dfr$class3$tag, dfr$class3$score,
  multis = subset(dfr$class3, select = -c(tag, score)),
  model_name = "Titanic Class Model",
  plots = FALSE
)
print(met2)
#> $dictionary
#> [1] "AUC: Area Under the Curve"                                                             
#> [2] "ACC: Accuracy"                                                                         
#> [3] "PRC: Precision = Positive Predictive Value"                                            
#> [4] "TPR: Sensitivity = Recall = Hit rate = True Positive Rate"                             
#> [5] "TNR: Specificity = Selectivity = True Negative Rate"                                   
#> [6] "Logloss (Error): Logarithmic loss [Neutral classification: 0.69315]"                   
#> [7] "Gain: When best n deciles selected, what % of the real target observations are picked?"
#> [8] "Lift: When best n deciles selected, how much better than random is?"                   
#> 
#> $confusion_matrix
#> # A tibble: 3 × 4
#>   `Real x Pred`   n_3   n_1   n_2
#>   <fct>         <int> <int> <int>
#> 1 n_3             120    11    18
#> 2 n_1              12    43     8
#> 3 n_2              26    15    15
#> 
#> $metrics
#>      AUC     ACC
#> 1 0.7896 0.66418
#> 
#> $metrics_tags
#> # A tibble: 3 × 9
#>   tag       n     p   AUC order   ACC   PRC   TPR   TNR
#>   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 n_3     149  55.6 0.826     1 0.75  0.759 0.805 0.681
#> 2 n_1      63  23.5 0.867     2 0.828 0.623 0.683 0.873
#> 3 n_2      56  20.9 0.675     3 0.75  0.366 0.268 0.877
#> 

# Metrics for Regression Model
met3 <- model_metrics(dfr$regr$tag, dfr$regr$score,
  model_name = "Titanic Fare Model",
  plots = FALSE
)
print(met3)
#> $dictionary
#> [1] "RMSE: Root Mean Squared Error"       
#> [2] "MAE: Mean Average Error"             
#> [3] "MAPE: Mean Absolute Percentage Error"
#> [4] "MSE: Mean Squared Error"             
#> [5] "RSQ: R Squared"                      
#> [6] "RSQA: Adjusted R Squared"            
#> 
#> $metrics
#>       rmse      mae       mape      mse    rsq   rsqa
#> 1 20.30881 14.24359 0.07303959 412.4477 0.3169 0.3143
#> 
```
