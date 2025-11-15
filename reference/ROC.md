# AUC and ROC Curves Data

This function calculates ROC Curves and AUC values with 95% confidence
range. It also works for multi-categorical models.

## Usage

``` r
ROC(tag, score, multis = NA)
```

## Arguments

- tag:

  Vector. Real known label

- score:

  Vector. Predicted value or model's result

- multis:

  Data.frame. Containing columns with each category score (only used
  when more than 2 categories coexist)

## Value

List with ROC's results, area under the curve (AUC) and their CI.

## Plot Results

To plot results, use the
[`mplot_roc()`](https://laresbernardo.github.io/lares/reference/mplot_roc.md)
function.

## See also

Other Machine Learning:
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
[`model_preprocess()`](https://laresbernardo.github.io/lares/reference/model_preprocess.md),
[`msplit()`](https://laresbernardo.github.io/lares/reference/msplit.md)

Other Model metrics:
[`conf_mat()`](https://laresbernardo.github.io/lares/reference/conf_mat.md),
[`errors()`](https://laresbernardo.github.io/lares/reference/errors.md),
[`gain_lift()`](https://laresbernardo.github.io/lares/reference/gain_lift.md),
[`loglossBinary()`](https://laresbernardo.github.io/lares/reference/loglossBinary.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md)

## Examples

``` r
data(dfr) # Results for AutoML Predictions
lapply(dfr[c(1, 2)], head)
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

# ROC Data for Binomial Model
roc1 <- ROC(dfr$class2$tag, dfr$class2$scores)
lapply(roc1, head)
#> $ci
#>        roc.ci
#> min 0.8542079
#> AUC 0.8946749
#> max 0.9351419
#> 
#> $roc
#>   fpr         tpr label
#> 1   1 0.000000000 2cats
#> 2   1 0.009708738 2cats
#> 3   1 0.019417476 2cats
#> 4   1 0.029126214 2cats
#> 5   1 0.038834951 2cats
#> 6   1 0.048543689 2cats
#> 

# ROC Data for Multi-Categorical Model
roc2 <- ROC(dfr$class3$tag, dfr$class3$score,
  multis = subset(dfr$class3, select = -c(tag, score))
)
lapply(roc2, head)
#> $ci
#>           n_1       n_2       n_3      mean
#> min 0.8198601 0.6018566 0.7774864 0.7330677
#> AUC 0.8669377 0.6754127 0.8264621 0.7896042
#> max 0.9140153 0.7489689 0.8754379 0.8461407
#> 
#> $roc
#>   fpr         tpr        label
#> 1   1 0.000000000 86.69% | n_1
#> 2   1 0.004878049 86.69% | n_1
#> 3   1 0.009756098 86.69% | n_1
#> 4   1 0.019512195 86.69% | n_1
#> 5   1 0.024390244 86.69% | n_1
#> 6   1 0.029268293 86.69% | n_1
#> 
#> $rocs
#> $rocs$n_1
#> 
#> Call:
#> roc.default(response = label, predictor = res, quiet = TRUE,     ci = TRUE)
#> 
#> Data: res in 63 controls (label n_1) > 205 cases (label other_label).
#> Area under the curve: 0.8669
#> 95% CI: 0.8199-0.914 (DeLong)
#> 
#> $rocs$n_2
#> 
#> Call:
#> roc.default(response = label, predictor = res, quiet = TRUE,     ci = TRUE)
#> 
#> Data: res in 56 controls (label n_2) > 212 cases (label other_label).
#> Area under the curve: 0.6754
#> 95% CI: 0.6019-0.749 (DeLong)
#> 
#> $rocs$n_3
#> 
#> Call:
#> roc.default(response = label, predictor = res, quiet = TRUE,     ci = TRUE)
#> 
#> Data: res in 149 controls (label n_3) > 119 cases (label other_label).
#> Area under the curve: 0.8265
#> 95% CI: 0.7775-0.8754 (DeLong)
#> 
#> 
```
