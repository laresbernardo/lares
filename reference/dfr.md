# Results for AutoML Predictions

List with categorical (2 and 3 classes) and continuous predictions,
generated with
[`h2o_automl()`](https://laresbernardo.github.io/lares/reference/h2o_automl.md)
and the `dft`. Note that the models per se won't work to predict.

## Usage

``` r
data(dfr)
```

## Format

An object of class `"list"` with 3 `"data.frame"`

- class2:

  Predictions for a Binomial Classification Model

- class3:

  Predictions for a Multi-Categorical Classification Model

- regr:

  Predictions for a Continuous Regression Model

## Value

List

## See also

Other Dataset:
[`dft`](https://laresbernardo.github.io/lares/reference/dft.md)

## Examples

``` r
data(dfr)
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
```
