# Impute Missing Values (using MICE)

This function uses the MICE methodology to impute missing values.

## Usage

``` r
impute(df, m = 5, iters = 5, seed = 0, quiet = FALSE)
```

## Arguments

- df:

  Dataframe. Dataframe to transform.

- m:

  Integer. Number of multiple imputations.

- iters:

  Integer. Number of iterations.

- seed:

  Integer. Set a seed for reproducibility.

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

## Value

data.frame with imputed values.

## See also

Other Data Wrangling:
[`balance_data()`](https://laresbernardo.github.io/lares/reference/balance_data.md),
[`categ_reducer()`](https://laresbernardo.github.io/lares/reference/categ_reducer.md),
[`cleanText()`](https://laresbernardo.github.io/lares/reference/clean_text.md),
[`date_cuts()`](https://laresbernardo.github.io/lares/reference/date_cuts.md),
[`date_feats()`](https://laresbernardo.github.io/lares/reference/date_feats.md),
[`file_name()`](https://laresbernardo.github.io/lares/reference/file_name.md),
[`formatHTML()`](https://laresbernardo.github.io/lares/reference/format_string.md),
[`holidays()`](https://laresbernardo.github.io/lares/reference/holidays.md),
[`left()`](https://laresbernardo.github.io/lares/reference/left_right.md),
[`normalize()`](https://laresbernardo.github.io/lares/reference/normalize.md),
[`num_abbr()`](https://laresbernardo.github.io/lares/reference/num_abbr.md),
[`ohe_commas()`](https://laresbernardo.github.io/lares/reference/ohe_commas.md),
[`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md),
[`quants()`](https://laresbernardo.github.io/lares/reference/quants.md),
[`removenacols()`](https://laresbernardo.github.io/lares/reference/filterdata.md),
[`replaceall()`](https://laresbernardo.github.io/lares/reference/replaceall.md),
[`replacefactor()`](https://laresbernardo.github.io/lares/reference/replacefactor.md),
[`textFeats()`](https://laresbernardo.github.io/lares/reference/textFeats.md),
[`textTokenizer()`](https://laresbernardo.github.io/lares/reference/textTokenizer.md),
[`vector2text()`](https://laresbernardo.github.io/lares/reference/vector2text.md),
[`year_month()`](https://laresbernardo.github.io/lares/reference/year_month.md),
[`zerovar()`](https://laresbernardo.github.io/lares/reference/zerovar.md)

Other Machine Learning:
[`ROC()`](https://laresbernardo.github.io/lares/reference/ROC.md),
[`conf_mat()`](https://laresbernardo.github.io/lares/reference/conf_mat.md),
[`export_results()`](https://laresbernardo.github.io/lares/reference/export_results.md),
[`gain_lift()`](https://laresbernardo.github.io/lares/reference/gain_lift.md),
[`h2o_automl()`](https://laresbernardo.github.io/lares/reference/h2o_automl.md),
[`h2o_predict_MOJO()`](https://laresbernardo.github.io/lares/reference/h2o_predict.md),
[`h2o_selectmodel()`](https://laresbernardo.github.io/lares/reference/h2o_selectmodel.md),
[`iter_seeds()`](https://laresbernardo.github.io/lares/reference/iter_seeds.md),
[`lasso_vars()`](https://laresbernardo.github.io/lares/reference/lasso_vars.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md),
[`model_preprocess()`](https://laresbernardo.github.io/lares/reference/model_preprocess.md),
[`msplit()`](https://laresbernardo.github.io/lares/reference/msplit.md)

Other Missing Values:
[`missingness()`](https://laresbernardo.github.io/lares/reference/missingness.md)
