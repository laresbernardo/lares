# Calculate predictions of h2o Models

`h2o_predict_MOJO` lets the user predict using the h2o .zip file
containing the MOJO files. Note that it works with the files generated
when using the function export_results()

`h2o_predict_binary` lets the user predict using the h2o binary file.
Note that it works with the files generated when using the function
export_results(). Recommendation: use the h2o_predict_MOJO() function
when possible - it let's you change h2o's version without problem.

`h2o_predict_model` lets the user get scores from a H2O Model Object.

`h2o_predict_API` lets the user get the score from an API service

## Usage

``` r
h2o_predict_MOJO(df, model_path, method = "mojo", batch = 300)

h2o_predict_binary(df, model_path, sample = NA)

h2o_predict_model(df, model)

h2o_predict_API(df, api, exclude = "tag")
```

## Arguments

- df:

  Dataframe/Vector. Data to insert into the model.

- model_path:

  Character. Relative model path directory or zip file.

- method:

  Character. One of "mojo" or "json".

- batch:

  Integer. Run n batches at a time for "json" method.

- sample:

  Integer. How many rows should the function predict?

- model:

  h2o model Object

- api:

  Character. API URL.

- exclude:

  Character. Name of the variables to exclude.

## Value

data.frame with predicted results.

vector with predicted results.

data.frame with predicted results.

vector with predicted results.

## See also

Other Machine Learning:
[`ROC()`](https://laresbernardo.github.io/lares/reference/ROC.md),
[`conf_mat()`](https://laresbernardo.github.io/lares/reference/conf_mat.md),
[`export_results()`](https://laresbernardo.github.io/lares/reference/export_results.md),
[`gain_lift()`](https://laresbernardo.github.io/lares/reference/gain_lift.md),
[`h2o_automl()`](https://laresbernardo.github.io/lares/reference/h2o_automl.md),
[`h2o_selectmodel()`](https://laresbernardo.github.io/lares/reference/h2o_selectmodel.md),
[`impute()`](https://laresbernardo.github.io/lares/reference/impute.md),
[`iter_seeds()`](https://laresbernardo.github.io/lares/reference/iter_seeds.md),
[`lasso_vars()`](https://laresbernardo.github.io/lares/reference/lasso_vars.md),
[`model_metrics()`](https://laresbernardo.github.io/lares/reference/model_metrics.md),
[`model_preprocess()`](https://laresbernardo.github.io/lares/reference/model_preprocess.md),
[`msplit()`](https://laresbernardo.github.io/lares/reference/msplit.md)
