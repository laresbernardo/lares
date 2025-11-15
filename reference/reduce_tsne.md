# Reduce Dimensionality with t-SNE

t-SNE takes high-dimensional data and reduces it to a low-dimensional
graph (1-3 dimensions). Unlike PCA, t-SNE can reduce dimensions with
non-linear relationships. PCA attempts to draw the best fitting line
through the distribution. T-SNE calculates a similarity measure based on
the distance between points instead of trying to maximize variance.

## Usage

``` r
reduce_tsne(df, n = 2, ignore = NULL, quiet = FALSE, plot = TRUE, ...)
```

## Arguments

- df:

  Dataframe

- n:

  Integer. Number of dimensions to reduce to.

- ignore:

  Character vector. Names of columns to ignore.

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- plot:

  Boolean. Create plots?

- ...:

  Additional parameters passed to `Rtsne::Rtsne`

## Value

List with reduced dataframe and possible plots.

## See also

Other Dimensionality:
[`reduce_pca()`](https://laresbernardo.github.io/lares/reference/reduce_pca.md)

Other Clusters:
[`clusterKmeans()`](https://laresbernardo.github.io/lares/reference/clusterKmeans.md),
[`clusterOptimalK()`](https://laresbernardo.github.io/lares/reference/clusterOptimalK.md),
[`clusterVisualK()`](https://laresbernardo.github.io/lares/reference/clusterVisualK.md),
[`reduce_pca()`](https://laresbernardo.github.io/lares/reference/reduce_pca.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data("iris")
df <- subset(iris, select = c(-Species))
df$id <- seq_len(nrow(df))
reduce_tsne(df, ignore = "id", max_iter = 800, perplexity = 20)
} # }
```
