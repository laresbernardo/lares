# Visualize K-Means Clusters for Several K Methods

Visualize cluster data for assorted values of k and methods such as WSS,
Silhouette and Gap Statistic. See `factoextra::fviz_nbclust` for more.

## Usage

``` r
clusterOptimalK(
  df,
  method = c("wss", "silhouette", "gap_stat"),
  drop_na = TRUE,
  ohse = TRUE,
  norm = TRUE,
  quiet = TRUE,
  ...
)
```

## Arguments

- df:

  Dataframe

- method:

  Character vector.

- drop_na:

  Boolean. Should NA rows be removed?

- ohse:

  Boolean. Do you wish to automatically run one hot encoding to
  non-numerical columns?

- norm:

  Boolean. Should the data be normalized?

- quiet:

  Boolean. Keep quiet? If not, informative messages will be shown.

- ...:

  Additional parameters passed to `factoextra::fviz_nbclust`

## Value

Plot. Optimal number of clusters of `df` data.frame given a selected
`method`.

## See also

Other Clusters:
[`clusterKmeans()`](https://laresbernardo.github.io/lares/reference/clusterKmeans.md),
[`clusterVisualK()`](https://laresbernardo.github.io/lares/reference/clusterVisualK.md),
[`reduce_pca()`](https://laresbernardo.github.io/lares/reference/reduce_pca.md),
[`reduce_tsne()`](https://laresbernardo.github.io/lares/reference/reduce_tsne.md)

## Examples

``` r
# You must have "factoextra" library to use this auxiliary function:
if (FALSE) { # \dontrun{
data("iris")
df <- subset(iris, select = c(-Species))
# Calculate and plot optimal k clusters
clusterOptimalK(df)
} # }
```
