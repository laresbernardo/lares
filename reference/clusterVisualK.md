# Visualize K-Means Clusters for Several K

Visualize cluster data for assorted values of k.

## Usage

``` r
clusterVisualK(df, ks = 2:6, ...)
```

## Arguments

- df:

  Dataframe

- ks:

  Integer vector. Which k should be tested?

- ...:

  Additional parameters passed to `clusterKmeans`

## Value

List. Plot and data.frame results of clustering `df` data.frame into
`ks` integer clusters.

## See also

Other Clusters:
[`clusterKmeans()`](https://laresbernardo.github.io/lares/reference/clusterKmeans.md),
[`clusterOptimalK()`](https://laresbernardo.github.io/lares/reference/clusterOptimalK.md),
[`reduce_pca()`](https://laresbernardo.github.io/lares/reference/reduce_pca.md),
[`reduce_tsne()`](https://laresbernardo.github.io/lares/reference/reduce_tsne.md)

## Examples

``` r
Sys.unsetenv("LARES_FONT") # Temporal
data("iris")
df <- subset(iris, select = c(-Species))
df <- df[sample(nrow(df)), ]

# Calculate and plot
result <- clusterVisualK(df, ks = 2:4)
plot(result$plot)


# You can use the data generated as well
lapply(result$data, function(x) head(x$cluster, 10))
#> [[1]]
#>  [1] 1 2 1 1 2 2 2 2 2 2
#> Levels: 1 2
#> 
#> [[2]]
#>  [1] 2 1 2 2 1 3 3 1 3 1
#> Levels: 1 2 3
#> 
#> [[3]]
#>  [1] 2 4 2 2 4 3 3 4 1 4
#> Levels: 1 2 3 4
#> 
```
