.onLoad <- function(libname, pkgname){
  # Session stopwatch start
  tic(id = "Rsession")
  options(
    # So user can set another font be default on theme_lares2()
    "lares.font" = if (is.null(getOption("lares.font"))) "Arial Narrow" else NULL,
    # Standard format for formatNum()
    "lares.formatNum" = 2,
    # Scrabble and other language settings
    "lares.lang" = "es",
    # Additional default options for other libraries
    "getSymbols.warning4.0" = FALSE,
    "getSymbols.yahoo.warning" = FALSE,
    "h2o.use.data.table" = FALSE # Avoid data.table warning: cannot be used without bit64
    )
}
