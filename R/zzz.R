.onLoad <- function(libname, pkgname){
  # Session stopwatch start
  tic(id = "Rsession")
  # To avoid data.table cannot be used without R package bit64 warning
  options("h2o.use.data.table" = FALSE)
  # So user can set another font be default on theme_lares2()
  if (is.null(getOption("lares.font")))
    options("lares.font" = "Arial Narrow")
  # Standard format for formatNum()
  options("lares.formatNum" = 2)
  # Scrabble and other language settings
  options("lares.lang" = "es")
}
