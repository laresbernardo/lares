.onLoad <- function(libname, pkgname) {
  # Old options: lares.font, lares.formatNum, lares.lang
  Sys.setenv(
    # So user can set another font be default on theme_lares()
    "LARES_FONT" = if (Sys.getenv("LARES_FONT") != "") {
      Sys.getenv("LARES_FONT")
    } else {
      "Arial Narrow"
    },
    # Standard format for formatNum()
    "LARES_NUMFORMAT" = if (Sys.getenv("LARES_NUMFORMAT") != "") {
      Sys.getenv("LARES_NUMFORMAT")
    } else {
      2
    },
    # Status / Loading bar style for statusbar()
    "LARES_STATUSBAR" = if (Sys.getenv("LARES_STATUSBAR") != "") {
      Sys.getenv("LARES_STATUSBAR")
    } else {
      "sword"
    },
    # Scrabble and other language settings
    "LARES_LANG" = if (Sys.getenv("LARES_LANG") != "") {
      Sys.getenv("LARES_LANG")
    } else {
      "es"
    }
  )
  options(
    # Additional default options for other libraries
    "getSymbols.warning4.0" = FALSE,
    "getSymbols.yahoo.warning" = FALSE,
    "h2o.use.data.table" = FALSE # Avoid data.table warning: cannot be used without bit64
  )
}
