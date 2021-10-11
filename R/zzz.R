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
      "filled"
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
    "h2o.use.data.table" = FALSE, # Avoid data.table warning: cannot be used without bit64
    "lares.palette" = c(
      "#FF8303" = "#000000",
      "#40A4D8" = "#000000",
      "#5D3A9B" = "#FFFFFF",
      "#E63946" = "#000000",
      "#2A9D8F" = "#000000",
      "#D35FB7" = "#000000",
      "#F8D962" = "#000000",
      "#03396C" = "#FFFFFF",
      "#F29595" = "#000000",
      "#2FFECC" = "#000000",
      "#8D99AE" = "#000000",
      "#7ADf90" = "#000000",
      "#290452" = "#FFFFFF",
      "#0C7BDC" = "#000000",
      "#817B7B" = "#000000",
      "#F66320" = "#000000",
      "#F4A261" = "#000000",
      "#005AB5" = "#FFFFFF",
      "#9A9A9A" = "#000000",
      "#00008B" = "#FFFFFF",
      "#E1BE6A" = "#000000",
      "#40B0A6" = "#000000",
      "#056E00" = "#FFFFFF",
      "#E40000" = "#000000",
      "#FE8A71" = "#000000",
      "#8600A1" = "#FFFFFF",
      "#A52A2A" = "#FFFFFF",
      "#000000" = "#FFFFFF",
      "#E69F00" = "#000000",
      "#009E73" = "#000000",
      "#0072B2" = "#FFFFFF",
      "#D55E00" = "#000000"
    ),
    "lares.colours" = c(
      orange = "#FF8303",
      blue = "#40A4D8",
      purple = "#5D3A9B",
      red = "#E63946",
      green = "#A1BD4D",
      navy = "#03396C",
      yellow = "#F8D962",
      grey = "#8D99AE",
      pink = "#FFCAD4",
      black = "#000000",
      white = "#F8F8F8"
    )
  )
}
