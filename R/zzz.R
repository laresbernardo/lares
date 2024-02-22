.onLoad <- function(libname, pkgname) {
  Sys.setenv(
    # So user can set another font by default on theme_lares()
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
    },
    # ChatGPT Default values
    "LARES_GPT_MODEL" = if (Sys.getenv("LARES_GPT_MODEL") != "") {
      Sys.getenv("LARES_GPT_MODEL")
    } else {
      "gpt-3.5-turbo"
    },
    "LARES_GPT_URL" = if (Sys.getenv("LARES_GPT_URL") != "") {
      Sys.getenv("LARES_GPT_URL")
    } else {
      "https://api.openai.com/v1/chat/completions"
    },
    "LARES_GEMINI_API" = if (Sys.getenv("LARES_GEMINI_API") != "") {
      Sys.getenv("LARES_GEMINI_API")
    } else {
      "https://generativelanguage.googleapis.com/v1beta/models/"
    }
  )
  options(
    # Additional default options for other libraries
    "getSymbols.warning4.0" = FALSE,
    "getSymbols.yahoo.warning" = FALSE,
    "h2o.use.data.table" = FALSE, # Avoid data.table warning: cannot be used without bit64
    "lares.colours" = c(
      orange = "#EA9A28",
      blue = "#00B1DA",
      purple = "#6A2C70",
      red = "#E63946",
      green = "#00C9AE",
      navy = "#125D98",
      yellow = "#F8D962",
      grey = "#8D99AE",
      pink = "#F6B8B8",
      fuchsia = "#FF2EB5",
      brown = "#884F3E",
      black = "#0c0c0c",
      white = "#F6F5F5"
    ),
    "lares.palette" = c(
      "#EA9A28" = "#000000",
      "#00B1DA" = "#000000",
      "#6A2C70" = "#FFFFFF",
      "#E63946" = "#000000",
      "#00C9AE" = "#000000",
      "#F8D962" = "#000000",
      "#D35FB7" = "#000000",
      "#125D98" = "#FFFFFF",
      "#F6B8B8" = "#000000",
      "#8D99AE" = "#000000",
      "#290452" = "#FFFFFF",
      "#7ADf90" = "#000000",
      "#2FFECC" = "#000000",
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
    )
  )
}
