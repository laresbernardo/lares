####################################################################
#' Custom colours to use in ggplot as scale_color_manual
#' 
#' This function lets the user use pre-defined default colours
#' 
#' @export
gg_colour_customs <- function () {
  
  red <- "tomato"
  green <- "mediumspringgreen"
  pink <- "lightpink2"
  blue <- "steelblue2"
  orange <- "orange"
  
  values <- c("allianz" = "#0038A8",
              "equidad" = "#52CF44",
              "colpatria" = "#EE0606",
              "del estado" = "#F37000",
              "suramericana" = "#1F6D8C",
              "mapfre" = "#34000D",
              "la previsora" = "#6F9A45",
              "aig" = "#C71585",
              "generali" = "#B21F1F",
              "solidaria" = "#E69500",
              "liberty" = "#4E629A",
              "bolivar" = "#F0F206",
              "cia" = "#8ACBE5",
              "puntored" = "#FFFF00",
              "movilred" = "#FF1493",
              "web" = "#290452",
              "red" = "azure4",
              "f1" = "#290452",
              "m" = blue,
              "f" = pink,
              "true" = green,
              "false" = red,
              "TRUE" = green,
              "FALSE" = red,
              "1" = green,
              "0" = red,
              "good" = green,
              "bad" = red,
              "bueno" = green,
              "malo" = red,
              "buenos" = green,
              "malos" = red,
              "spring" = green,
              "summer" = red,
              "fall" = orange,
              "winter" = blue)
  return(scale_color_manual(values = values))
}


####################################################################
#' Custom colours to use in ggplot as scale_fill_manual
#' 
#' This function lets the user use pre-defined default colours
#' 
#' @export
gg_fill_customs <- function () {
  
  red <- "tomato"
  green <- "mediumspringgreen"
  pink <- "lightpink2"
  blue <- "steelblue2"
  orange <- "orange"
  
  values <- c("allianz" = "#0038A8",
              "equidad" = "#52CF44",
              "colpatria" = "#EE0606",
              "del estado" = "#F37000",
              "suramericana" = "#1F6D8C",
              "mapfre" = "#34000D",
              "la previsora" = "#6F9A45",
              "aig" = "#C71585",
              "generali" = "#B21F1F",
              "solidaria" = "#E69500",
              "liberty" = "#4E629A",
              "bolivar" = "#F0F206",
              "cia" = "#8ACBE5",
              "puntored" = "#FFFF00",
              "movilred" = "#FF1493",
              "web" = "#290452",
              "red" = "azure4",
              "f1" = "#290452",
              "m" = blue,
              "f" = pink,
              "true" = green,
              "false" = red,
              "TRUE" = green,
              "FALSE" = red,
              "1" = green,
              "0" = red,
              "good" = green,
              "bad" = red,
              "bueno" = green,
              "malo" = red,
              "buenos" = green,
              "malos" = red,
              "spring" = green,
              "summer" = red,
              "fall" = orange,
              "winter" = blue)
  return(scale_fill_manual(values = values))
}


####################################################################
#' Custom colours to use in ggplot as scale_color_manual on texts
#' 
#' This function lets the user use pre-defined default colours
#' 
#' @export
gg_text_customs <- function () {
  
  black <- "#000000"
  white <- "#FFFFFF"
  
  values <- c("allianz" = black,
              "equidad" = black,
              "colpatria" = black,
              "del estado" = black,
              "suramericana" = black,
              "mapfre" = black,
              "la previsora" = black,
              "aig" = black,
              "generali" = black,
              "solidaria" = black,
              "liberty" = black,
              "bolivar" = black,
              "cia" = black,
              "puntored" = black,
              "movilred" = black,
              "web" = white,
              "red" = black,
              "f1" = white,
              "m" = white,
              "f" = black,
              "true" = black,
              "false" = black,
              "TRUE"= black,
              "FALSE"= black,
              "1" = black,
              "0" = black,
              "good" = black,
              "bad" = black,
              "bueno" = black,
              "malo" = black,
              "spring" = white,
              "summer" = white,
              "fall" = black,
              "winter" = black,
              "w" = white,
              "b" = black)
  return(scale_color_manual(values = values))
}
