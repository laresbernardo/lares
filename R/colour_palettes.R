####################################################################
#' Custom colours to use in ggplot as scale_color_manual
#' 
#' This function lets the user use pre-defined default colours
#' 
#' @export
gg_colour_customs <- function () {
  
  values <- c("allianz"="#0038A8",
              "equidad"="#52CF44",
              "colpatria"="#EE0606",
              "del estado"="#F37000",
              "suramericana"="#1F6D8C",
              "mapfre"="#34000D",
              "la previsora"="#6F9A45",
              "aig"="#C71585",
              "generali"="#B21F1F",
              "solidaria"="#E69500",
              "liberty"="#4E629A",
              "bolivar"="#F0F206",
              "cia"="#8ACBE5",
              "puntored"="#FFFF00",
              "movilred"="#FF1493",
              "web"="#290452",
              "f1"="#290452",
              "m"="steelblue2",
              "f"="lightpink2",
              "true"="springgreen3",
              "false"="red2",
              "1"="springgreen3",
              "0"="red2",
              "good"="springgreen3",
              "bad"="red2",
              "bueno"="springgreen3",
              "malo"="red2",
              "buenos"="springgreen3",
              "malos"="red2",
              "spring"="springgreen3",
              "summer"="red2",
              "fall"="orange",
              "winter"="steelblue2")
  return(scale_color_manual(values = values))
}


####################################################################
#' Custom colours to use in ggplot as scale_fill_manual
#' 
#' This function lets the user use pre-defined default colours
#' 
#' @export
gg_fill_customs <- function () {
  
  values <- c("allianz"="#0038A8",
              "equidad"="#52CF44",
              "colpatria"="#EE0606",
              "del estado"="#F37000",
              "suramericana"="#1F6D8C",
              "mapfre"="#34000D",
              "la previsora"="#6F9A45",
              "aig"="#C71585",
              "generali"="#B21F1F",
              "solidaria"="#E69500",
              "liberty"="#4E629A",
              "bolivar"="#F0F206",
              "cia"="#8ACBE5",
              "puntored"="#FFFF00",
              "movilred"="#FF1493",
              "web"="#290452",
              "f1"="#290452",
              "m"="steelblue2",
              "f"="lightpink2",
              "true"="springgreen3",
              "false"="red2",
              "1"="springgreen3",
              "0"="red2",
              "good"="springgreen3",
              "bad"="red2",
              "bueno"="springgreen3",
              "malo"="red2",
              "buenos"="springgreen3",
              "malos"="red2",
              "spring"="springgreen3",
              "summer"="red2",
              "fall"="orange",
              "winter"="steelblue2")
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
              "f1" = white,
              "m" = white,
              "f" = black,
              "true" = black,
              "false" = black,
              "1" = black,
              "0" = black,
              "good" = black,
              "bad" = black,
              "bueno" = black,
              "malo" = black,
              "spring" = white,
              "summer" = white,
              "fall" = black,
              "winter" = black)
  return(scale_color_manual(values = values))
}
