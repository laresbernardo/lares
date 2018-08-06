####################################################################
#' Custom colours to use in ggplot as scale_color_manual
#' 
#' This function lets the user use some default colours
#' 
#' @export
gg_colour_customs <- function () {
  
  suppressMessages(require(ggplot2))
  
  values <- c("ALLIANZ"="#0038A8",
              "EQUIDAD"="#52CF44",
              "COLPATRIA"="#EE0606",
              "DEL ESTADO"="#F37000",
              "SURAMERICANA"="#1F6D8C",
              "MAPFRE"="#34000D",
              "LA PREVISORA"="#6F9A45",
              "AIG"="#C71585",
              "GENERALI"="#B21F1F",
              "SOLIDARIA"="#E69500",
              "LIBERTY"="#4E629A",
              "BOLIVAR"="#F0F206",
              "CIA"="#8ACBE5",
              "puntored"="#FFFF00",
              "movilred"="#FF1493",
              "web"="#FFFFFF",
              "f1"="#FFFFFF",
              "M"="steelblue2",
              "F"="lightpink2",
              "TRUE"="springgreen3",
              "FALSE"="red2",
              "1"="springgreen3",
              "0"="red2",
              "good"="springgreen3",
              "bad"="red2",
              "bueno"="springgreen3",
              "malo"="red2")
  return(scale_color_manual(values=values))
}


####################################################################
#' Custom colours to use in ggplot as scale_color_manual on texts
#' 
#' This function lets the user use some default colours
#' 
#' @export
gg_text_customs <- function () {
  
  suppressMessages(require(ggplot2))
  
  values <- c("ALLIANZ"="#000000",
              "EQUIDAD"="#000000",
              "COLPATRIA"="#000000",
              "DEL ESTADO"="#000000",
              "SURAMERICANA"="#000000",
              "MAPFRE"="#000000",
              "LA PREVISORA"="#000000",
              "AIG"="#000000",
              "GENERALI"="#000000",
              "SOLIDARIA"="#000000",
              "LIBERTY"="#000000",
              "BOLIVAR"="#000000",
              "CIA"="#000000",
              "puntored"="#000000",
              "movilred"="#000000",
              "web"="#FFFFFF",
              "f1"="#FFFFFF",
              "M"="steelblue2",
              "F"="lightpink2",
              "TRUE"="springgreen3",
              "FALSE"="red2",
              "1"="springgreen3",
              "0"="red2",
              "good"="springgreen3",
              "bad"="red2",
              "bueno"="springgreen3",
              "malo"="red2")
  return(scale_color_manual(values=values))
}


####################################################################
#' Custom colours to use in ggplot as scale_fill_manual
#' 
#' This function lets the user use some default colours
#' 
#' @export
gg_fill_customs <- function () {
  
  suppressMessages(require(ggplot2))
  
  values <- c("ALLIANZ"="#0038A8",
              "EQUIDAD"="#52CF44",
              "COLPATRIA"="#EE0606",
              "DEL ESTADO"="#F37000",
              "SURAMERICANA"="#1F6D8C",
              "MAPFRE"="#34000D",
              "LA PREVISORA"="#6F9A45",
              "AIG"="#C71585",
              "GENERALI"="#B21F1F",
              "SOLIDARIA"="#E69500",
              "LIBERTY"="#4E629A",
              "BOLIVAR"="#F0F206",
              "CIA"="#8ACBE5",
              "puntored"="#FFFF00",
              "movilred"="#FF1493",
              "web"="#290452",
              "f1"="#FFFFFF",
              "M"="steelblue2",
              "F"="lightpink2",
              "TRUE"="springgreen3",
              "FALSE"="red2",
              "1"="springgreen3",
              "0"="red2",
              "good"="springgreen3",
              "bad"="red2",
              "bueno"="springgreen3",
              "malo"="red2")
  return(scale_fill_manual(values=values))
}
