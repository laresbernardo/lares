####################################################################
#' lares Theme for ggplot2
#' 
#' This function sets some default values into ggplot2 outputs. This
#' function is almost as using gg_colour_customs() + gg_fill_customs() 
#' + gg_text_customs, but these won't be mantained any longer.
#' 
#' @param labels Boolean. Add labels to plot?
#' @param colours Boolean. Personalize colour palettes?
#' @param cont Boolean. Is the value continuous? Discrete by default
#' @param xcommas Boolean. Nice format for x continuous values?
#' @param ycommas Boolean. Nice format for y continuous values?
#' @export
theme_lares <- function(labels = FALSE, colours = TRUE, cont = FALSE,
                        xcommas = FALSE, ycommas = FALSE) {
  
  r <- list(theme_minimal())
  
  if (labels) {
    r <- c(r, geom_label(show.legend = FALSE, size = 3))
  }
  
  if (xcommas) {
    r <- c(r, scale_x_continuous(labels = comma))
  }
  if (ycommas) {
    r <- c(r, scale_y_continuous(labels = comma))
  }
  
  if (colours) {
    red <- "tomato"
    green <- "#3DA4AB" 
    pink <- "lightpink2"
    blue <- "#0E9AA7"
    orange <- "#FE8A71"
    black <- "#000000"
    white <- "#FFFFFF"
    grey <- "azure4"
    megblue <- "#071D49"
    megyellow <- "#EBB600"
    meggrey1 <- "#F2F1F0"
    meggrey2 <- "#9A9A9A"
    
    colours_list <- data.frame(rbind(
      c("allianz", "#0038A8", black),
      c("equidad", "#52CF44", black),
      c("colpatria", "#EE0606", black),
      c("del estado", "#F37000", black),
      c("suramericana", "#1F6D8C", black),
      c("mapfre", "#34000D", black),
      c("la previsora", "#6F9A45", black),
      c("aig", "#C71585", black),
      c("generali", "#B21F1F", black),
      c("solidaria", "#E69500", black),
      c("liberty", "#4E629A", black),
      c("bolivar", "#F0F206", black),
      c("cia", "#8ACBE5", black),
      c("mundial", "#8ACBE5", black),
      c("puntored", "#FFFF00", grey),
      c("movilred", "#FF1493", black),
      c("moviired", "#FF1493", black),
      c("web", "#290452", white),
      c("somosf1", "#290452", white),
      c("f1", "#290452", white),
      c("funnel-soat4_desktop", "#290452", white),
      c("funnel-ujk2d_desktop", "#8ACBE5", black),
      c("funnel-ujk2m_old_mobile", "#7AC4E1", black),
      c("red", grey, black),
      c("m", blue, white),
      c("f", pink, black),
      c("male", blue, white),
      c("female", pink, black),
      c("hombre", blue, white),
      c("mujer", pink, black),
      c("true", green, black),
      c("false", red, black),
      c("TRUE", green, black),
      c("FALSE", red, black),
      c("1", green, black),
      c("0", red, black),
      c("good", green, black),
      c("bad", red, black),
      c("bueno", green, black),
      c("malo", red, black),
      c("spring", green, white),
      c("summer", red, white),
      c("fall", orange, black),
      c("winter", blue, black),
      c("meg1", megblue, white),
      c("meg2", megyellow, black),
      c("meg3", meggrey1, black),
      c("meg4", meggrey2, white)
    ))
    colnames(colours_list) <- c("values","fill","colour")
    
    scale_fill_lares <- function(){
      values <- as.character(t(colours_list$fill)[1,])
      names(values) <- colours_list$values
      structure(list(scale_fill_manual(values = values)))
    }
    
    scale_colour_lares <- function(){
      values <- as.character(t(colours_list$colour)[1,])
      names(values) <- colours_list$values
      structure(list(scale_color_manual(values = values)))
    }
    
    if (cont) {
      scale_colour_lares <- function(){
        # https://www.color-hex.com/color-palette/69673
        pal_lares <- c("#817b7b", "#0e9aa7", "#3da4ab", "#f6cd61", "#fe8a71")
        structure(list(scale_color_gradientn(colours = pal_lares)))
      } 
    }
    
    r <- c(r, scale_fill_lares(), scale_colour_lares())
  }
  
  return(r)
  
}


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
              "funnel-soat4_desktop" = "#290452",
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
              "funnel-soat4_desktop" = "#290452",
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
              "funnel-soat4_desktop" = white,
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
              "b" = black,
              "none" = black)
  return(scale_color_manual(values = values))
}
