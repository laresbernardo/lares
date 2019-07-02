####################################################################
#' Personal Colours Palette
#' 
#' This function plots a list of colours on a specific palette
#' 
#' @family Auxiliary
#' @export
lares_pal <- function() {
  
  black <- "#000000"
  white <- "#FFFFFF"
  red <- "tomato"
  green <- "#3DA4AB" 
  pink <- "lightpink2"
  blue <- "#0E9AA7"
  orange <- "#FE8A71"
  grey <- "azure4"
  
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
    c("red", black, black),
    c("m", blue, white),
    c("f", pink, black),
    c("male", blue, white),
    c("female", pink, black),
    c("hombre", blue, white),
    c("mujer", pink, black),
    c("true", green, white),
    c("false", red, black),
    c("TRUE", green, white),
    c("FALSE", red, black),
    c("1", green, white),
    c("0", red, black),
    c("good", green, white),
    c("bad", red, black),
    c("bueno", green, white),
    c("malo", red, black),
    c("spring", green, white),
    c("summer", red, white),
    c("fall", orange, black),
    c("winter", blue, black),
    c("meg1", "071D49", white),
    c("meg2", "EBB600", black),
    c("meg3", "F2F1F0", black),
    c("meg4", "9A9A9A", white),
    c("r5", "#290452", white),
    c("olx", green, black),
    c("virtualllantas", red, black),
    c("eltiempo", white, black),
    c("otro", grey, black),
    c("other", grey, black)
  ))
  colnames(colours_list) <- c("values","fill","colour")
  
  colours_names <- c(
    "#EBB600" = black,
    "#40A4D8" = black,
    "#5D3A9B" = white,
    "#7ADf90" = black,
    "#D35FB7" = black,
    "#DC3220" = black,
    "#F29595" = black,
    "#32CD32" = black,
    "#2FC9FE" = black,
    "#2FFECC" = black,
    "#290452" = white,
    "#0C7BDC" = black,
    "#817B7B" = black,
    "#F66320" = black,
    "#FEFE95" = black,
    "#005AB5" = white,
    "#9A9A9A" = black,
    "#00008B" = white,
    "#E1BE6A" = black,
    "#40B0A6" = black,
    "#056E00" = white,
    "#E40000" = black,
    "#FE8A71" = black,
    "#8600A1" = white,
    "#A52A2A" = white,
    "#000000" = white,
    "#E69F00" = black,
    "#009E73" = black,
    "#0072B2" = white,
    "#D55E00" = black)
  
  pal <- list(labels = colours_list, palette = rep(colours_names, 4))
  return(pal)
}

# plot_palette(names(colours_names), colours_names)
# plot_palette(colours_list$fill, colours_list$colour, colours_list$values)

####################################################################
#' Plot Palette Colours
#' 
#' This function plots a list of colours
#' 
#' @family Auxiliary
#' @param fill Vector. List of colours for fills
#' @param colour Vector. List of colours for colours
#' @param id Vector. ID for each color
#' @export
plot_palette <- function(fill, colour = "black", id = NA) {
  
  if (is.na(id)) id <- 1:length(fill)
  
  data.frame(fill = fill, 
             colour = colour,
             id = id) %>%
    ggplot(aes(x = reorder(fill, -id), y = 1)) + 
    geom_bar(aes(fill = fill), stat = "identity", position = "dodge") +
    geom_text(aes(colour = colour, label = id), hjust = 1.5) +
    scale_fill_identity() +
    scale_colour_identity() +
    coord_flip() + labs(x = "Colours", y = "") +
    guides(fill = FALSE, colour = FALSE) +
    theme_lares2() + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}


####################################################################
#' Theme for ggplot2
#' 
#' This function sets some default values into ggplot2 outputs. This
#' function is almost as using gg_colour_customs() + gg_fill_customs() 
#' + gg_text_customs = but these won't be mantained any longer. 
#' 
#' This function is lo longer mantained: use theme_lares2() instead!
#' 
#' @family Visualization
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
    
    colours_list <- lares_pal()$labels
    
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
        pal_lares <- lares_pal()$palette
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
#' @family Auxiliary
#' @export
gg_colour_customs <- function () {
  
  colours_list <- lares_pal()$labels
  values <- as.character(t(colours_list$colour)[1,])
  names(values) <- colours_list$values
  
  return(scale_color_manual(values = values))
}


####################################################################
#' Custom colours to use in ggplot as scale_fill_manual
#' 
#' This function lets the user use pre-defined default colours
#' 
#' @family Auxiliary
#' @export
gg_fill_customs <- function () {
  
  colours_list <- lares_pal()$labels
  values <- as.character(t(colours_list$fill)[1,])
  names(values) <- colours_list$values
  
  return(scale_fill_manual(values = values))
}


####################################################################
#' Custom colours to use in ggplot as scale_color_manual on texts
#' 
#' This function lets the user use pre-defined default colours
#' 
#' @family Auxiliary
#' @export
gg_text_customs <- function() {
  
  colours_list <- lares_pal()$labels
  values <- as.character(t(colours_list$colour)[1,])
  names(values) <- colours_list$values
  
  return(scale_color_manual(values = values))
}
