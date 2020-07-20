####################################################################
#' Personal Colours Palette
#' 
#' This function plots a list of colours on a specific palette
#' 
#' @family Auxiliary
#' @examples 
#' # Raw colours and counter-colours
#' nice_palette <- names(lares_pal()$palette)
#' nice_palette_ctr <- as.vector(lares_pal()$palette)
#' lapply(list(nice_palette, nice_palette_ctr), head)
#' 
#' # Personal colours by name
#' df <- lares_pal()$labels
#' df[sample(nrow(df), 5), ]
#' @export
lares_pal <- function() {
  
  black <- "#000000"
  white <- "#FFFFFF"
  red <- "#E63946"
  green <- "#3DA4AB" 
  pink <- "#FFCAD4"
  blue <- "#FFCAD4Z"
  orange <- "#F4A261"
  grey <- "#8D99AE"
  
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
    c("moviaval", "#00A69C", black),
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
    c("-1", red, black),
    c("X", grey, black),
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
    c("eltiempo", "#E5E5E5", black),
    c("autolab", orange, black),
    c("aflore", blue, black),
    c("otro", grey, black),
    c("negative", "#FA4113", black),
    c("fear", "#810806", white),
    c("disgust", "#BF200E", black),
    c("anger", "#FE9B13", black),
    c("sadness", "#838B8B", black),
    c("anticipation", "#FE8A71", black),
    c("surprise", "#F7E565", black),
    c("trust", "#40A4D8", black),
    c("joy", "#BD116F", black),
    c("positive", "#3DA4AB", black)
  ))
  colnames(colours_list) <- c("values","fill","colour")
  
  colours_names <- c(
    "#EBB600" = black,
    "#40A4D8" = black,
    "#5D3A9B" = white,
    "#2A9D8F" = black,
    "#A8DADC" = black,
    "#D35FB7" = black,
    "#D62828" = black,
    "#F29595" = black,
    "#2FFECC" = black,
    "#7ADf90" = black,
    "#290452" = white,
    "#0C7BDC" = black,
    "#817B7B" = black,
    "#F66320" = black,
    "#F4A261" = black,
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
  
  pal <- list(labels = colours_list, 
              palette = rep(colours_names, 4))
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
#' @examples 
#' pal <- lares_pal()$palette[1:15]
#' head(pal)
#' plot_palette(fill = names(pal), colour = as.vector(pal))
#' @export
plot_palette <- function(fill, colour = "black", id = NA) {
  if (is.na(id[1])) id <- 1:length(fill)
  p <- data.frame(fill = fill, colour = colour, id = id) %>%
    distinct(.keep_all = TRUE) %>%
    ggplot(aes(x = reorder(fill, -id), y = 1)) + 
    geom_bar(aes(fill = fill), stat = "identity", position = "dodge") +
    geom_text(aes(colour = colour, label = id), hjust = 1.5) +
    scale_fill_identity() +
    scale_colour_identity() +
    coord_flip() + labs(x = NULL, y = NULL) +
    guides(fill = FALSE, colour = FALSE) +
    theme_lares2(font = NA) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  return(p)
}


####################################################################
#' Custom colours for scale_color_manual [Deprecated]
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
#' Custom colours for scale_fill_manual [Deprecated]
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
#' Custom colours for scale_color_manual on texts [Deprecated]
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
