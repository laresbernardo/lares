####################################################################
#' Personal Colours Palette
#'
#' This function plots a list of colours on a specific palette
#'
#' @family Auxiliary
#' @param return Character. Get only what you need. Select any of:
#' "all" or "list" (list),
#' "colors" or "colours" (vector),
#' "pal" or "palette" (named vector),
#' "simple" (named vector),
#' "custom" or "personal" (data.frame)
#' @return Depending on the \code{return} input, we get a:
#' \itemize{
#'   \item \code{vector} with \code{palette} results vector
#'   \item \code{vector} with \code{palette} results vector's names
#'   \item \code{list} with \code{palette} results vector, \code{labels}
#'   results data.frame, and \code{simple} results named vector
#' }
#' @examples
#' # Simple colour-named palette
#' lares_pal("simple")
#'
#' # Raw colours and counter-colours
#' # OR simply: lares_pal("palette")
#' nice_palette <- lares_pal("colours")
#' nice_palette_ctr <- as.vector(lares_pal()$palette)
#' lapply(list(nice_palette, nice_palette_ctr), head)
#'
#' # Personal colours by name
#' df <- lares_pal("custom")
#' df[sample(nrow(df), 5), ]
#' @export
lares_pal <- function(return = "list") {
  simple <- c(
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

  colours_list <- data.frame(rbind(
    c("allianz", "#0038A8", simple[["black"]]),
    c("equidad", "#52CF44", simple[["black"]]),
    c("colpatria", "#EE0606", simple[["black"]]),
    c("del estado", "#F37000", simple[["black"]]),
    c("suramericana", "#1F6D8C", simple[["black"]]),
    c("mapfre", "#34000D", simple[["black"]]),
    c("la previsora", "#6F9A45", simple[["black"]]),
    c("aig", "#C71585", simple[["black"]]),
    c("generali", "#B21F1F", simple[["black"]]),
    c("solidaria", "#E69500", simple[["black"]]),
    c("liberty", "#4E629A", simple[["black"]]),
    c("bolivar", "#F0F206", simple[["black"]]),
    c("cia", "#8ACBE5", simple[["black"]]),
    c("mundial", "#8ACBE5", simple[["black"]]),
    c("puntored", "#FFFF00", simple[["grey"]]),
    c("movilred", "#FF1493", simple[["black"]]),
    c("moviired", "#FF1493", simple[["black"]]),
    c("moviaval", "#00A69C", simple[["black"]]),
    c("web", "#290452", simple[["white"]]),
    c("somosf1", "#290452", simple[["white"]]),
    c("f1", "#290452", simple[["white"]]),
    c("funnel-soat4_desktop", "#290452", simple[["white"]]),
    c("funnel-ujk2d_desktop", "#8ACBE5", simple[["black"]]),
    c("funnel-ujk2m_old_mobile", "#7AC4E1", simple[["black"]]),
    c("red", simple[["black"]], simple[["black"]]),
    c("m", simple[["blue"]], simple[["white"]]),
    c("f", simple[["pink"]], simple[["black"]]),
    c("male", simple[["blue"]], simple[["white"]]),
    c("female", simple[["pink"]], simple[["black"]]),
    c("hombre", simple[["blue"]], simple[["white"]]),
    c("mujer", simple[["pink"]], simple[["black"]]),
    c("true", simple[["green"]], simple[["white"]]),
    c("false", simple[["red"]], simple[["black"]]),
    c("TRUE", simple[["green"]], simple[["white"]]),
    c("FALSE", simple[["red"]], simple[["black"]]),
    c("1", simple[["green"]], simple[["white"]]),
    c("0", simple[["red"]], simple[["black"]]),
    c("-1", simple[["red"]], simple[["black"]]),
    c("X", simple[["grey"]], simple[["black"]]),
    c("good", "#59B3D2", simple[["white"]]),
    c("bad", "#E5586E", simple[["black"]]),
    c("bueno", "#59B3D2", simple[["white"]]),
    c("malo", "#E5586E", simple[["black"]]),
    c("spring", simple[["green"]], simple[["white"]]),
    c("summer", simple[["red"]], simple[["white"]]),
    c("fall", simple[["orange"]], simple[["black"]]),
    c("winter", simple[["blue"]], simple[["black"]]),
    c("meg1", "071D49", simple[["white"]]),
    c("meg2", "EBB600", simple[["black"]]),
    c("meg3", "F2F1F0", simple[["black"]]),
    c("meg4", "9A9A9A", simple[["white"]]),
    c("r5", "#290452", simple[["white"]]),
    c("olx", simple[["green"]], simple[["black"]]),
    c("virtualllantas", simple[["red"]], simple[["black"]]),
    c("eltiempo", "#E5E5E5", simple[["black"]]),
    c("autolab", simple[["orange"]], simple[["black"]]),
    c("aflore", simple[["blue"]], simple[["black"]]),
    c("otro", simple[["grey"]], simple[["black"]]),
    c("negative", "#FA4113", simple[["black"]]),
    c("fear", "#810806", simple[["white"]]),
    c("disgust", "#BF200E", simple[["black"]]),
    c("anger", "#FE9B13", simple[["black"]]),
    c("sadness", "#838B8B", simple[["black"]]),
    c("anticipation", "#FE8A71", simple[["black"]]),
    c("surprise", "#F7E565", simple[["black"]]),
    c("trust", "#40A4D8", simple[["black"]]),
    c("joy", "#BD116F", simple[["black"]]),
    c("positive", "#3DA4AB", simple[["black"]]),
    c("fb1", "#405996", simple[["white"]]),
    c("fb2", "#7184B2", simple[["black"]]),
    c("fb3", "#AFBEE3", simple[["black"]])
  ))
  colnames(colours_list) <- c("values", "fill", "colour")

  # Generic colour and counter-colour palette
  colours_names <- c(
    "#FF8303" = simple[["black"]],
    "#40A4D8" = simple[["black"]],
    "#5D3A9B" = simple[["white"]],
    "#E63946" = simple[["black"]],
    "#2A9D8F" = simple[["black"]],
    "#D35FB7" = simple[["black"]],
    "#F8D962" = simple[["black"]],
    "#03396C" = simple[["white"]],
    "#F29595" = simple[["black"]],
    "#2FFECC" = simple[["black"]],
    "#8D99AE" = simple[["black"]],
    "#7ADf90" = simple[["black"]],
    "#290452" = simple[["white"]],
    "#0C7BDC" = simple[["black"]],
    "#817B7B" = simple[["black"]],
    "#F66320" = simple[["black"]],
    "#F4A261" = simple[["black"]],
    "#005AB5" = simple[["white"]],
    "#9A9A9A" = simple[["black"]],
    "#00008B" = simple[["white"]],
    "#E1BE6A" = simple[["black"]],
    "#40B0A6" = simple[["black"]],
    "#056E00" = simple[["white"]],
    "#E40000" = simple[["black"]],
    "#FE8A71" = simple[["black"]],
    "#8600A1" = simple[["white"]],
    "#A52A2A" = simple[["white"]],
    "#000000" = simple[["white"]],
    "#E69F00" = simple[["black"]],
    "#009E73" = simple[["black"]],
    "#0072B2" = simple[["white"]],
    "#D55E00" = simple[["black"]]
  )

  pal <- list(
    labels = colours_list,
    palette = rep(colours_names, 4),
    simple = simple
  )

  if (return %in% c("colors", "colours", "color", "colour", "raw", "col")) {
    pal <- names(colours_names)
  }
  if (return %in% c("pal", "palette")) {
    pal <- colours_names
  }
  if (return %in% c("personal", "custom", "labels")) {
    pal <- colours_list
  }
  if (return %in% c("simple")) {
    pal <- simple
  }

  structure(pal, class = "lares_pal")
  attr("type", return)
  return(pal)
}


####################################################################
#' Plot Palette Colours
#'
#' This function plots a list of colours
#'
#' @family Auxiliary
#' @param fill Vector. List of colours for fills.
#' @param colour Vector. List of colours for colours.
#' @param id Vector. ID for each color.
#' @param limit Integer. Show only first n values.
#' @return Plot with \code{fill} colours and \code{colour} counter-colours
#' if provided.
#' @examples
#' # Simply pass a vector
#' pal <- lares_pal("simple")
#' plot_palette(pal)
#' # Or fill + color named vector
#' pal <- lares_pal("pal")
#' plot_palette(fill = names(pal), colour = as.vector(pal))
#' @export
plot_palette <- function(fill, colour = "black", id = NA, limit = 12) {
  if (length(fill) > limit) {
    fill <- fill[1:limit]
    colour <- colour[1:limit]
    colour <- colour[!is.na(colour)]
    message(paste("Limited to", limit, "colours. Overwrite with 'limit' parameter"))
  }
  if (is.na(id[1])) id <- seq_along(fill)
  p <- data.frame(fill = fill, colour = colour, id = id) %>%
    distinct(.keep_all = TRUE) %>%
    ggplot(aes(x = reorder(fill, -id), y = 1)) +
    geom_bar(aes(fill = fill), stat = "identity", position = "dodge") +
    geom_text(aes(colour = colour, label = id), hjust = 1.5) +
    scale_fill_identity() +
    scale_colour_identity() +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    guides(fill = "none", colour = "none") +
    theme_lares(font = NA, axis = "Y")
  return(p)
}


####################################################################
#' Custom colours for scale_color_manual [Deprecated]
#'
#' This function lets the user use pre-defined default colours
#'
#' @family Auxiliary
#' @return Same as \code{scale_color_manual} but with custom palette.
#' @export
gg_colour_customs <- function() {
  colours_list <- lares_pal()$labels
  values <- as.character(t(colours_list$colour)[1, ])
  names(values) <- colours_list$values
  return(scale_color_manual(values = values))
}


####################################################################
#' Custom colours for scale_fill_manual [Deprecated]
#'
#' This function lets the user use pre-defined default colours
#'
#' @family Auxiliary
#' @return Same as \code{scale_fill_manual} but with custom palette.
#' @export
gg_fill_customs <- function() {
  colours_list <- lares_pal()$labels
  values <- as.character(t(colours_list$fill)[1, ])
  names(values) <- colours_list$values
  return(scale_fill_manual(values = values))
}


####################################################################
#' Custom colours for scale_color_manual on texts [Deprecated]
#'
#' This function lets the user use pre-defined default colours
#'
#' @family Auxiliary
#' @return Same as \code{scale_color_manual} but with custom palette.
#' @export
gg_text_customs <- function() {
  colours_list <- lares_pal()$labels
  values <- as.character(t(colours_list$colour)[1, ])
  names(values) <- colours_list$values
  return(scale_color_manual(values = values))
}
