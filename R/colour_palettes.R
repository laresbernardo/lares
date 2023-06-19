####################################################################
#' Personal Colours Palette
#'
#' Fetch customizable palettes for the library's usage. The package has
#' its own default colour-blind friendly colours but can be customized using
#' R internal options (i.e. \code{options("lares.palette" = c("#FF8303" = "#000",
#' "#40A4D8" = "#FFF", ...))}. There are 3 options you can use to customize all
#' colour palletes: "lares.palette" (vector, will be used in the same order as passed,
#' and must have a counter colour defined), "lares.colours" (vector, simple colour
#' names and their HEX codes), and "lares.colours.custom" (data.frame, containing
#' "values" to use dynamically, "fill" for main colour, and "colour" (not obligatory)
#' for counter colour).
#'
#' @family Themes
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
  # Generic colour and counter-colour palette
  colours_names <- getOption("lares.palette")
  if (length(unique(names(colours_names))) != length(colours_names)) {
    stop("Check your lares.palette option. Do not repeat colours.")
  }
  if (!"character" %in% class(colours_names)) {
    stop("Check your lares.palette option. Must be a character vector.")
  }

  # Specific (simple) colours by names
  simple <- c(getOption("lares.colours"), getOption("lares.colors"))
  if (length(unique(names(simple))) != length(simple)) {
    stop("Check your lares.colours option. Do not repeat colours.")
  }
  if (!"character" %in% class(simple)) {
    stop("Check your lares.colours option. Must be a character vector.")
  }

  # Personal colours list (add more with lares.colours.custom options)
  colours_list <- data.frame(rbind(
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
    c("aprueba", "#59B3D2", simple[["white"]]),
    c("rechaza", "#E5586E", simple[["black"]]),
    c("ns/nc", "#E5586E", simple[["grey"]]),
    c("positive", "#59B3D2", simple[["black"]]),
    c("negative", "#E5586E", simple[["black"]]),
    c("bueno", "#59B3D2", simple[["white"]]),
    c("malo", "#E5586E", simple[["black"]]),
    c("spring", simple[["green"]], simple[["white"]]),
    c("summer", simple[["red"]], simple[["white"]]),
    c("fall", simple[["orange"]], simple[["black"]]),
    c("winter", simple[["blue"]], simple[["black"]]),
    c("meg1", "#071D49", simple[["white"]]),
    c("meg2", "#EBB600", simple[["black"]]),
    c("meg3", "#F2F1F0", simple[["black"]]),
    c("meg4", "#9A9A9A", simple[["white"]]),
    c("r5", "#290452", simple[["white"]]),
    c("otro", simple[["grey"]], simple[["black"]]),
    c("fear", "#810806", simple[["white"]]),
    c("disgust", "#BF200E", simple[["black"]]),
    c("anger", "#FE9B13", simple[["black"]]),
    c("sadness", "#838B8B", simple[["black"]]),
    c("anticipation", "#FE8A71", simple[["black"]]),
    c("surprise", "#F7E565", simple[["black"]]),
    c("trust", "#40A4D8", simple[["black"]]),
    c("joy", "#BD116F", simple[["black"]]),
    c("fb1", "#405996", simple[["white"]]),
    c("fb2", "#7184B2", simple[["black"]]),
    c("fb3", "#AFBEE3", simple[["black"]]),
    c("facebook", "#4267B2", simple[["black"]]),
    c("instagram", "#E95950", simple[["black"]]),
    c("messenger", "#0084FF", simple[["black"]]),
    c("whatsapp", "#4DC247", simple[["black"]])
  ))
  colnames(colours_list) <- c("values", "fill", "colour")

  more_cols <- getOption("lares.colours.custom")
  if (length(more_cols) > 1) {
    if (!"data.frame" %in% class(more_cols)) {
      stop("Check your lares.colours.custom option. Must be a data.frame.")
    }
    if (!"colour" %in% colnames(more_cols)) more_cols$colour <- "#000"
    if (!all(colnames(more_cols) %in% colnames(colours_list))) {
      stop(
        "Check your lares.colours.custom option. Column names must match: ",
        v2t(colnames(colours_list))
      )
    }
    colours_list <- colours_list %>%
      filter(!.data$values %in% more_cols$values) %>%
      bind_rows(more_cols)
  }

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
  if (return %in% "simple") {
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
#' @family Themes
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
