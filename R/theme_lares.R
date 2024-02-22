####################################################################
#' Theme for ggplot2 (lares)
#'
#' Based on \code{hrbrthemes}' \code{theme_ipsum} and customized for \code{lares} usage.
#' With this team you can custom the colour and fill palettes, global colour parameters,
#' major and minor grids, legend, font and font size.
#'
#' @section Why Arial Narrow?:
#' First and foremost, Arial Narrow is generally installed by default or readily
#' available on any modern system, so it's "free"-ish; plus, it is a condensed font
#' with solid default kerning pairs and geometric numbers.
#'
#' @family Themes
#' @inheritParams cache_write
#' @param font,size Character and numeric. Base font family and base size for texts.
#' \code{Arial Narrow} is set by default when the library is loaded; you may change it
#' with \code{Sys.setenv("LARES_FONT" = "X")} or by using this parameter manually.
#' @param main_colour,hard_colour,soft_colour,plot_colour,panel_colour
#' Character. Main colours for your theme.
#' @param background Character. Main colour for your background. Overwrites
#' \code{plot_colour} and \code{panel_colour}.
#' @param no_facets Boolean. Suppress facet labels?
#' @param legend Character. Legend position: \code{"top"}, \code{"right"},
#' \code{"bottom"}, or \code{"left"} You can also set to \code{FALSE} or
#' \code{"none"} to suppress legend.
#' @param grid Character or Boolean. Use \code{TRUE/FALSE} or a combination of
#' \code{X}, \code{x}, \code{Y}, \code{y} to enable/disable minor and major grids.
#' @param axis Character or Boolean. Use \code{TRUE/FALSE}, \code{x} or \code{Y}
#' to enable X and/or Y axis lines.
#' @param clean Boolean. Suppress grids and axis? Overwrites both parameters.
#' @param mg Numeric. External margins reference.
#' @param pal Integer. \code{1} for fill and colour palette,
#' \code{2} for only colour palette, \code{3} for only fill palette, \code{4} for
#' personal labels-colour palette. \code{0} for nothing.
#' @param palette Character vector. Pass a vector with HEX colour
#' codes to use a custom palette. If you pass a named vector, the name values will be
#' used as fill and the values will be used as colour.
#' @param which Character. When \code{pal = 3}, select which colours should be
#' added with the custom colours palette: fill, colour, text (fct) - first letters.
#' @return Themed ggplot2 object
#' @examples
#' \donttest{
#' data(dft)
#' library(ggplot2)
#' p <- ggplot(dft, aes(x = Pclass, y = sum(Fare), fill = Pclass)) +
#'   geom_col()
#' p + theme_lares()
#' p + theme_lares(pal = 1)
#' p + theme_lares(background = "#999999", mg = 25)
#' p + theme_lares(legend = "top", grid = "Yy")
#' p + theme_lares(clean = TRUE)
#' }
#' @export
theme_lares <- function(font = Sys.getenv("LARES_FONT"),
                        size = 12,
                        main_colour = "darkorange3",
                        hard_colour = "black",
                        soft_colour = "grey30",
                        plot_colour = "transparent",
                        panel_colour = "transparent",
                        background = "transparent",
                        no_facets = FALSE,
                        legend = NULL,
                        grid = TRUE,
                        axis = TRUE,
                        clean = FALSE,
                        mg = 9,
                        pal = 0,
                        palette = NULL,
                        which = "fc",
                        ...) {
  # Start from theme_minimal()
  ret <- theme_minimal(base_size = size)

  # Overwrite parameters with quick setting parameters
  if (clean) axis <- grid <- FALSE
  if (background != "transparent") panel_colour <- plot_colour <- background
  if (isFALSE(legend)) legend <- "none"

  # Check and set font
  font <- .font_global(font, quiet = FALSE, ...)
  ret <- ret + theme(text = element_text(family = font))

  # Set some defaults
  update_geom_defaults("text", list(colour = hard_colour, family = font))
  update_geom_defaults("label", list(colour = hard_colour, family = font))
  update_geom_defaults("point", list(colour = hard_colour, alpha = 0.95))
  update_geom_defaults("line", list(colour = hard_colour, alpha = 0.95))
  update_geom_defaults("area", list(fill = main_colour, alpha = 0.95))
  update_geom_defaults("rect", list(fill = main_colour, alpha = 0.95))
  update_geom_defaults("density", list(fill = main_colour, alpha = 0.95))
  update_geom_defaults("bar", list(fill = main_colour, alpha = 0.95))
  update_geom_defaults("col", list(fill = main_colour, alpha = 0.95))
  update_geom_defaults("boxplot", list(fill = main_colour, alpha = 0.9))
  # update_geom_defaults("text_repel", list(family = font))

  # Legend
  aux <- ifelse("top" %in% legend, "right", "left")
  xj <- switch(tolower(substr(aux, 1, 1)),
    b = 0,
    l = 0,
    m = 0.5,
    c = 0.5,
    r = 1,
    t = 1
  )
  yj <- switch(tolower(substr(aux, 2, 2)),
    b = 0,
    l = 0,
    m = 0.5,
    c = 0.5,
    r = 1,
    t = 1
  )
  if (!is.null(legend)) {
    ret <- ret + theme(
      legend.title = element_text(color = soft_colour, size = size * 0.9, face = "bold"),
      legend.position = legend,
      legend.justification = c(
        ifelse(legend %in% c("top", "bottom"), 0, .5),
        ifelse(legend == "top", 0, ifelse(legend %in% "left", 1, .5))
      ),
      legend.margin = margin(-3, 0, -4, 0)
    )
  }

  if (inherits(grid, "character") || grid) {
    grid_col <- "#CCCCCC"
    ret <- ret + theme(panel.grid = element_line(color = grid_col, size = 0.2))
    ret <- ret + theme(panel.grid.major = element_line(color = grid_col, size = 0.1))
    ret <- ret + theme(panel.grid.minor = element_line(color = grid_col, size = 0.05))
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x = element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y = element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x = element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y = element_blank())
    }
  } else {
    ret <- ret + theme(panel.grid = element_blank())
  }

  # Axis lines
  ret <- ret + theme(axis.line = element_blank())

  # Axis ticks
  ret <- ret + theme(
    axis.ticks = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )

  # Axis text
  ret <- ret + theme(axis.text.x = element_text(
    size = size * 0.8, margin = margin(t = 0), colour = soft_colour
  ))
  ret <- ret + theme(axis.text.y = element_text(
    size = size * 0.8, margin = margin(r = 0), colour = soft_colour
  ))

  # Axis titles
  ret <- ret + theme(axis.title = element_text(
    size = size * 0.85, family = font, colour = soft_colour
  ))
  ret <- ret + theme(axis.title.x = element_text(
    hjust = xj, size = size * 0.85, family = font, face = "bold", colour = soft_colour
  ))
  ret <- ret + theme(axis.title.y = element_text(
    hjust = yj, size = size * 0.85, colour = soft_colour, family = font, face = "bold"
  ))
  ret <- ret + theme(axis.title.y.right = element_text(
    hjust = yj, size = size * 0.85, angle = -90, colour = soft_colour, family = font, face = "bold"
  ))

  # Suppress axis
  if (axis == FALSE) {
    ret <- ret +
      theme(
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank()
      )
  }
  if (is.character(axis)) {
    if (tolower(axis) == "x") {
      ret <- ret + theme(
        axis.text.y = element_blank(), axis.title.y = element_blank()
      )
    }
    if (tolower(axis) == "y") {
      ret <- ret + theme(
        axis.text.x = element_blank(), axis.title.x = element_blank()
      )
    }
  }

  # facet_grid
  ret <- ret + theme(strip.text = element_text(
    hjust = 0, size = size * 0.9, colour = soft_colour, face = "bold", family = font
  ))
  ret <- ret + theme(panel.spacing = grid::unit(0.8, "lines"))
  if (no_facets) {
    ret <- ret + theme(strip.background = element_blank(), strip.text = element_blank())
  }

  # Plot title
  ret <- ret + theme(plot.title = element_text(
    size = size * 1.25, margin = margin(b = size * 0.3),
    family = font, face = "bold", color = "black"
  ))
  # Align plot title to the very left edge (more space) [ggplot2 >= 3.3.0]
  ggc <- stringr::str_split(as.character(packageVersion("ggplot2")), "\\.")[[1]]
  if (ggc[1] >= 3 && ggc[2] >= 3) {
    ret <- ret + theme(plot.title.position = "plot")
  }

  # Plot subtitle
  ret <- ret + theme(plot.subtitle = element_text(
    hjust = 0, size = size * 1.1, colour = soft_colour,
    margin = margin(b = size * 0.5), family = font, face = "italic"
  ))

  # Caption
  ret <- ret + theme(plot.caption = element_text(
    hjust = 1, size = size * 0.85, margin = margin(t = size * 0.9),
    family = font, color = soft_colour
  ))

  # Background
  ret <- ret + theme(
    panel.background = element_rect(fill = panel_colour, colour = NA),
    plot.background = element_rect(fill = plot_colour, colour = NA)
  )

  # External margins
  ret <- ret + theme(plot.margin = margin(mg, mg, mg, mg))

  # Colour Palette
  if (!is.null(palette)) {
    if (is.null(names(palette))) {
      names(palette) <- as.vector(palette)
    }
    colours_pal <- palette
  } else {
    colours_pal <- lares_pal()$palette
  }

  # Palette with fills and colour
  if (pal == 1) {
    ret <- list(ret, scale_fill_manual(values = names(colours_pal)))
    ret <- list(ret, scale_colour_manual(values = as.vector(colours_pal)))
  }

  # Palette without fills
  if (pal == 2) {
    ret <- list(ret, scale_colour_manual(values = names(colours_pal)))
  }

  # Palette without fills
  if (pal == 3) {
    ret <- list(ret, scale_fill_manual(values = names(colours_pal)))
  }

  # Custom palette defined in colour_palettes.R and/or lares.colours.custom options
  if (pal == 4) {
    which <- tolower(which)
    if ((grepl("c", which) && grepl("t", which))) {
      stop("In your 'which' parameter, pass only 'c' OR 't', not both")
    }
    # FIX: Scale for 'fill' is already present. Adding another scale for 'fill',
    # which will replace the existing scale. (not being suppressed)
    suppressMessages({
      ret <- list(ret)
      if (grepl("f", which)) ret <- append(ret, gg_fill_customs())
      if (grepl("c", which)) ret <- append(ret, gg_colour_customs())
      if (grepl("t", which)) ret <- append(ret, gg_text_customs())
    })
  }

  return(ret)
}

####################################################################
#' Custom fill, colour and text colours for ggplot2
#'
#' This function lets the user use pre-defined default colours.
#' Check your \code{lares_pal()$labels} scale. Feel free to use
#' \code{gg_vals()} to debug colours used in latest plot.
#'
#' Notice that when the layer defined is any of GeomPoint, GeomLine,
#' GeomText or GeomLabel, \code{gg_colour_customs()} will force
#' \code{column = "fill"} parameter.
#'
#' @family Themes
#' @param column Character. Select any of "fill" or "colour" to use on
#' your \code{lares_pal()$labels} palette.
#' @param ... Allow additional parameters not used.
#' @examples
#' library("ggplot2")
#' # Generic plot function to run examples to
#' run_plot <- function(add_fxs = TRUE) {
#'   p <- data.frame(station = c("spring", "summer", "fall", "winter"), num = 1:4) %>%
#'     ggplot(aes(x = station, y = num, fill = station)) +
#'     geom_col() +
#'     geom_text(aes(y = 0.5, label = num, colour = station), size = 6)
#'   if (add_fxs) p <- p + gg_fill_customs() + gg_colour_customs()
#'   return(p)
#' }
#' # Default colours
#' run_plot()
#' # Check last colours used
#' gg_vals("fill", "fill")
#' gg_vals("colour", "colour")
#' # Change any default colour
#' options("lares.colours.custom" = data.frame(
#'   values = c("summer", "winter"),
#'   fill = c("pink", "black"),
#'   colour = c("black", "white")
#' ))
#' run_plot()
#' # Check last colours used
#' gg_vals("fill", "fill")
#' gg_vals("colour", "colour")
#' # Reset to default colours
#' options("lares.colours.custom" = NULL)
#' # Notice you can use 'pal = 4' argument on theme_lares() too
#' run_plot(add_fxs = FALSE) + theme_lares(pal = 4)
#' @return Same as \code{scale_fill_manual} or \code{scale_colour_manual}
#' but with custom palette applied.
#' @export
gg_fill_customs <- function(column = "fill", ...) {
  scale_fill_manual(values = gg_vals("fill", column, ...), aesthetics = "fill")
}

#' @rdname gg_fill_customs
#' @export
gg_colour_customs <- function(column = "colour", ...) {
  scale_color_manual(values = gg_vals("colour", column, ...), aesthetics = "colour")
}

#' @rdname gg_fill_customs
#' @export
gg_text_customs <- function(column = "colour", ...) {
  scale_color_manual(values = gg_vals("label", column, ...), aesthetics = "colour")
}

#' @rdname gg_fill_customs
#' @param layer Character. Select any of "fill", "colour", or "label" to get the
#' layer containing the colours labels you wish to colour.
#' @param cols Data.frame. Customize colour palette with a data.frame. Must
#' contain values, fill, and colour columns.
#' @export
gg_vals <- function(layer = "fill", column = layer, cols = NULL, ...) {
  check_opts(layer, c("fill", "colour", "label"))
  check_opts(column, c("fill", "colour"))
  x <- last_plot()
  if (!is.null(cols)) {
    stopifnot(is.data.frame(cols))
    check_opts(colnames(cols), c("values", "fill", "colour"))
  } else {
    cols <- lares_pal()$labels
  }
  # Get colours present in data
  labs <- unlist(lapply(x$layers, function(y) as_label(y$mapping[[layer]])))
  labs <- c(labs, as_label(x$mapping[[layer]]))
  labs <- labs[!labs %in% c("NULL", "<uneval>")]
  cols <- cols[cols$values %in% unique(unlist(select(x$data, any_of(labs)))), ]
  # If point, line, text, label, force using fill colours
  invert <- c("GeomPoint", "GeomLine", "GeomText", "GeomLabel")
  layers_present <- unique(unlist(lapply(x$layers, function(y) class(y$geom))))
  if (any(invert %in% layers_present)) column <- "fill"
  # Final values vector
  values <- as.character(t(cols[, column])[1, ])
  names(values) <- cols$values
  return(values)
}

.font_global <- function(font, quiet = TRUE, when_not = NA, ...) {
  temp <- font_exists(font, ...)
  if (!any(isTRUE(temp))) {
    if (isFALSE(is.na(font[1]))) {
      if (isTRUE(font[1] != "") && !quiet) {
        warning(sprintf("Font(s) %s not installed, with other name, or can't be found", v2t(font)))
      }
      Sys.unsetenv("LARES_FONT") # So R doesn't try again by default
      font <- when_not
    }
  } else {
    # Return first one that is found
    font <- font[head(which(temp), 1)]
  }
  return(font)
}
