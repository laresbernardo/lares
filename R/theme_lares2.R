####################################################################
#' lares Theme for ggplot2
#' 
#' Based on hrbrthemes' theme_ipsum and customized for lares' use.
#'
#' @md
#' @section Why Arial Narrow?:
#' First and foremost, Arial Narrow is generally installed by default or readily
#' available on any modern system, so it's "free"-ish; plus, it is a condensed font
#' with solid default kerning pairs and geometric numbers.
#'
#' @md
#' @family Visualization
#' @param font,size Character and numeric. Base font family and base size for texts
#' @param main_colour,hard_colour,soft_colour,bg_colour,panel_colour
#' Character. Main colours for your theme
#' @param legend Character. Legend position: top, right, bottom, left
#' @param mg Numeric. External margin
#' @param pal Integer. 1 for fill and colour palette, 2 for only colour palette,
#' 3 for only fill palette, 4 for personal labels-colour palette. 0 or else for nothing.
#' @param which Character. When pal = 3, select which colours should be
#' added with the custom colours pelette: fill, colour, text (fct) - first letters
#' @export
theme_lares2 <- function(font = "Arial Narrow", 
                         size = 12.5, 
                         main_colour = "darkorange3", 
                         hard_colour = "black",
                         soft_colour = "grey25",
                         bg_colour = "white",
                         panel_colour = "grey97",
                         legend = "right",
                         mg = 14,
                         pal = 0,
                         which = "fct") {
  
  # Start from theme_minimal()
  ret <- theme_minimal(base_size = size)
  
  # Check and set font
  if (!font_exists(font)[1]) {
    if (font != "Arial Narrow")
      warning(paste(font, "font is not installed, has other name, or can't be found..."))  
    font <- NA
  } else {
    ret <- ret + theme(text = element_text(family = font))  
  }
  
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
  #update_geom_defaults("text_repel", list(family = font))
  
  ## USING ASSIGN - IMPROVE:
  # envir <- as.environment(1)
  # assign("scale_x_continuous", function(..., labels = scales::comma) scale_x_continuous(..., labels = labels), envir = envir)
  # assign("scale_y_continuous", function(..., labels = scales::comma) scale_y_continuous(..., labels = labels), envir = envir)
  # assign("scale_colour_discrete", function(..., values = as.vector(colours_pal)) scale_colour_manual(..., values = values), envir = envir)
  # assign("scale_fill_discrete", function(..., values = names(colours_pal)) scale_fill_manual(..., values = values), envir = envir)
  # assign("scale_colour_continuous", function(..., low = names(colours_pal)[2], high = names(colours_pal)[1], na.value = soft_colour) scale_colour_gradient(..., low = low, high = high, na.value = na.value), envir = envir)
  # assign("scale_fill_continuous", function(...,low = names(colours_pal)[2], high = names(colours_pal)[1], na.value = soft_colour) scale_colour_gradient(..., low = low, high = high, na.value = na.value), envir = envir)
  # assign("ggsave", function(..., bg = bg_colour) ggsave(..., bg = bg), envir = envir)
  
  if (inherits(grid, "character")) {
    grid_col <- "#CCCCCC"
    ret <- ret + theme(panel.grid = element_line(color = grid_col, size = 0.1))
    ret <- ret + theme(panel.grid.major = element_line(color = grid_col, size = 0.2))
    ret <- ret + theme(panel.grid.minor = element_line(color = grid_col, size = 0.1))
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x = element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y = element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x = element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y = element_blank())
    }
  }
  
  xj <- switch(tolower(substr("left", 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr("left", 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  
  # Axis lines
  ret <- ret + theme(axis.line = element_blank())
  # Axis ticks
  ret <- ret + theme(axis.ticks = element_blank(), 
                     axis.ticks.x = element_blank(), 
                     axis.ticks.y = element_blank())
  # Axis text
  ret <- ret + theme(axis.text.x = element_text(
    size = size * 0.85, margin = margin(t = 0), colour = soft_colour))
  ret <- ret + theme(axis.text.y = element_text(
    size = size * 0.85, margin = margin(r = 0), colour = soft_colour))
  # Axis titles
  ret <- ret + theme(axis.title = element_text(
    size = size * 1, family = font, colour = soft_colour))
  ret <- ret + theme(axis.title.x = element_text(
    hjust = xj, size = size * 1, family = font, face = "bold", colour = soft_colour))
  ret <- ret + theme(axis.title.y = element_text(
    hjust = yj, size = size * 1, colour = soft_colour,family = font, face = "bold"))
  ret <- ret + theme(axis.title.y.right = element_text(
    hjust = yj, size = size * 1, angle = -90, colour = soft_colour, family = font, face = "bold"))
  # facet_grid
  ret <- ret + theme(strip.text = element_text(
    hjust = 0, size = size * 1, colour = soft_colour, face = "bold", family = font))
  ret <- ret + theme(panel.spacing = grid::unit(0.8, "lines"))
  # Plot title
  ret <- ret + theme(plot.title = element_text(
    hjust = 0, size = size * 1.3, margin = margin(b = size * 0.85), 
    family = font, face = "bold", color = "black"))
  # Plot subtitle
  ret <- ret + theme(plot.subtitle = element_text(
    hjust = 0, size = size * 1, colour = soft_colour, 
    margin = margin(b = size * 0.7),family = font, face = "italic"))
  # Caption
  ret <- ret + theme(plot.caption = element_text(
    hjust = 1, size = size * 0.85, margin = margin(t = size * 0.9), 
    family = font, face = "bold", color = soft_colour))
  # Legend 
  ret <- ret + theme(legend.title = element_text(
    color = soft_colour, size = size * 0.9, face = "bold"),
    legend.position = legend,
    legend.justification = c(ifelse(legend %in% c("top","bottom"), 0, .5),
                             ifelse(legend == "top", 0, .5)),
    legend.margin = margin(-3,0,-5,0),
    legend.key.size = unit(0.4, "cm"))
  # Background
  ret <- ret + theme(
    panel.background = element_rect(fill = panel_colour, colour = NA),
    plot.background = element_rect(fill = bg_colour, colour = NA))
  # External margins
  ret <- ret + theme(plot.margin = margin(mg, mg, mg, mg))
  
  # Axis scales
  # p <- ggplot(dft, aes(x=Survived, y=Age)) + geom_density()
  # which <- data.frame(p$labels)
  # list <- vector2text(unlist(which), sep = "|", quotes = FALSE)
  # df <- p$data %>% select(matches(list))
  # classes <- data.frame(lapply(df, class))
  
  # if (grepl("x",tolower(comma))) ret <- ret + scale_x_comma()
  # if (grepl("y",tolower(comma))) ret <- ret + scale_y_comma()
  # if (grepl("x",tolower(percent))) ret <- ret + scale_x_percent()
  # if (grepl("y",tolower(percent))) ret <- ret + scale_y_percent()
  
  # Palette with fills and colour
  colours_pal <- lares_pal()$palette
  if (pal == 1) {
    ret <- list(ret, scale_fill_manual(values = names(colours_pal))) 
    ret <- list(ret, scale_colour_manual(values = as.vector(colours_pal)))
  }
  
  # Palette without fills
  if (pal == 2)
    ret <- list(ret, scale_colour_manual(values = names(colours_pal)))
  
  # Palette without fills
  if (pal == 3)
    ret <- list(ret, scale_fill_manual(values = names(colours_pal))) 
  
  # Custom Palette Colours defined in colour_palettes.R (personal use)
  if (pal == 4) {
    ret <- list(ret) 
    if (grepl("f", which)) ret <- append(ret, gg_fill_customs())
    if (grepl("c", which)) ret <- append(ret, gg_colour_customs())
    if (grepl("t", which)) ret <- append(ret, gg_text_customs())
  }
  
  return(ret)
  
}
