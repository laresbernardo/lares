####################################################################
#' lares Theme for ggplot2
#' 
#' Based on hrbrthemes' theme_ipsum: A precise & pristine [ggplot2] theme 
#' with opinionated defaults and an emphasis on typography.
#'
#' @md
#' @section Why Arial Narrow?:
#' First and foremost, Arial Narrow is generally installed by default or readily
#' available on any modern system, so it's "free"-ish; plus, it is a condensed font
#' with solid default kerning pairs and geometric numbers.
#'
#' There is an option `hrbrthemes.loadfonts` which -- if set to `TRUE` -- will
#' call `extrafont::loadfonts()` to register non-core fonts with R PDF & PostScript
#' devices. If you are running under Windows, the package calls the same function
#' to register non-core fonts with the Windows graphics device.
#'
#' @md
#' @param font,base_size Character and numeric. Base font family and size
#' @param main_colour,second_colour,soft_colour Character. Main colours for your theme
#' @export
theme_lares2 <- function(font = "Arial Narrow", base_size = 11.5, 
                         main_colour = "darkorange3", 
                         second_colour = "deepskyblue3",
                         soft_colour = "grey30") {
  
  ret <- theme_minimal(base_family = font, base_size = base_size)
  
  ret <- ret + theme(plot.title = element_text(color = "black"),
                     plot.caption = element_text(color = soft_colour, face = "bold"),
                     legend.title = element_text(color = soft_colour, size = base_size * 0.9, face = "bold"),
                     axis.line=element_blank(),
                     text = element_text(family = font))
  
  update_geom_defaults("text", list(family = font))
  update_geom_defaults("label", list(family = font))
  #update_geom_defaults("text_repel", list(family = font))
  update_geom_defaults("point", list(colour = main_colour))
  update_geom_defaults("bar", list(fill = main_colour))
  update_geom_defaults("col", list(fill = main_colour))
  
  scale_y_continuous <- function(...) ggplot2::scale_y_continuous(..., labels = scales::comma)
  scale_x_continuous <- function(...) ggplot2::scale_x_continuous(..., labels = scales::comma)
  scale_colour_continuous <- function(...) ggplot2::scale_colour_gradient(
    low = second_colour, high = main_colour, na.value = soft_colour, ...)
  
  if (inherits(grid, "character")) {
    grid_col <- "#CCCCCC"
    ret <- ret + theme(panel.grid = element_line(color = grid_col, size = 0.2))
    ret <- ret + theme(panel.grid.major = element_line(color = grid_col, size = 0.2))
    ret <- ret + theme(panel.grid.minor = element_line(color = grid_col, size = 0.15))
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
    }
  }
  
  xj <- switch(tolower(substr("left", 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr("left", 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  
  ret <- ret + theme(
    axis.ticks = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
  ret <- ret + theme(axis.text.x=element_text(
    size=base_size * 0.85, margin=margin(t=0),colour = soft_colour))
  ret <- ret + theme(axis.text.y=element_text(
    size=base_size * 0.85, margin=margin(r=0),colour = soft_colour))
  ret <- ret + theme(axis.title=element_text(
    size=base_size * 0.85, family=font,colour = soft_colour))
  ret <- ret + theme(axis.title.x=element_text(
    hjust=xj, size=base_size * 0.85, family=font, face="bold", colour = soft_colour))
  ret <- ret + theme(axis.title.y=element_text(
    hjust=yj, size=base_size * 0.85, colour = soft_colour,family=font, face="bold"))
  ret <- ret + theme(axis.title.y.right=element_text(
    hjust=yj, size=base_size * 0.85, angle=-90,colour = soft_colour,
    family=font, face="bold"))
  ret <- ret + theme(strip.text=element_text(
    hjust=0, size=base_size * 1, colour = soft_colour, face="bold", family=font))
  ret <- ret + theme(panel.spacing=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(
    hjust=0, size=base_size * 1.55, margin=margin(b=base_size * 0.85), 
    family=font, face="bold"))
  ret <- ret + theme(plot.subtitle=element_text(
    hjust=0, size=base_size * 1, colour = soft_colour, 
    margin=margin(b=base_size * 0.7),family = font, face="italic"))
  ret <- ret + theme(plot.caption=element_text(
    hjust=1, size=base_size * 0.85, margin = margin(t=base_size * 0.85), 
    family=font, face="plain"))
  ret <- ret + theme(plot.margin = margin(15, 15, 15, 15))
  
  return(ret)
  
}
