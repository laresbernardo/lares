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
#' @section Building upon `theme_ipsum`:
#' The function is setup in such a way that you can customize your own one by just
#' wrapping the call and changing the parameters. See source for examples.
#'
#' @section Gotchas:
#' There are distinctions between font names and various devices. Names that work
#' for display graphics devices and bitmap ones such as `png` may not work well
#' for PostScript or PDF ones. You may need two versions of a font-based
#' theme function for them to work in a particular situation. This situation
#' usually only arises when using a newer font with many weights but somewhat
#' irregular internal font name patterns.
#'
#' There is an option `hrbrthemes.loadfonts` which -- if set to `TRUE` -- will
#' call `extrafont::loadfonts()` to register non-core fonts with R PDF & PostScript
#' devices. If you are running under Windows, the package calls the same function
#' to register non-core fonts with the Windows graphics device.
#'
#' @md
#' @param base_family,base_size Character and numeric. Base font family and size
#' @param main_colour Character. Main colour for your theme; default to `#darkorange3`
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin Character and numeric. 
#' Plot title family, face, size and margi
#' @param subtitle_family,subtitle_face,subtitle_size Character and numeric. Plot subtitle 
#' family, face and size
#' @param subtitle_margin Numeric Plot subtitle margin bottom
#' @param strip_text_family,strip_text_face,strip_text_size Character and numeric. Facet 
#' label font family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin Character and numeric. 
#' Plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size Character and numeric. 
#' Axis title font family, face and size
#' @param axis_title_just Character. Axis title font justification, one of `[blmcrt]`
#' @param plot_margin Margin Vector. Plot margin (specify with [ggplot2::margin()])
#' @param grid_col,axis_col Character. Grid & axis colors; both default to `#cccccc`
#' @param grid Boolean. Panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param axis_text_size Numeric. Font size of axis text
#' @param axis Boolean. Add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks Boolean. Ticks if `TRUE` add ticks
#' @export
#' @examples \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # seminal scatterplot
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
#'        title="Seminal ggplot2 scatterplot example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_ipsum()
#'
#' # seminal bar chart
#'
#' update_geom_font_defaults()
#'
#' count(mpg, class) %>%
#'   ggplot(aes(class, n)) +
#'   geom_col() +
#'   geom_text(aes(label=n), nudge_y=3) +
#'   labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
#'        title="Seminal ggplot2 bar chart example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_ipsum(grid="Y") +
#'   theme(axis.text.y=element_blank())
#' }
theme_lares2 <- function(base_family = "Arial Narrow", base_size = 11.5, 
                         main_colour = "darkorange3",
                         plot_title_family = base_family, plot_title_size = 18,
                         plot_title_face = "bold", plot_title_margin = 10,
                         subtitle_family = base_family, subtitle_size = 12,
                         subtitle_face = "plain", subtitle_margin = 8,
                         strip_text_family = base_family, strip_text_size = 12,
                         strip_text_face = "plain",
                         caption_family = base_family, caption_size = 10,
                         caption_face = "italic", caption_margin = 10,
                         axis_text_size = base_size * 0.85,
                         axis_title_family = subtitle_family, axis_title_size = 9,
                         axis_title_face = "plain", axis_title_just = "left",
                         plot_margin = margin(15, 15, 15, 15),
                         grid_col = "#cccccc", grid = TRUE,
                         axis_col = "#cccccc", axis = FALSE, ticks = FALSE) {
  
  ret <- theme_minimal(base_family=base_family, base_size=base_size) +
    theme(plot.title = element_text(color = main_colour),
          plot.caption = element_text(color = main_colour, face = 'bold'),
          legend.background=element_blank(),
          legend.key=element_blank())
  
  if (inherits(grid, "character") | grid == TRUE) {
    
    ret <- ret + theme(panel.grid = element_line(color = grid_col, size = 0.2))
    ret <- ret + theme(panel.grid.major = element_line(color = grid_col, size = 0.2))
    ret <- ret + theme(panel.grid.minor = element_line(color = grid_col, size = 0.15))
    
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
    }
    
  } else {
    ret <- ret + theme(panel.grid=element_blank())
  }
  
  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line=element_line(color="#2b2b2b", size=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }
  
  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  
  ret <- ret + theme(axis.text.x=element_text(size=axis_text_size, margin=margin(t=0)))
  ret <- ret + theme(axis.text.y=element_text(size=axis_text_size, margin=margin(r=0)))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y.right=element_text(hjust=yj, size=axis_title_size, angle=90,
                                                     family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                             face=strip_text_face, family=strip_text_family))
  ret <- ret + theme(panel.spacing=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             family=plot_title_family, face=plot_title_face))
  ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                family=subtitle_family, face=subtitle_face))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               family=caption_family, face=caption_face))
  ret <- ret + theme(plot.margin=plot_margin)
  
  return(ret)
  
}
