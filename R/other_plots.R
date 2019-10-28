####################################################################
#' Plot timeline as Gantt Plot
#' 
#' This function plots groups of observartions with timelines in a 
#' Gantt Plot way. Only works if start and end are date format values.
#' 
#' @param event Vector. Event, role, label, or row.
#' @param start Vector. Start date.
#' @param end Vector. End date. Only one day be default if not defined
#' @param label Vector. Place, institution, or label.
#' @param group Vector. Academic, Work, Extracurricular...
#' @param title Character. Title for the plot
#' @param subtitle Character. Subtitle for the plot
#' @param size Numeric. Bars' width
#' @param colour Character. Colour when not using type
#' @param save Boolean. Save the output plot in our working directory
#' @param subdir Character. Into which subdirectory do you wish to save the plot to?
#' @export
plot_timeline <- function(event, start, 
                          end = start + 1, 
                          label = NA, 
                          group = NA, 
                          title = "Curriculum Vitae Timeline", 
                          subtitle = "Bernardo Lares",
                          size = 7,
                          colour = "orange",
                          save = FALSE,
                          subdir = NA) {
  
  # Let's gather all the data
  df <- data.frame(
    Role = as.character(event), 
    Place = as.character(label), 
    Start = lubridate::date(start), 
    End = lubridate::date(end),
    Type = group)

  # Duplicate data for ggplot's geom_lines
  cvlong <- data.frame(
    pos = rep(as.numeric(rownames(df)), 2),
    name = rep(as.character(df$Role), 2),
    type = rep(factor(df$Type, ordered = TRUE), 2),
    where = rep(as.character(df$Place), 2),
    value = c(df$Start, df$End),
    label_pos = rep(df$Start + floor((df$End - df$Start)/2), 2))

  # Plot timeline
  maxdate <- max(df$End)
  p <- ggplot(cvlong, aes(x = value, y = reorder(name, -pos), label = where, group = pos)) + 
    geom_vline(xintercept = maxdate, alpha = 0.8, linetype = "dotted") +
    labs(title = title, subtitle = subtitle, 
         x = NULL, y = NULL, colour = "") +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_line(size = 0.25, colour = "grey80"))
  
  if (!is.na(cvlong$type) | length(unique(cvlong$type)) > 1) {
    p <- p + geom_line(aes(colour = type), size = size) +
      facet_grid(type ~ ., scales = "free", space = "free") +
      guides(colour = FALSE) +
      scale_colour_brewer(palette = "Set1")
  } else {
    p <- p + geom_line(size = size, colour = colour)
  }
  
  p <- p + geom_label(aes(x = label_pos), colour = "black", size = 2, alpha = 0.7)
  
  # Export file name and folder for plot
  if (save == TRUE) {
    file_name <- "cv_timeline.png"
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep = "/")
    }
    p <- p + ggsave(file_name, width = 8, height = 6)
    message(paste("Saved plot as", file_name))
  }
  
  return(p)
  
  # Possible improvememts:
  # Add interactive plotly with more info when you hover over each role
  
}


####################################################################
#' Density plot for discrete and continuous values
#' 
#' This function plots discrete and continuous values results
#' 
#' @param df Dataframe Event, role, label, or row.
#' @param var Variable to group, count and plot
#' @param table Boolean. Print results as table?
#' @param save Boolean. Save the output plot in our working directory
#' @param subdir Character. Into which subdirectory do you wish to save the plot to?
#' @export
gg_pie <- function(df, var, table = FALSE, save = FALSE, subdir = NA){
  
  variable <- enquo(var)
  
  title <- paste("Pie chart for", as.character(variable)[2])
  caption <- paste("Obs:", formatNum(nrow(df),0))
  
  n <- df %>% freqs(!!!variable)
  
  if(nrow(n) > 6){
    geom_label <- function(...){
      ggrepel::geom_label_repel(...)
    }
  } 
  
  if (table) { print(n) }
  
  p <- ggplot(n, aes(x = NULL, y = reorder(p, n), 
                     fill = as.character(!!!variable), label = p)) + 
    geom_col() + 
    geom_label(position = position_stack(vjust = 0.4), 
               show.legend = FALSE, size = 2.5) + 
    coord_polar("y") +
    labs(title = title, caption = caption) +
    theme_minimal() + 
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "bottom") +
    scale_fill_brewer(palette="Set3")
  
  # Export file name and folder for plot
  if (save == TRUE) {
    file_name <- paste0("viz_pie_",as.character(variable)[2],".png")
    if (!is.na(subdir)) {
      options(warn=-1)
      dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep="/")
    }
    p <- p + ggsave(file_name, width = 8, height = 6)
    message(paste("Saved plot as", file_name))
  }
  return(p)
}


####################################################################
#' Chords Plot
#' 
#' This function plots discrete and continuous values results
#' 
#' @param origin,dest Vectors. Origin and destination vectors
#' @param weight Vector. Weight for each chor
#' @param mg Numeric. Margin adjust for plot in case of need'
#' @param title Character. Title for the plot
#' @param subtitle Character. Subtitle for the plot
#' @param pal Vector. Colour pallete. Order matters.
#' @export
plot_chord <- function(origin, dest, weight = 1, mg = 3, 
                       title = "Chord Diagram",
                       subtitle = "", pal = NA) {
  
  try_require("circlize")
  
  if (length(origin) != length(dest)) {
    stop("The origin and dest vectors should have the same length!")
  }
  
  df <- data.frame(origin, dest, weight) %>%
    mutate(origin = ifelse(origin == "", " ", as.character(origin)),
           dest = ifelse(dest == "", " ", as.character(dest))) %>%
    replaceall(NA, "NA")
  colnames(df) <- c("orig_reg", "dest_reg", "flow")
  uniq <- unique(c(as.character(df$orig_reg), as.character(df$dest_reg)))
  
  if (is.na(pal)) pal <- names(lares_pal()$palette)
  
  if (length(unique(origin)) > length(pal)) {
    stop("Too many chords to plot and not enough colours :(")
  }
  
  col <- c(pal[1:length(unique(origin))], 
           rep("darkgrey", length(unique(uniq)) - length(unique(origin))))
  
  chordDiagram(x = df, 
               grid.col = col,
               transparency = 0.2, directional = 1,
               preAllocateTracks = list(track.height = uh(mg, "mm"), 
                                        track.margin = c(uh(mg, "mm"), 0)),
               direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
               annotationTrack = c("grid", "axis"), annotationTrackHeight = c(0.05, 0.1),
               link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
  
  title(main = title, line = -1, sub = subtitle, font.sub = 3, family = "Arial Narrow")
  legend("bottomright", pch = 15, col = col, legend = unique(origin), 
         bg = "transparent", box.lty = 0, cex = 0.8)
  
}


####################################################################
#' Quick Nice Bar Plot
#' 
#' This function uses a nice template for barplots.
#' 
#' @param names Character Vector. Bar names
#' @param n,p Numeric Vectors. n for counter, p to force percentage.
#' @param title,subtitle,axis Character. Texts for plot
#' @param obs Boolean. Show observations counter?
#' @param limit Integer. Limit n most frequent values only
#' @param na.rm Boolean. Remove empty and NAs?
#' @export
gg_bars <- function(names, n, p = NA, 
                    title = NA, subtitle = NA, axis = "Counter", 
                    obs = TRUE, 
                    limit = 15, na.rm = FALSE) {
  
  dfn <- data.frame(names, count = n) %>% arrange(desc(count), names)
  
  
  if (na.rm == TRUE) {
    dfn <- dfn %>% filter(!is.na(names), names != "")
  }
  
  if (class(n) == "integer") {
    dfn <- dfn %>% 
      mutate(p = ifelse(!is.na(p), p, 100*count/sum(count)),
             labels = paste0(formatNum(count, 0)," (", signif(p, 3), "%)"))
  } else {
    dfn <- dfn %>% mutate(p = count, labels = signif(count, 3))
  }
  
  if (nrow(dfn) >= limit) {
    dfn <- head(dfn, limit)
    subtitle <- paste(limit, "most frequent results")
  }
  
  dfn <- dfn %>%
    mutate(label_colours = ifelse(p > mean(range(p)) * 0.9, "m", "f"),
           label_hjust = ifelse(count < min(count) + diff(range(count)) * 0.35, -0.1, 1.05)) %>%
    mutate(label_colours = ifelse(label_colours == "m" & label_hjust < 0.35, "f", label_colours))
  
  p <- ggplot(dfn, aes(x = reorder(names, count), y = count, label = labels, fill = p)) +
    geom_col(alpha = 0.9, width = 0.8) +
    geom_text(aes(hjust = label_hjust, colour = label_colours), size = 3) + 
    coord_flip() + guides(colour = FALSE, fill = FALSE) +
    labs(x = NULL, y = axis, 
         title = if (!is.na(title)) title, 
         subtitle = if (!is.na(subtitle)) subtitle, 
         caption = if (obs == TRUE) paste0("Obs.: ", formatNum(sum(n), 0))) +
    theme_lares2(legend = "right") + gg_text_customs() +
    scale_fill_gradient(low = "lightskyblue2", high = "navy")
  return(p)
}


####################################################################
#' Axis scales format
#'
#' The `_comma` ones set comma format for axis text, the `_percent` 
#' ones set percent format for axis text, and `_dollar` for collar currency 
#'
#' @inheritDotParams ggplot2::continuous_scale -expand -position
#' @export
scale_x_comma <- function(...) scale_x_continuous(..., labels = scales::comma)

#' @rdname scale_x_comma
#' @export
scale_y_comma <- function(...) scale_y_continuous(..., labels = scales::comma)

#' @rdname scale_x_comma
#' @export
scale_x_percent <- function(...) scale_x_continuous(..., labels = scales::percent)

#' @rdname scale_x_comma
#' @export
scale_y_percent <- function(...) scale_y_continuous(..., labels = scales::percent)

#' @rdname scale_x_comma
#' @export
scale_x_dollar <- function(...) scale_y_continuous(..., labels = scales::dollar)

#' @rdname scale_x_comma
#' @export
scale_y_dollar <- function(...) scale_y_continuous(..., labels = scales::dollar)
