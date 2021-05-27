####################################################################
#' Plot timeline as Gantt Plot
#' 
#' This function plots groups of observartions with timelines in a 
#' Gantt Plot way. Only works if start and end are date format values.
#' 
#' @family Visualization
#' @param event Vector. Event, role, label, or row.
#' @param start Vector. Start date.
#' @param end Vector. End date. Only one day be default if not defined
#' @param label Vector. Place, institution, or label.
#' @param group Vector. Academic, Work, Extracurricular... Pass as factor
#' to keep a specific order
#' @param title Character. Title for the plot
#' @param subtitle Character. Subtitle for the plot
#' @param interactive Boolean. Run with plotly?
#' @param save Boolean. Save the output plot in our working directory
#' @param subdir Character. Into which subdirectory do you wish to save the plot to?
#' @return ggplot2 object
#' @examples 
#' Sys.unsetenv("LARES_FONT") # Temporal
#' cols <- c("Role", "Place", "Type", "Start", "End")
#' today <- as.character(Sys.Date())
#' cv <- data.frame(rbind(
#'   c("Marketing Science Partner", "Facebook", "Work Experience", "2019-12-09", today),
#'   c("Data Scientist Consultant", "MatrixDS", "Work Experience", "2018-09-01", today),
#'   c("R Community Contributor", "lares library", "Extra", "2018-07-18", today),
#'   c("Lead Data Scientist", "MEG", "Work Experience", "2019-01-15", "2019-12-09"),
#'   c("Head Data Science & Analytics","Comparamejor/R5","Work Experience","2016-08-01","2019-01-15"),
#'   c("Big Data & Data Science Programme", "UdC", "Academic", "2017-09-01", "2018-02-28"),
#'   c("Project Engineer", "Polytex", "Work Experience", "2016-05-15", "2016-09-01"),
#'   c("Big Data Analyst", "MEG", "Work Experience", "2016-01-01", "2016-04-30"),
#'   c("Advanced Excel Instructor", "ARTS", "Work Experience", "2015-11-01", "2016-04-30"),
#'   c("Continuous Improvement Intern", "PAVCO", "Work Experience", "2015-04-01", "2015-08-30"),
#'   c("Mechanical Design Intern", "SIGALCA", "Work Experience", "2013-07-01", "2013-09-30"),
#'   c("DJs Online Community Owner","LaresDJ.com / SoloParaDJs","Extra","2010-01-05","2020-05-20"),
#'   c("Mechanical Engineer Degree", "USB", "Academic", "2009-09-15", "2015-11-20"),
#'   c("DJ and Composer/Producer", "Legacy Discplay", "Extra", "2009-05-01", "2015-04-30")
#' ))
#' colnames(cv) <- cols
#' plot_timeline(event = cv$Role, 
#'               start = cv$Start, 
#'               end = cv$End, 
#'               label = cv$Place, 
#'               # Simple trick to re-arrange the grids
#'               group = factor(cv$Type, levels = c("Work Experience", "Academic", "Extra")))
#' @export
plot_timeline <- function(event, 
                          start, end = start + 1, 
                          label = NA, group = NA, 
                          title = "Curriculum Vitae Timeline", 
                          subtitle = "Bernardo Lares",
                          interactive = FALSE,
                          save = FALSE,
                          subdir = NA) {
  
  # Let's gather all the data
  df <- data.frame(
    Role = as.character(event), 
    Place = as.character(label), 
    Start = as.Date(as.character(start)), 
    End = as.Date(as.character(end)),
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
  p <- ggplot(cvlong, aes(x = .data$value, 
                          y = reorder(.data$name, -.data$pos), 
                          label = .data$where, 
                          group = .data$pos)) + 
    geom_vline(xintercept = maxdate, alpha = 0.2) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL, colour = NULL) +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_line(size = 0.25, colour = "grey80"))
  #scale_x_date(expand = c(0, 0))
  
  if (!is.na(cvlong$type)[1] | length(unique(cvlong$type)) > 1) {
    p <- p + geom_line(aes(colour = .data$type), size = 7) +
      facet_grid(.data$type ~ ., scales = "free", space = "free") +
      guides(colour = FALSE)
  }
  
  p <- p + 
    geom_label(aes(x = .data$label_pos), colour = "black", size = 2, alpha = 0.7) +
    theme_lares(pal = 2, legend = "none")
  
  # Export file name and folder for plot
  if (save) {
    file_name <- "cv_timeline.png"
    if (!is.na(subdir)) {
      #dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep = "/")
    }
    p <- p + ggsave(file_name, width = 8, height = 6)
    message(paste("Saved plot as", file_name))
  }
  
  if (interactive) {
    try_require("plotly")
    p <- ggplotly(p)
  }
  
  return(p)
  
}


####################################################################
#' Density plot for discrete and continuous values
#' 
#' This function plots discrete and continuous values results
#' 
#' @family Visualization
#' @param df Dataframe
#' @param var Variable to group, count and plot
#' @param table Boolean. Print results as table?
#' @param ... Further parameters passed to \code{freqs()}
#' @return ggplot2 object
#' @examples 
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dft) # Titanic dataset
#' gg_pie(dft, Survived)
#' gg_pie(dft, Pclass, table = TRUE)
#' gg_pie(dft, SibSp, table = TRUE, abc = TRUE)
#' @export
gg_pie <- function(df, var, table = FALSE, ...){
  
  variable <- enquo(var)
  
  title <- paste("Pie chart for", as.character(variable)[2])
  caption <- paste("Obs:", formatNum(nrow(df),0))
  
  n <- df %>% freqs(!!!variable, ...)
  
  if (nrow(n) > 6) {
    geom_label <- function(...){
      ggrepel::geom_label_repel(...)
    }
  } 
  
  if (table) { print(n) }
  
  p <- ggplot(n, aes(x = "", y = reorder(.data$p, -.data$order), 
                     fill = as.character(!!!variable), 
                     label = .data$p)) + 
    geom_col() + 
    geom_label(position = position_stack(vjust = 0.4), 
               show.legend = FALSE, size = 2.5) + 
    coord_polar("y") +
    labs(title = title, caption = caption, x = NULL, y = NULL) +
    theme_lares(pal = 1) + 
    theme(legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "bottom")
  return(p)
}


####################################################################
#' Chords Plot
#' 
#' This auxiliary function plots discrete and continuous values results
#' 
#' @family Visualization
#' @param origin,dest Vectors. Origin and destination vectors
#' @param weight Vector. Weight for each chord.
#' @param mg Numeric. Margin adjust for plot in case of need
#' @param title Character. Title for the plot
#' @param subtitle Character. Subtitle for the plot
#' @param pal Vector. Colour pallete. Order matters.
#' @return chordDiagram object
#' @examples 
#' # You must have \code{circlize} library to use this auxiliary function:
#' \dontrun{
#' df <- data.frame(from = c(1, 1, 2, 3, 4, 1, 6), to = c(4, 4, 4, 2, 2, NA, NA))
#' plot_chord(df$from, df$to)
#' }
#' @export
plot_chord <- function(origin, dest, 
                       weight = 1, mg = 3, 
                       title = "Chord Diagram",
                       subtitle = "", pal = NA) {
  
  try_require("circlize")
  
  if (length(origin) != length(dest)) {
    stop("The origin and dest vectors should have the same length!")
  }
  
  df <- data.frame(origin, dest, weight) %>%
    mutate(origin = ifelse(.data$origin == "", " ", as.character(.data$origin)),
           dest = ifelse(.data$dest == "", " ", as.character(.data$dest))) %>%
    replaceall(NA, "NA")
  colnames(df) <- c("orig_reg", "dest_reg", "flow")
  uniq <- unique(c(as.character(df$orig_reg), as.character(df$dest_reg)))
  
  if (is.na(pal)) pal <- names(lares_pal()$palette)
  
  if (length(unique(origin)) > length(pal)) {
    stop("Too many chords to plot and not enough colours :(")
  }
  
  col <- c(pal[seq_along(unique(origin))], 
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
#' @family Visualization
#' @param names Character Vector. Bar names
#' @param n,p Numeric Vectors. n for counter, p to force percentage.
#' @param title,subtitle,axis Character. Texts for plot
#' @param obs Boolean. Show observations counter?
#' @param limit Integer. Limit n most frequent values only
#' @param na.rm Boolean. Remove empty and NAs?
#' @return ggplot2 object
#' @examples 
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dft) # Titanic dataset
#' df <- freqs(dft, Pclass)
#' gg_bars(df$Pclass, n = df$n)
#' gg_bars(df$Pclass, n = df$n, p = df$p, axis = "Percentage of ...")
#' @export
gg_bars <- function(names, n, p = NA, 
                    title = NA, 
                    subtitle = NA, 
                    axis = "Counter", 
                    obs = TRUE, 
                    limit = 15, 
                    na.rm = FALSE) {
  
  dfn <- data.frame(names, count = n) %>% 
    arrange(desc(.data$count), .data$names)
  
  
  if (na.rm == TRUE) {
    dfn <- filter(dfn, !is.na(.data$names), .data$names != "")
  }
  
  if (class(n) == "integer") {
    dfn <- dfn %>% 
      mutate(p = ifelse(!is.na(p), p, 100*.data$count/sum(.data$count)),
             labels = paste0(formatNum(.data$count, 0)," (", signif(.data$p, 3), "%)"))
  } else {
    dfn <- dfn %>% 
      mutate(p = .data$count, labels = signif(.data$count, 3))
  }
  
  if (nrow(dfn) >= limit) {
    dfn <- head(dfn, limit)
    subtitle <- paste(limit, "most frequent results")
  }
  
  dfn <- dfn %>%
    mutate(label_colours = ifelse(p > mean(range(.data$p)) * 0.9, "m", "f"),
           label_hjust = ifelse(.data$count < min(.data$count) + 
                                  diff(range(.data$count)) * 0.35, -0.1, 1.05)) %>%
    mutate(label_colours = ifelse(.data$label_colours == "m" & 
                                    .data$label_hjust < 0.35, "f", .data$label_colours))
  
  p <- ggplot(dfn, aes(x = reorder(.data$names, .data$count), 
                       y = .data$count, label = .data$labels, fill = .data$p)) +
    geom_col(alpha = 0.9, width = 0.8) +
    geom_text(aes(hjust = .data$label_hjust, colour = .data$label_colours), size = 3) + 
    coord_flip() + guides(colour = FALSE, fill = FALSE) +
    labs(x = NULL, y = axis, 
         title = if (!is.na(title)) title, 
         subtitle = if (!is.na(subtitle)) subtitle, 
         caption = if (obs == TRUE) paste0("Obs.: ", formatNum(sum(n), 0))) +
    theme_lares(legend = "right") + gg_text_customs() +
    scale_fill_gradient(low = "lightskyblue2", high = "navy")
  return(p)
}


####################################################################
#' Axis scales format
#'
#' The \code{_comma} ones set comma format for axis text, the \code{_percent} 
#' ones set percent format for axis text, \code{_dollar} for collar currency,
#' and \code{_abbr} for abbreviated format. Lastly, use \code{_formatNum} to
#' further customize your numerical scales with \code{lares::formatNum}.
#'
#' @param ... Arguments passed to \code{ggplot2::continuous_scale} or
#' \code{lares::formatNum} depending on the function.
#' @return Reformatted scales on ggplot2 object
#' @examples 
#' library(ggplot2)
#' df <- ggplot2::txhousing %>% removenarows(all = FALSE)
#' 
#' ggplot(df, aes(x = sales, y = volume)) + geom_point() +
#'   scale_x_dollar() + scale_y_abbr()
#'   
#' # Use any argument from scale_x/y_continuous
#' ggplot(df, aes(x = listings, y = log(inventory))) + geom_point() +
#'   scale_x_comma() + scale_y_percent(limits = c(0, 3))
#'   
#' # Use any argument from scale_x/y_continuous AND formatNum
#' ggplot(df, aes(x = median, y = inventory)) + geom_point() +
#'   scale_x_formatNum(n.breaks = 3, pre = "@", abbr = TRUE) +
#'   scale_y_formatNum(position = "right", decimals = 0, pos = " X")
#' @export
scale_x_comma <- function(...) scale_x_continuous(..., labels = comma)

#' @rdname scale_x_comma
#' @export
scale_y_comma <- function(...) scale_y_continuous(..., labels = comma)

#' @rdname scale_x_comma
#' @export
scale_x_percent <- function(...) scale_x_continuous(..., labels = percent)

#' @rdname scale_x_comma
#' @export
scale_y_percent <- function(...) scale_y_continuous(..., labels = percent)

#' @rdname scale_x_comma
#' @export
scale_x_dollar <- function(...) scale_x_continuous(..., labels = dollar)

#' @rdname scale_x_comma
#' @export
scale_y_dollar <- function(...) scale_y_continuous(..., labels = dollar)

#' @rdname scale_x_comma
#' @export
scale_x_abbr <- function(...) scale_x_continuous(..., labels = num_abbr)

#' @rdname scale_x_comma
#' @export
scale_y_abbr <- function(...) scale_y_continuous(..., labels = num_abbr)

#' @rdname scale_x_comma
#' @inheritParams formatNum
#' @export
scale_x_formatNum <- function(..., decimals = 2, signif = NULL,
                              type = Sys.getenv("LARES_NUMFORMAT"),
                              pre = "", pos = "", sign = FALSE, abbr = FALSE) {
  scale_x_continuous(..., labels = function(x)
    formatNum(x, decimals = decimals, signif = signif, type = type,
              pre = pre, pos = pos, sign = sign, abbr = abbr))
}

#' @rdname scale_x_comma
#' @export
scale_y_formatNum <- function(..., decimals = 2, signif = NULL,
                              type = Sys.getenv("LARES_NUMFORMAT"),
                              pre = "", pos = "", sign = FALSE, abbr = FALSE) {
  scale_y_continuous(..., labels = function(x)
    formatNum(x, decimals = decimals, signif = signif, type = type,
              pre = pre, pos = pos, sign = sign, abbr = abbr))
}


####################################################################
#' Plot Result with Nothing to Plot
#' 
#' This function lets the user print a plot without plot, with a 
#' customizable message. It is quite useful for Shiny renderPlot when
#' using filters and no data is returned.
#' 
#' @family Visualization
#' @param message Character. What message do you wish to show?
#' @param size Numeric. Text size.
#' @param font Character. Font name
#' @return Empty ggplot2 object (with a \code{message} if set).
#' @examples 
#' Sys.unsetenv("LARES_FONT") # Temporal
#' noPlot(message = "No plot to show!")
#' @export
noPlot <- function(message = "Nothing to show here!", size = 4,
                   font = Sys.getenv("LARES_FONT")) {
  
  ggplot(data.frame(), aes(x = 0, y = 0, label = message)) + 
    theme_lares(font = font) + theme_minimal() +
    geom_text(size = size) +
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          plot.margin = margin(0, 0, 0, 0))
}


####################################################################
#' Export ggplot2, gridExtra, or any plot object into rendered file
#' 
#' Export any \code{ggplot2}, \code{gridExtra}, or any plot object 
#' created with R into rendered \code{png} or \code{jpg} file.
#' 
#' @family Tools
#' @param p Plot object. Plot to render and export.
#' @param name Character. File's name or suffix if vars is not \code{NA}. No need
#' to include file format on file name.
#' @param vars Vector. Variable names to identify by filename.
#' @param sep Character. Separator for \code{vars}.
#' @param format Character. One of: \code{png} or \code{jpeg}.
#' @param width,height,res Numeric. Plot's width, height, and res (for grids).
#' @param dir,subdir Character. In which directory/subdirectory do you 
#' wish to save the plot? Working directory as default \code{dir}.
#' @param quiet Boolean. Display successful message with filename when saved?
#' @return No return value, called for side effects.
#' @examples
#' \donttest{
#' p <- noPlot()
#' export_plot(p, name = "noplot", width = 10, height = 8, res = 300, dir = tempdir())
#' export_plot(p, name = "noplot2", subdir = "newplots", dir = tempdir())
#' }
#' @export
export_plot <- function(p, 
                        name = "plot", vars = NA, sep = ".vs.", 
                        width = 8, height = 6, 
                        format = "png", res = 300,
                        dir = getwd(), subdir = NA,
                        quiet = FALSE) {
  
  check_opts(format, c("png","jpeg"))
  
  # File name
  name <- sub('\\..[^\\.]*$', '', name)
  end <- paste0(".", format)
  if (!is.na(vars)) {
    names <- v2t(cleanText(as.character(vars), spaces = FALSE), sep = sep, quotes = FALSE)
    file_name <- paste0(name, "_", names, end)  
  } else {
    file_name <- paste0(name, end)  
  }
  
  # Create directory if needed
  if (!is.na(subdir)) {
    dir <- file.path(dir, subdir)
    if (!dir.exists(dir)) dir.create(dir)
    file_name <- paste(subdir, file_name, sep = "/")
  }
  
  # Export plot to file
  # if (!"patchwork" %in% class(p) & "ggplot" %in% class(p)) {
  #   ggsave(filename = file_name, plot = p, device = format, height = height, width = width, dpi = res)
  export_fx <- base::get(format)
  export_fx(file_name, height = height * res, width = width * res, res = res)
  plot(p)
  dev.off()
  
  if (!quiet) message(paste("Plot saved as", file_name)) 
  
}####################################################################
#' Plot timeline as Gantt Plot
#' 
#' This function plots groups of observartions with timelines in a 
#' Gantt Plot way. Only works if start and end are date format values.
#' 
#' @family Visualization
#' @param event Vector. Event, role, label, or row.
#' @param start Vector. Start date.
#' @param end Vector. End date. Only one day be default if not defined
#' @param label Vector. Place, institution, or label.
#' @param group Vector. Academic, Work, Extracurricular... Pass as factor
#' to keep a specific order
#' @param title Character. Title for the plot
#' @param subtitle Character. Subtitle for the plot
#' @param interactive Boolean. Run with plotly?
#' @param save Boolean. Save the output plot in our working directory
#' @param subdir Character. Into which subdirectory do you wish to save the plot to?
#' @return ggplot2 object
#' @examples 
#' Sys.unsetenv("LARES_FONT") # Temporal
#' cols <- c("Role", "Place", "Type", "Start", "End")
#' today <- as.character(Sys.Date())
#' cv <- data.frame(rbind(
#'   c("Marketing Science Partner", "Facebook", "Work Experience", "2019-12-09", today),
#'   c("Data Scientist Consultant", "MatrixDS", "Work Experience", "2018-09-01", today),
#'   c("R Community Contributor", "lares library", "Extra", "2018-07-18", today),
#'   c("Lead Data Scientist", "MEG", "Work Experience", "2019-01-15", "2019-12-09"),
#'   c("Head Data Science & Analytics","Comparamejor/R5","Work Experience","2016-08-01","2019-01-15"),
#'   c("Big Data & Data Science Programme", "UdC", "Academic", "2017-09-01", "2018-02-28"),
#'   c("Project Engineer", "Polytex", "Work Experience", "2016-05-15", "2016-09-01"),
#'   c("Big Data Analyst", "MEG", "Work Experience", "2016-01-01", "2016-04-30"),
#'   c("Advanced Excel Instructor", "ARTS", "Work Experience", "2015-11-01", "2016-04-30"),
#'   c("Continuous Improvement Intern", "PAVCO", "Work Experience", "2015-04-01", "2015-08-30"),
#'   c("Mechanical Design Intern", "SIGALCA", "Work Experience", "2013-07-01", "2013-09-30"),
#'   c("DJs Online Community Owner","LaresDJ.com / SoloParaDJs","Extra","2010-01-05","2020-05-20"),
#'   c("Mechanical Engineer Degree", "USB", "Academic", "2009-09-15", "2015-11-20"),
#'   c("DJ and Composer/Producer", "Legacy Discplay", "Extra", "2009-05-01", "2015-04-30")
#' ))
#' colnames(cv) <- cols
#' plot_timeline(event = cv$Role, 
#'               start = cv$Start, 
#'               end = cv$End, 
#'               label = cv$Place, 
#'               # Simple trick to re-arrange the grids
#'               group = factor(cv$Type, levels = c("Work Experience", "Academic", "Extra")))
#' @export
plot_timeline <- function(event, 
                          start, end = start + 1, 
                          label = NA, group = NA, 
                          title = "Curriculum Vitae Timeline", 
                          subtitle = "Bernardo Lares",
                          interactive = FALSE,
                          save = FALSE,
                          subdir = NA) {
  
  # Let's gather all the data
  df <- data.frame(
    Role = as.character(event), 
    Place = as.character(label), 
    Start = as.Date(as.character(start)), 
    End = as.Date(as.character(end)),
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
  p <- ggplot(cvlong, aes(x = .data$value, 
                          y = reorder(.data$name, -.data$pos), 
                          label = .data$where, 
                          group = .data$pos)) + 
    geom_vline(xintercept = maxdate, alpha = 0.2) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL, colour = NULL) +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_line(size = 0.25, colour = "grey80"))
  #scale_x_date(expand = c(0, 0))
  
  if (!is.na(cvlong$type)[1] | length(unique(cvlong$type)) > 1) {
    p <- p + geom_line(aes(colour = .data$type), size = 7) +
      facet_grid(.data$type ~ ., scales = "free", space = "free") +
      guides(colour = FALSE)
  }
  
  p <- p + 
    geom_label(aes(x = .data$label_pos), colour = "black", size = 2, alpha = 0.7) +
    theme_lares(pal = 2, legend = "none")
  
  # Export file name and folder for plot
  if (save) {
    file_name <- "cv_timeline.png"
    if (!is.na(subdir)) {
      #dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep = "/")
    }
    p <- p + ggsave(file_name, width = 8, height = 6)
    message(paste("Saved plot as", file_name))
  }
  
  if (interactive) {
    try_require("plotly")
    p <- ggplotly(p)
  }
  
  return(p)
  
}


####################################################################
#' Density plot for discrete and continuous values
#' 
#' This function plots discrete and continuous values results
#' 
#' @family Visualization
#' @param df Dataframe
#' @param var Variable to group, count and plot
#' @param table Boolean. Print results as table?
#' @param ... Further parameters passed to \code{freqs()}
#' @return ggplot2 object
#' @examples 
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dft) # Titanic dataset
#' gg_pie(dft, Survived)
#' gg_pie(dft, Pclass, table = TRUE)
#' gg_pie(dft, SibSp, table = TRUE, abc = TRUE)
#' @export
gg_pie <- function(df, var, table = FALSE, ...){
  
  variable <- enquo(var)
  
  title <- paste("Pie chart for", as.character(variable)[2])
  caption <- paste("Obs:", formatNum(nrow(df),0))
  
  n <- df %>% freqs(!!!variable, ...)
  
  if (nrow(n) > 6) {
    geom_label <- function(...){
      ggrepel::geom_label_repel(...)
    }
  } 
  
  if (table) { print(n) }
  
  p <- ggplot(n, aes(x = "", y = reorder(.data$p, -.data$order), 
                     fill = as.character(!!!variable), 
                     label = .data$p)) + 
    geom_col() + 
    geom_label(position = position_stack(vjust = 0.4), 
               show.legend = FALSE, size = 2.5) + 
    coord_polar("y") +
    labs(title = title, caption = caption, x = NULL, y = NULL) +
    theme_lares(pal = 1) + 
    theme(legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "bottom")
  return(p)
}


####################################################################
#' Chords Plot
#' 
#' This auxiliary function plots discrete and continuous values results
#' 
#' @family Visualization
#' @param origin,dest Vectors. Origin and destination vectors
#' @param weight Vector. Weight for each chord.
#' @param mg Numeric. Margin adjust for plot in case of need
#' @param title Character. Title for the plot
#' @param subtitle Character. Subtitle for the plot
#' @param pal Vector. Colour pallete. Order matters.
#' @return chordDiagram object
#' @examples 
#' # You must have \code{circlize} library to use this auxiliary function:
#' \dontrun{
#' df <- data.frame(from = c(1, 1, 2, 3, 4, 1, 6), to = c(4, 4, 4, 2, 2, NA, NA))
#' plot_chord(df$from, df$to)
#' }
#' @export
plot_chord <- function(origin, dest, 
                       weight = 1, mg = 3, 
                       title = "Chord Diagram",
                       subtitle = "", pal = NA) {
  
  try_require("circlize")
  
  if (length(origin) != length(dest)) {
    stop("The origin and dest vectors should have the same length!")
  }
  
  df <- data.frame(origin, dest, weight) %>%
    mutate(origin = ifelse(.data$origin == "", " ", as.character(.data$origin)),
           dest = ifelse(.data$dest == "", " ", as.character(.data$dest))) %>%
    replaceall(NA, "NA")
  colnames(df) <- c("orig_reg", "dest_reg", "flow")
  uniq <- unique(c(as.character(df$orig_reg), as.character(df$dest_reg)))
  
  if (is.na(pal)) pal <- names(lares_pal()$palette)
  
  if (length(unique(origin)) > length(pal)) {
    stop("Too many chords to plot and not enough colours :(")
  }
  
  col <- c(pal[seq_along(unique(origin))], 
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
#' @family Visualization
#' @param names Character Vector. Bar names
#' @param n,p Numeric Vectors. n for counter, p to force percentage.
#' @param title,subtitle,axis Character. Texts for plot
#' @param obs Boolean. Show observations counter?
#' @param limit Integer. Limit n most frequent values only
#' @param na.rm Boolean. Remove empty and NAs?
#' @return ggplot2 object
#' @examples 
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dft) # Titanic dataset
#' df <- freqs(dft, Pclass)
#' gg_bars(df$Pclass, n = df$n)
#' gg_bars(df$Pclass, n = df$n, p = df$p, axis = "Percentage of ...")
#' @export
gg_bars <- function(names, n, p = NA, 
                    title = NA, 
                    subtitle = NA, 
                    axis = "Counter", 
                    obs = TRUE, 
                    limit = 15, 
                    na.rm = FALSE) {
  
  dfn <- data.frame(names, count = n) %>% 
    arrange(desc(.data$count), .data$names)
  
  
  if (na.rm == TRUE) {
    dfn <- filter(dfn, !is.na(.data$names), .data$names != "")
  }
  
  if (class(n) == "integer") {
    dfn <- dfn %>% 
      mutate(p = ifelse(!is.na(p), p, 100*.data$count/sum(.data$count)),
             labels = paste0(formatNum(.data$count, 0)," (", signif(.data$p, 3), "%)"))
  } else {
    dfn <- dfn %>% 
      mutate(p = .data$count, labels = signif(.data$count, 3))
  }
  
  if (nrow(dfn) >= limit) {
    dfn <- head(dfn, limit)
    subtitle <- paste(limit, "most frequent results")
  }
  
  dfn <- dfn %>%
    mutate(label_colours = ifelse(p > mean(range(.data$p)) * 0.9, "m", "f"),
           label_hjust = ifelse(.data$count < min(.data$count) + 
                                  diff(range(.data$count)) * 0.35, -0.1, 1.05)) %>%
    mutate(label_colours = ifelse(.data$label_colours == "m" & 
                                    .data$label_hjust < 0.35, "f", .data$label_colours))
  
  p <- ggplot(dfn, aes(x = reorder(.data$names, .data$count), 
                       y = .data$count, label = .data$labels, fill = .data$p)) +
    geom_col(alpha = 0.9, width = 0.8) +
    geom_text(aes(hjust = .data$label_hjust, colour = .data$label_colours), size = 3) + 
    coord_flip() + guides(colour = FALSE, fill = FALSE) +
    labs(x = NULL, y = axis, 
         title = if (!is.na(title)) title, 
         subtitle = if (!is.na(subtitle)) subtitle, 
         caption = if (obs == TRUE) paste0("Obs.: ", formatNum(sum(n), 0))) +
    theme_lares(legend = "right") + gg_text_customs() +
    scale_fill_gradient(low = "lightskyblue2", high = "navy")
  return(p)
}


####################################################################
#' Axis scales format
#'
#' The \code{_comma} ones set comma format for axis text, the \code{_percent} 
#' ones set percent format for axis text, \code{_dollar} for collar currency,
#' and \code{_abbr} for abbreviated format. Lastly, use \code{_formatNum} to
#' further customize your numerical scales with \code{lares::formatNum}.
#'
#' @param ... Arguments passed to \code{ggplot2::continuous_scale} or
#' \code{lares::formatNum} depending on the function.
#' @return Reformatted scales on ggplot2 object
#' @examples 
#' library(ggplot2)
#' df <- ggplot2::txhousing %>% removenarows(all = FALSE)
#' 
#' ggplot(df, aes(x = sales, y = volume)) + geom_point() +
#'   scale_x_dollar() + scale_y_abbr()
#'   
#' # Use any argument from scale_x/y_continuous
#' ggplot(df, aes(x = listings, y = log(inventory))) + geom_point() +
#'   scale_x_comma() + scale_y_percent(limits = c(0, 3))
#'   
#' # Use any argument from scale_x/y_continuous AND formatNum
#' ggplot(df, aes(x = median, y = inventory)) + geom_point() +
#'   scale_x_formatNum(n.breaks = 3, pre = "@", abbr = TRUE) +
#'   scale_y_formatNum(position = "right", decimals = 0, pos = " X")
#' @export
scale_x_comma <- function(...) scale_x_continuous(..., labels = comma)

#' @rdname scale_x_comma
#' @export
scale_y_comma <- function(...) scale_y_continuous(..., labels = comma)

#' @rdname scale_x_comma
#' @export
scale_x_percent <- function(...) scale_x_continuous(..., labels = percent)

#' @rdname scale_x_comma
#' @export
scale_y_percent <- function(...) scale_y_continuous(..., labels = percent)

#' @rdname scale_x_comma
#' @export
scale_x_dollar <- function(...) scale_x_continuous(..., labels = dollar)

#' @rdname scale_x_comma
#' @export
scale_y_dollar <- function(...) scale_y_continuous(..., labels = dollar)

#' @rdname scale_x_comma
#' @export
scale_x_abbr <- function(...) scale_x_continuous(..., labels = num_abbr)

#' @rdname scale_x_comma
#' @export
scale_y_abbr <- function(...) scale_y_continuous(..., labels = num_abbr)

#' @rdname scale_x_comma
#' @inheritParams formatNum
#' @export
scale_x_formatNum <- function(..., decimals = 2, signif = NULL,
                              type = Sys.getenv("LARES_NUMFORMAT"),
                              pre = "", pos = "", sign = FALSE, abbr = FALSE) {
  scale_x_continuous(..., labels = function(x)
    formatNum(x, decimals = decimals, signif = signif, type = type,
              pre = pre, pos = pos, sign = sign, abbr = abbr))
}

#' @rdname scale_x_comma
#' @export
scale_y_formatNum <- function(..., decimals = 2, signif = NULL,
                              type = Sys.getenv("LARES_NUMFORMAT"),
                              pre = "", pos = "", sign = FALSE, abbr = FALSE) {
  scale_y_continuous(..., labels = function(x)
    formatNum(x, decimals = decimals, signif = signif, type = type,
              pre = pre, pos = pos, sign = sign, abbr = abbr))
}


####################################################################
#' Plot Result with Nothing to Plot
#' 
#' This function lets the user print a plot without plot, with a 
#' customizable message. It is quite useful for Shiny renderPlot when
#' using filters and no data is returned.
#' 
#' @family Visualization
#' @param message Character. What message do you wish to show?
#' @param size Numeric. Text size.
#' @param font Character. Font name
#' @return Empty ggplot2 object (with a \code{message} if set).
#' @examples 
#' Sys.unsetenv("LARES_FONT") # Temporal
#' noPlot(message = "No plot to show!")
#' @export
noPlot <- function(message = "Nothing to show here!", size = 4,
                   font = Sys.getenv("LARES_FONT")) {
  
  ggplot(data.frame(), aes(x = 0, y = 0, label = message)) + 
    theme_lares(font = font) + theme_minimal() +
    geom_text(size = size) +
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          plot.margin = margin(0, 0, 0, 0))
}


####################################################################
#' Export ggplot2, gridExtra, or any plot object into rendered file
#' 
#' Export any \code{ggplot2}, \code{gridExtra}, or any plot object 
#' created with R into rendered \code{png} or \code{jpg} file.
#' 
#' @family Tools
#' @param p Plot object. Plot to render and export.
#' @param name Character. File's name or suffix if vars is not \code{NA}. No need
#' to include file format on file name.
#' @param vars Vector. Variable names to identify by filename.
#' @param sep Character. Separator for \code{vars}.
#' @param format Character. One of: \code{png} or \code{jpeg}.
#' @param width,height,res Numeric. Plot's width, height, and res (for grids).
#' @param dir,subdir Character. In which directory/subdirectory do you 
#' wish to save the plot? Working directory as default \code{dir}.
#' @param quiet Boolean. Display successful message with filename when saved?
#' @return No return value, called for side effects.
#' @examples
#' \donttest{
#' p <- noPlot()
#' export_plot(p, name = "noplot", width = 10, height = 8, res = 300, dir = tempdir())
#' export_plot(p, name = "noplot2", subdir = "newplots", dir = tempdir())
#' }
#' @export
export_plot <- function(p, 
                        name = "plot", vars = NA, sep = ".vs.", 
                        width = 8, height = 6, 
                        format = "png", res = 300,
                        dir = getwd(), subdir = NA,
                        quiet = FALSE) {
  
  check_opts(format, c("png","jpeg"))
  
  # File name
  name <- sub('\\..[^\\.]*$', '', name)
  end <- paste0(".", format)
  if (!is.na(vars)) {
    names <- v2t(cleanText(as.character(vars), spaces = FALSE), sep = sep, quotes = FALSE)
    file_name <- paste0(name, "_", names, end)  
  } else {
    file_name <- paste0(name, end)
  }
  
  # Create directory if needed
  if (!is.na(subdir)) {
    dir <- file.path(dir, subdir)
    if (!dir.exists(dir)) dir.create(dir)
  }
  
  # Full path file name
  file_name <- paste(dir, file_name, sep = "/")
  
  # Export plot to file
  # if (!"patchwork" %in% class(p) & "ggplot" %in% class(p)) {
  #   ggsave(filename = file_name, plot = p, device = format, height = height, width = width, dpi = res)
  export_fx <- base::get(format)
  export_fx(file_name, height = height * res, width = width * res, res = res)
  plot(p)
  dev.off()
  
  if (!quiet) message(paste("Plot saved as", file_name)) 
  
}
