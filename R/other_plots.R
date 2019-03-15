####################################################################
#' Density plot for discrete and continuous values
#' 
#' This function plots discrete and continuous values results
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
  options(warn=-1)
  
  # Let's gather all the data
  df <- data.frame(
    Role = as.character(event), 
    Place = as.character(label), 
    Start = lubridate::date(start), 
    End = lubridate::date(end),
    Type = group)

  # Duplicate data for ggplot's geom_lines
  cvlong <- data.frame(
    pos = rep(as.numeric(rownames(df)),2),
    name = rep(as.character(df$Role),2),
    type = rep(as.character(df$Type),2),
    where = rep(as.character(df$Place),2),
    value = c(df$Start, df$End),
    label_pos = rep(df$Start + floor((df$End-df$Start)/2) , 2))

  # Plot timeline
  maxdate <- max(df$End)
  p <- ggplot(cvlong, aes(x=value, y=reorder(name, -pos), label=where, group=pos)) + 
    geom_vline(xintercept = maxdate, alpha = 0.8, linetype="dotted") +
    labs(title = title, subtitle = subtitle, 
         x = "", y = "", colour = "") +
    theme(panel.background = element_rect(fill="white", colour=NA),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_line(size=0.25, colour="grey80"))
  
  if (!is.na(cvlong$type) | length(unique(cvlong$type)) > 1) {
    p <- p + geom_line(aes(colour=type), size = size) +
      facet_grid(type ~ ., scales = "free", space= "free") +
      guides(colour = FALSE) +
      scale_colour_brewer(palette="Set1")
  } else {
    p <- p + geom_line(size = size, colour=colour)
  }
  
  p <- p + geom_label(aes(x = label_pos), colour = "black", size = 2, alpha = 0.7)
  
  # Export file name and folder for plot
  if (save == TRUE) {
    file_name <- "cv_timeline.png"
    if (!is.na(subdir)) {
      options(warn=-1)
      dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep="/")
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
  
  p <- ggplot(n, aes(x = "", y = reorder(p, n), 
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
plot_chord <- function(origin, dest, weight = 1, mg = 7, 
                       title = "Chord Diagram",
                       subtitle = "", pal = NA) {
  
  # require(circlize)
  
  if (length(origin) != length(dest)) {
    stop("The origin and dest vectors should have the same length!")
  }
  
  df <- data.frame(origin, dest, weight) %>%
    mutate(origin = ifelse(origin == "", " ", as.character(origin)),
           dest = ifelse(dest == "", " ", as.character(dest)))
  colnames(df) <- c("orig_reg", "dest_reg", "flow")
  uniq <- unique(c(as.character(df$orig_reg), as.character(df$dest_reg)))
  
  if (is.na(pal)) {
    pal <- names(lares::lares_pal()$palette)
  }
  
  if (length(unique(origin)) > length(pal)) {
    stop("Too many chords to plot and not enough colurs :(")
  }
  
  chordDiagram(x = df, 
               grid.col = c(pal[1:length(unique(origin))], 
                            rep("darkgrey", length(unique(uniq))-length(unique(origin)))),
               transparency = 0.2, directional = 1,
               preAllocateTracks = list(track.height = uh(mg, "mm"), 
                                        track.margin = c(uh(mg, "mm"), 0)),
               direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
               annotationTrack = c("grid", "axis"), annotationTrackHeight = c(0.05, 0.1),
               link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)
  
  circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
                facing = "clockwise", niceFacing = TRUE, adj = c(0.4, 0.5))
  }, bg.border = NA)
  
  title(main = title, line = -1, sub = subtitle, font.sub = 3, family = "Arial Narrow")
  
}
