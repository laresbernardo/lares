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
  maxdate <- as.Date(max(df$End))
  p <- ggplot(cvlong, aes(x=value, y=reorder(name, -pos), label=where)) + 
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
