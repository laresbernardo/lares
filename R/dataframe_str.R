####################################################################
#' Dataset columns and rows structure
#' 
#' This function lets the user to check quickly the structure of a
#' dataset (data.frame). It returns multiple counters for useful metrics,
#' a plot, and a list of column names for each of the column metrics.
#' 
#' @param df Dataframe
#' @param return Character. Return "skimr" for skim results, "numbers" for
#' numbers, or "names" for column names of each of the cateogries
#' @param plot Boolean. Do you wish to see a plot?
#' @param subtitle Character. Add subtitle to plot
#' @export
df_str <- function (df, 
                    return = "skimr", 
                    plot = TRUE, 
                    subtitle = ""){
  
  # require(dplyr)
  # require(ggplot2)
  
  df <- data.frame(df)
  
  names <- list(
    cols = colnames(df),
    nums = colnames(df)[unlist(lapply(df, is.numeric))],
    char = colnames(df)[unlist(lapply(df, is.character))],
    factor = colnames(df)[unlist(lapply(df, is.factor))],
    logic = colnames(df)[unlist(lapply(df, is.logical))])
  names[["time"]] <- names$cols[!colnames(df) %in% c(
    names$nums, names$char, names$factor, names$logic)]
  names[["allnas"]] <- names$cols[sapply(df, function(x) all(is.na(x)))]

  numbers <- data.frame(
    "Total Observations" = nrow(df) * ncol(df),
    "Total Rows" = nrow(df),
    "Total Columns" = ncol(df),
    "Numeric Columns" = length(names$nums),
    "Character Columns" = length(names$char),
    "Factor Columns" = length(names$factor),
    "Logical Columns" = length(names$logic),
    "Time/Date Columns" = length(names$time),
    "All Missing Columns" = length(names$allnas),
    "Missing Values" = sum(is.na(df)),
    "Complete Rows" = sum(complete.cases(df)),
    "Memory Usage" = as.numeric(object.size(df))
  )
  
  intro2 <- data.frame(counter = t(numbers)) %>%
    mutate(metric = row.names(.),
           type = ifelse(grepl("Column", colnames(numbers)), "Columns",
                         ifelse(grepl("Rows", colnames(numbers)), "Rows", "Observations")),
           p = ifelse(type == "Columns", 100*counter/numbers$Total.Columns,
                      ifelse(type == "Rows", 100*counter/numbers$Total.Rows, 
                             100*counter/numbers$Total.Observations)),
           p = round(p,2),
           type = factor(type, levels = c("Observations","Columns","Rows"))) %>%
    select(metric, counter, type, p)
  
  if (plot == TRUE) {
    plot <- intro2 %>%
      filter(!metric %in% c("Memory.Usage")) %>%
      ggplot(aes(x=reorder(metric, as.integer(counter)), 
                 y=p, fill=type, 
                 label=lares::formatNum(counter, 0))) + 
      geom_col() + coord_flip() + 
      theme_minimal() + guides(fill=FALSE) +
      labs(title = "Dataset columns and rows counter", 
           subtitle = subtitle,
           x = "", y = "% of total", fill="", 
           caption = paste("Memory Usage:", lares::formatNum(numbers$Memory.Usage/(1024*1024)),"Mb")) +
      facet_grid(type ~., scales = "free") + 
      geom_text(size = 3, hjust = 1.1)  
    print(plot)
  }
  if (return == "skimr") {
    return(skimr::skim(df))
  }
  if (return == "numbers") {
    return(intro2 %>% select(-type))
  }
  if (return == "names") {
    return(names) 
  }
}
