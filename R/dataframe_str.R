####################################################################
#' Dataset columns and rows structure
#' 
#' This function lets the user to check quickly the structure of a
#' dataset (data.frame). It returns multiple counters for useful metrics,
#' a plot, and a list of column names for each of the column metrics.
#' 
#' @family Exploratory
#' @param df Dataframe
#' @param return Character. Return "skimr" for skim report, "numbers" for
#' stats and numbers, "names" for a list with the column names of each of 
#' the class types, "plot" for a nice plot with "numbers" output
#' @param subtitle Character. Add subtitle to plot
#' @param quiet Boolean. Keep quiet or show other options available?
#' @export
df_str <- function(df, 
                   return = "plot", 
                   subtitle = NA, 
                   quiet = FALSE){
  
  if (!quiet) {
    rets <- c("plot","skimr","numbers","names")
    message(paste("Other available 'return' options:", vector2text(rets[rets != return]))) 
  }
  
  df <- data.frame(df)
  
  if (return == "skimr") {
    try_require("skimr")
    return(skim(df))
  }
  
  names <- list(
    cols = colnames(df),
    nums = colnames(df)[unlist(lapply(df, is.numeric))],
    char = colnames(df)[unlist(lapply(df, is.character))],
    factor = colnames(df)[unlist(lapply(df, is.factor))],
    logic = colnames(df)[unlist(lapply(df, is.logical))])
  names[["time"]] <- names$cols[!colnames(df) %in% c(
    names$nums, names$char, names$factor, names$logic)]
  names[["allnas"]] <- names$cols[sapply(df, function(x) all(is.na(x)))] 
  
  if (return == "names") return(names)
  
  numbers <- data.frame(
    "Total Values" = nrow(df) * ncol(df),
    "Total Rows" = nrow(df),
    "Total Columns" = ncol(df),
    "Numeric Columns" = length(names$nums),
    "Character Columns" = length(names$char),
    "Factor Columns" = length(names$factor),
    "Logical Columns" = length(names$logic),
    "Time/Date Columns" = length(names$time),
    "All Missing Columns" = length(names$allnas),
    "Missing Values" = sum(is.na(df)),
    "Complete Rows" = sum(complete.cases(df[, sapply(x, !is.list)])),
    "Memory Usage" = as.numeric(object.size(df)))
  
  intro2 <- data.frame(counter = t(numbers)) %>%
    mutate(metric = row.names(.),
           type = ifelse(grepl("Column", colnames(numbers)), "Columns",
                         ifelse(grepl("Rows", colnames(numbers)), "Rows", "Values")),
           p = ifelse(type == "Columns", 100*counter/numbers$Total.Columns,
                      ifelse(type == "Rows", 100*counter/numbers$Total.Rows, 
                             100*counter/numbers$Total.Values)),
           p = round(p,2),
           type = factor(type, levels = c("Values","Columns","Rows"))) %>%
    select(metric, counter, type, p)
  
  if (return == "numbers") return(select(intro2, -type))
  
  if (return == "plot") {
    p <- intro2 %>%
      filter(!metric %in% c("Memory.Usage")) %>%
      mutate(x = ifelse(p < 75, -0.15, 1.15)) %>%
      ggplot(aes(x = reorder(metric, as.integer(counter)), 
                 y = p, fill = type,
                 label = formatNum(counter, 0))) + 
      geom_col() + coord_flip() + ylim(0, 100) +
      theme_minimal() + guides(fill = FALSE) +
      labs(title = "Dataset overall structure", 
           x = "", y = "% of total", fill = "", 
           caption = paste("Memory Usage:", formatNum(numbers$Memory.Usage/(1024*1024)),"Mb")) +
      facet_grid(type ~., scales = "free", space = "free") + 
      geom_text(aes(hjust = x), size = 3) +
      theme_lares2(pal = 1)
    if (!is.na(subtitle)) p <- p + labs(subtitle = subtitle)
    return(p)
  }
}
