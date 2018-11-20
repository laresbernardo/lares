####################################################################
#' One Hot Encoding (Dummy Variables)
#'
#' This function lets the user automatically transform a dataframe with
#' categorical columns into numerical by one hot encoding technic.
#'
#' @param df Dataframe
#' @param redundant Boolean. Should we keep redundat columns? i.e. It the
#' column only has two different values, should we keep both new columns?
#' @param trim Integer. Trim names until the nth character
#' @param summary Boolean. Print a summary of the operations?
#' @param variance Numeric. Drop columns with more than n variance. Range: 0-1
#' @param limit Integer. Limit OHE to n columns. Automatically, the
#' function will select the n most frequent values
#' @param other_label Character. With which text do you wish to replace 
#' the filtered values with?
#' @param sep Character. Separator's string
#' @export
ohe <- function(df, redundant = FALSE, trim = 0, summary = TRUE, variance = 0.95,
                limit = 10, other_label = "OTHER", sep = "_") {
  
  types <- data.frame(name = colnames(df), 
                      type = unlist(lapply(lapply(df, class), `[[`, 1)))
  no_need_to_convert <- converted <- converted_binary <- not_converted <- no_variance <- c()
  
  for (i in 1:ncol(df)) {
    vector_type <- types[i, "type"]
    vector_name <- as.character(types$name[i])
    vector_levels <- length(unique(df[,c(vector_name)]))
    
    # Columns with no variance or same amount of unique values as rows
    if (vector_levels <= 1 | vector_levels >= variance * length(vector_values[!is.na(vector_values),1])) {
      no_variance <- rbind(no_variance, vector_name)
    }
    
    if (!vector_type %in% c("integer","numeric","POSIXct","POSIXt","Date")) {
      vector_values <- df[toString(types[i, "name"])]
      vector_values <- vector_values %>% 
        mutate_all(as.character) %>%
        replace(., is.na(.), 'NAs')
      vector_values[,1] <- paste0(sep, vector_values[,1])
      
      # Columns with 2 possible values
      if (vector_levels == 2) {
        df[,c(vector_name)] <- as.integer(as.factor(df[,c(vector_name)]))-1
        converted_binary <- rbind(converted_binary, vector_name)
      }
      # Columns with more than 2 with variance
      if (!colnames(vector_values) %in% c(converted_binary, no_variance)) {
        if (vector_levels >= 3) {
          options(na.action = 'na.pass')
          vector_values <- categ_reducer(vector_values, !!as.name(vector_name), top = limit,
                                         other_label = paste0(sep, other_label))
          dummy_matx <- data.frame(model.matrix( ~ . -1, data = vector_values))
          if (redundant == FALSE) {
            dummy_matx <- dummy_matx[, 1:(ncol(dummy_matx)-1)]
          }
          df <- cbind(df, dummy_matx)
          converted <- rbind(converted, vector_name)
        }
      }
    }
    no_need_to_convert <- rbind(no_need_to_convert, vector_name)
  }
  
  if (trim > 0) {
    colnames(df) <- substr(colnames(df), 1, trim)
  }
  
  total_converted <- rbind(converted, converted_binary)
  
  if (summary == TRUE) {
    message(paste("One Hot Encoding applied to", length(total_converted), "variables:", vector2text(total_converted)))
    message(paste0("Automatically dropped ", length(no_variance), " columns with 0% or +", round(variance*100),
                   "% variance: ", vector2text(no_variance)))
  }
  
  df <- df[, c(!colnames(df) %in% c(no_variance, converted))]
  
  return(df)
  
}
