####################################################################
#' Compare Variables with their Distributions
#' 
#' Compare the distribution of a target variable vs another variable. This 
#' function automatically splits into quantiles for numerical variables.
#' Custom and tidyverse friendly.
#' 
#' @param data Dataframe
#' @param ... Variables. Main (target variable) and secondary (values 
#' variable) to group by
#' @param type Integer. 1 for both plots, 2 for counter plot only, 3 por 
#' percentages plot only.
#' @param top Integer. Filter and plot the most n frequent for categorical values
#' @param breaks Integer. Number of splits for numerical values
#' @param na.rm Boolean. Ignore NAs if needed
#' @param force Character. Force class on the values data. Choose between 'none',
#' 'character', 'numeric', 'date'
#' @param trim Integer. Trim words until the nth character for categorical values 
#' (applies for both, target and values)
#' @param clean Boolean. Use lares::cleanText for categorical values (applies 
#' for both, target and values)
#' @param abc Boolean. Do you wish to sort by alphabetical order?
#' @param custom_colours Boolean. Use custom colours function?
#' @param results Boolean. Return results data.frame?
#' @param save Boolean. Save the output plot in our working directory
#' @param subdir Character. Into which subdirectory do you wish to save the plot to?
#' @export
distr <- function(data, ...,
                  type = 1,
                  top = 10, 
                  breaks = 10, 
                  na.rm = FALSE, 
                  force = "none",
                  trim = 0,
                  clean = FALSE,
                  abc = FALSE,
                  custom_colours = FALSE,
                  results = FALSE,
                  save = FALSE, 
                  subdir = NA) {
  
  options(scipen=999)
  
  vars <- quos(...)
  
  # Validate if we can continue with given data:
  if (length(vars) > 2) {
    stop("Please, select only one or two variables to continue...") 
  }
  
  # Functions
  force_class <- function(value, class = "none") {
    if (class != "none") {
      if (grepl("char|fact", class) & is.numeric(value)) {
        value <- as.character(value)
      }
      if (grepl("num|int", class) & !is.numeric(value)) {
        value <- as.numeric(value)
      }
      if (grepl("dat|day|time", class)) {
        value <- gsub(" .*", "", as.character(value))
        value <- lubridate::date(value)
      }
    }
    return(value)
  }
  
  fxtrim <- function(value, trim, targets = NA) {
    if (trim > 0) {
      if (!is.numeric(value)) {
        value <- substr(value, 1, trim)
      }
      if (!is.numeric(targets) & !is.na(targets)) {
        targets <- substr(targets, 1, trim)
      }
      message(paste("Chopping everything to", trim, "characters..."))
    }
    return(value)
  }
  
  fxclean <- function(value, clean = FALSE, targets = NA) {
    if (clean == TRUE) {
      if (!is.numeric(value)) {
        value <- cleanText(value, spaces = F)
      }
      if (!is.numeric(targets) & !is.na(targets)) {
        targets <- cleanText(targets, spaces = F)
      } 
    }
    return(value)
  }
  
  fxna_rm <- function(df, na.rm = FALSE){
    if (na.rm == TRUE) {
      df <- df[complete.cases(df), ]
    }
    return(df)
  }
  
  # When we only have one variable
  if (length(vars) == 1) {
    value <- data %>% select(!!!vars[[1]])
    variable_name <- colnames(value)
    value <- value[,1] # do.call("c", value)
    value <- force_class(value, force)
    value <- fxtrim(value, trim)
    value <- fxclean(value, clean)
    
    df <- data.frame(value = value, dummy = 0)
    df <- fxna_rm(df, na.rm)
    
    is.Date <- function(x) inherits(x, "Date")    
    is.POSIXct <- function(x) inherits(x, "POSIXct")
    is.POSIXlt <- function(x) inherits(x, "POSIXlt")
    if (is.numeric(value) | is.Date(value) | is.POSIXct(value) | is.POSIXlt(value)) {
      # Continuous and date values
      if (!is.numeric(value)) {
        p <- ggplot(df, aes(x = value))
      } else {
        p <- ggplot(df, aes(x = date(value)))
      }
      p <- p + theme_minimal() +
        geom_density(fill = "deepskyblue", alpha = 0.7, adjust = 1/3) +
        labs(y = "", x = "", fill = "Density",
             title = paste("Density Distribution"),
             subtitle = paste("Variable:", variable_name),
             caption = paste("Obs:", formatNum(nrow(df), 0)))
      print(p)
    } else {
      # Discrete values
      df %>% freqs(value, plot = T, results = F, 
                   variable_name = variable_name, 
                   abc = abc, top = top)
    }
    # Return table with results?
    if (results == TRUE) {
      output <- df %>% freqs(value, top = top)
      return(output)
    }
  }
  
  # When we only have 2 variables
  if (length(vars) == 2) {
    
    targets <- data %>% select(!!!vars[[1]])
    targets_name <- colnames(targets)
    targets <- targets[,1]
    value <- data %>% select(!!!vars[[2]])
    variable_name <- colnames(value)
    # Transformations
    value <- value[,1] # do.call("c", value)
    value <- force_class(value, force)
    value <- fxtrim(value, trim)
    value <- fxclean(value, clean)
    
    if (length(targets) != length(value)) {
      message("The targets and value vectors should be the same length.")
      stop(message(paste("Currently, targets has", length(targets),
                         "rows and value has", length(value))))
    }
    
    # For num-num distributions or too many unique target variables
    if (length(unique(targets)) >= 8) {
      if (is.numeric(targets) & is.numeric(value)) {
        df <- data.frame(x = targets, y = value)
        df <- fxna_rm(df, na.rm = TRUE)
        p <- ggplot(df, aes(x = x, y = y)) +
          stat_density_2d(aes(fill = ..level..), geom = "polygon") +
          theme_minimal() +
          labs(title = "2D Distribution Plot",
               x = targets_name, y = variable_name,
               subtitle = paste("For", variable_name, "vs.", targets_name),
               caption = paste("Obs:", nrow(df))) +
          scale_x_continuous(labels = scales::comma) +
          scale_y_continuous(labels = scales::comma)
        return(p)  
      }
      stop("You should use a 'target' variable with max 8 different values.")
    }
    
    # Only n numeric values, really numeric?
    if (is.numeric(value) & length(unique(value)) <= 8) {
      value <- force_class(value, class = "char")
    }
    
    # Turn numeric variables into quantiles
    if (is.numeric(value)) {
      breaks <- ifelse(top != 10, top, breaks)
      value <- quants(value, breaks, return = "labels")
      cuts <- length(unique(value[!is.na(value)]))
      if (cuts != breaks) {
        message(paste("When dividing", variable_name, "into", breaks, "quantiles,", 
                      cuts, "cuts/groups are possible."))
      }
      top <- top + 1
    }
    
    # Caption for plots
    caption <- paste0("Variables: ", targets_name, " vs. ", variable_name,
                      ". Obs: ", formatNum(nrow(df), 0))
    
    # Finally, we have our data.frame
    df <- data.frame(targets = targets, value = value)
    df <- fxna_rm(df, na.rm)
    
    freqs <- df %>% 
      group_by(value, targets) %>% 
      tally() %>% arrange(desc(n)) %>% 
      mutate(p = round(100*n/sum(n),2)) %>% ungroup() %>%
      mutate(row = row_number(),
             order = ifelse(grepl("\\(|\\)", value), 
                            as.numeric(as.character(substr(gsub(",.*", "", value), 2, 100))), row))
    if(length(unique(value)) > top & !is.numeric(value)) {
      message(paste("Filtering the", top, "most frequent values. Use `top` to overrule."))
      which <- df %>% group_by(value) %>% tally() %>% arrange(desc(n)) %>% slice(1:top)
      freqs <- freqs %>%
        mutate(value = ifelse(value %in% which$value, as.character(value), "OTHERS")) %>%
        group_by(value, targets) %>% select(-row, -order) %>%
        summarise(n = sum(n)) %>%
        mutate(p = round(100*n/sum(n),2)) %>%
        ungroup() %>% arrange(desc(n)) %>%
        mutate(row = row_number(), 
               order = row_number())
    }
    
    # Sort values alphabetically or ascending if numeric
    if (abc == TRUE) {
      freqs <- freqs %>% mutate(order = rank(value))
    }
    
    # Counter plot
    if(type %in% c(1,2)) {
      count <- ggplot(freqs, aes(
        x = reorder(as.character(value), order), y=n, 
        fill = tolower(as.character(targets)), 
        label = n, ymax = max(n) * 1.1)) + 
        geom_col(position = "dodge") +
        geom_text(colour = "black",
                  check_overlap = TRUE, 
                  position = position_dodge(0.9), 
                  size=3, vjust = -0.15) +
        labs(x = "", y = "Counter", fill = targets_name, caption = caption) + 
        theme_minimal() + theme(legend.position = "top") + guides(colour = FALSE) +
        theme(axis.title.y = element_text(size = rel(0.8), angle = 90))
      # Give an angle to labels when more than...
      if (length(unique(value)) >= 7) {
        count <- count + theme(axis.text.x = element_text(angle = 45, hjust=1))
      } 
      # Custom colours if wanted...
      if (custom_colours == TRUE) {
        count <- count + gg_fill_customs()
      } else {
        count <- count + scale_fill_brewer(palette = "Blues")
      }
    }
    
    # Proportions (%) plot
    if (type %in% c(1,3)) {
      prop <- freqs %>%
        group_by(value) %>%
        mutate(size = sum(n)/sum(freqs$n)) %>%
        ggplot(aes(x = reorder(value, -order), 
                   y = as.numeric(p/100),
                   fill = tolower(as.character(targets)),
                   label = p)) + 
        geom_col(position = "fill") +
        geom_text(aes(size = size,
                      colour = ifelse(custom_colours, tolower(as.character(targets)), "none")),
                  check_overlap = TRUE,
                  position = position_stack(vjust = 0.5)) +
        scale_size(range = c(1.8, 3.5)) +
        theme_minimal() + coord_flip() +
        labs(x = "Proportions", y = "", fill = targets_name, caption = caption) +
        theme(legend.position = "top") + ylim(0, 1) + guides(colour = FALSE, size = FALSE) +
        theme(axis.title.y = element_text(size = rel(0.8), angle = 90)) +
        gg_text_customs()
      # Show limit caption when more values than top
      if (length(unique(value)) > top) {
        count <- count + labs(caption = paste("Showing the", top, "most frequent values"))
      }
      # Show a reference line if levels = 2; quite useful when data is unbalanced (not 50/50)
      if (length(unique(targets)) == 2) {
        distr <- df %>% freqs(targets)
        h <- signif(100 - distr$pcum[1], 3)
        prop <- prop +
          geom_hline(yintercept = h/100, colour = "purple", 
                     linetype = "dotted", alpha = 0.8) +
          geom_label(aes(0, h/100, label = h, vjust = -0.05), 
                     size = 2.3, fill="white", alpha = 0.8)
      }
      # Custom colours if wanted...
      if (custom_colours == TRUE) {
        prop <- prop + gg_fill_customs()
      } else {
        prop <- prop + scale_fill_brewer(palette = "Blues")
      }
    }
    
    # Export file name and folder
    if (save == TRUE) {
      file_name <- paste0(
        "viz_distr_", 
        cleanText(targets_name), ".vs.", 
        cleanText(variable_name), 
        case_when(type == 2 ~ "_c", type == 3 ~ "_p", TRUE ~ ""),".png")
      if (!is.na(subdir)) {
        options(warn=-1)
        dir.create(file.path(getwd(), subdir), recursive = T)
        file_name <- paste(subdir, file_name, sep="/")
      }
    }
    
    # Plot the results and save if needed
    if (type == 1) {
      prop <- prop + guides(fill=FALSE)
      count <- count + labs(caption = "")
      if (save == TRUE) {
        png(file_name, height = 1000, width = 1300, res = 200)
        gridExtra::grid.arrange(count, prop, ncol = 1, nrow = 2)
        dev.off()
      }
      invisible(gridExtra::grid.arrange(count, prop, ncol = 1, nrow = 2))
    }
    if (type == 2) {
      if (save == TRUE) {
        count <- count + 
          ggsave(file_name, width = 8, height = 6)
      }
      plot(count)
    }
    if (type == 3) {
      if (save == TRUE) {
        prop <- prop + 
          ggsave(file_name, width = 8, height = 6)
      }
      plot(prop)
    }
    
    # Return table with results?
    if (results == TRUE) {
      table <- freqs %>% select(-order)
      return(table)
    }
  }
}
