####################################################################
#' Compare Variables with their Distributions
#' 
#' Compare the distribution of a target variable vs another variable. This 
#' function automatically splits into quantiles for numerical variables.
#' Custom and tidyverse friendly.
#' 
#' @family Exploratory
#' @param data Dataframe
#' @param ... Variables. Main (target variable) and secondary (values 
#' variable) to group by
#' @param type Integer. 1 for both plots, 2 for counter plot only, 3 por 
#' percentages plot only.
#' @param note Character. Caption for the plot
#' @param top Integer. Filter and plot the most n frequent for categorical values
#' @param breaks Integer. Number of splits for numerical values
#' @param na.rm Boolean. Ignore NAs if needed
#' @param force Character. Force class on the values data. Choose between 'none',
#' 'character', 'numeric', 'date'
#' @param trim Integer. Trim labels until the nth character for categorical values 
#' (applies for both, target and values)
#' @param clean Boolean. Use lares::cleanText for categorical values (applies 
#' for both, target and values)
#' @param abc Boolean. Do you wish to sort by alphabetical order?
#' @param custom_colours Boolean. Use custom colours function?
#' @param plot Boolean. Return a plot? Otherwise, a table with results
#' @param chords Boolean. Use a chords plot?
#' @param save Boolean. Save the output plot in our working directory
#' @param subdir Character. Into which subdirectory do you wish to save the plot to?
#' @export
distr <- function(data, ...,
                  type = 1,
                  note = NA,
                  top = 10, 
                  breaks = 10, 
                  na.rm = FALSE, 
                  force = "none",
                  trim = 0,
                  clean = FALSE,
                  abc = FALSE,
                  custom_colours = FALSE,
                  plot = TRUE,
                  chords = FALSE,
                  save = FALSE, 
                  subdir = NA) {
  
  options(scipen=999)
  options(warn=-1)
  
  data <- data.frame(data)
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
    if (clean) {
      if (!is.numeric(value)) {
        value <- cleanText(value, spaces = FALSE)
      }
      if (!is.numeric(targets) & !is.na(targets)) {
        targets <- cleanText(targets, spaces = FALSE)
      } 
    }
    return(value)
  }
  
  fxna_rm <- function(df, na.rm = FALSE){
    if (na.rm) {
      df <- df[complete.cases(df), ]
    }
    return(df)
  }
  
  # lares' default palette colours
  colours_pal <- lares_pal()$palette
  
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
      if (is.numeric(value)) {
        p <- ggplot(df, aes(x = value))
      } else {
        p <- ggplot(df, aes(x = date(value)))
      }
      p <- p + 
        geom_density(fill = "deepskyblue", alpha = 0.7, adjust = 1/3) +
        labs(y = "", x = "", fill = "Density",
             title = "Density Distribution",
             subtitle = paste("Variable:", variable_name),
             caption = paste("Obs:", formatNum(nrow(df), 0))) +
        theme_lares2()
      if (top != 10) {
        p <- p + xlim(0, top)
      }
    } else {
      # Discrete values
      p <- df %>% 
        freqs(value, plot = TRUE, variable_name = variable_name, abc = abc, top = top)
    }
    # Return table with results?
    if (!plot) {
      output <- df %>% freqs(value, top = top)
      return(output)
    }
    return(p)
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
        subtitle <- paste0("Variables: ", variable_name, " vs. ", targets_name,
                           ". Obs: ", formatNum(length(value), 0))
        df <- data.frame(x = targets, y = value)
        df <- fxna_rm(df, na.rm = TRUE)
        p <- ggplot(df, aes(x = x, y = y)) +
          stat_density_2d(aes(fill = ..level..), geom = "polygon") +
          labs(title = "2D Density Distribution",
               x = targets_name, y = variable_name,
               subtitle = subtitle) +
          scale_x_continuous(labels = comma) +
          scale_y_continuous(labels = comma) +
          theme_lares2()
        return(p)  
      }
      message("You should try a 'target' variable with max 8 different values.")
      message("Automatically trying a chords plot...")
      chords <- TRUE
    }
    
    # Chords plot
    if (chords) {
      df <- data.frame(value = value, targets = targets)
      output <- freqs(df, targets, value)
      if (!na.rm) {
        output <- output %>% replaceall(NA, "NA") 
      }
      title <- "Frequency Chords Diagram"
      subtitle <- paste("Variables:", targets_name, "to", variable_name)
      if (!plot) {
        return(output)
      }
      return(plot_chord(
        output$targets, output$value, output$n, mg = 13, title = title, subtitle = subtitle))
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
    
    # Finally, we have our data.frame
    df <- data.frame(targets = targets, value = value)
    df <- fxna_rm(df, na.rm)
    
    # Captions for plots
    subtitle <- paste0("Variables: ", targets_name, " vs. ", variable_name,". Obs: ", formatNum(nrow(df), 0))
    caption <- ifelse(is.na(note), "", note)
    
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
    freqs <- if (abc) mutate(freqs, order = rank(value))
    
    # Counter plot
    if(type %in% c(1,2)) {
      vadj <- ifelse(type == 1, -0.15, 0.5)
      hadj <- ifelse(type == 1, 0.5, -0.15)
      count <- ggplot(freqs, aes(
        x = reorder(as.character(value), order), y=n, 
        fill = tolower(as.character(targets)), 
        label = formatNum(n, 0), ymax = max(n) * 1.1)) + 
        geom_col(position = "dodge") +
        geom_text(colour = "black",
                  check_overlap = TRUE, 
                  position = position_dodge(0.9), 
                  size = 3, vjust = vadj, hjust = hadj) +
        labs(x = "", y = "Counter [#]", fill = targets_name, caption = note) + 
        theme(legend.position = "top") + guides(colour = FALSE) +
        theme(axis.title.y = element_text(size = rel(0.8), angle = 90)) +
        scale_y_continuous(labels = comma) +
        theme_lares2(pal = 1)
      # Give an angle to labels when more than...
      if (length(unique(value)) >= 7) {
        count <- count + theme(axis.text.x = element_text(angle = 30, hjust=1))
      } 
      # Custom colours if wanted...
      count <- if (custom_colours) count + gg_fill_customs()
    }
    
    # Proportions (%) plot
    if (type %in% c(1,3)) {
      prop <- freqs %>%
        group_by(value) %>%
        mutate(size = sum(n)/sum(freqs$n)) %>%
        mutate(ptag = ifelse(p < 3, "", as.character(round(p, 1)))) %>%
        ggplot(aes(x = reorder(value, -order), y = p/100, label = ptag,
                   fill = tolower(as.character(targets)))) + 
        geom_col(position = "fill") +
        geom_text(aes(size = size, colour = tolower(as.character(targets))), 
                  position = position_stack(vjust = 0.5)) +
        scale_size(range = c(2.2, 3)) +
        coord_flip() +
        labs(x = "Proportions [%]", y = "", fill = targets_name, caption = note) +
        theme(legend.position = "top") + ylim(0, 1) + guides(colour = FALSE, size = FALSE) +
        theme(axis.title.y = element_text(size = rel(0.8), angle = 90)) +
        theme_lares2(pal = 1)
      # Show a reference line if levels = 2; quite useful when data is unbalanced (not 50/50)
      if (length(unique(targets)) == 2) {
        distr <- df %>% freqs(targets) %>% arrange(as.character(targets))
        h <- signif(100 - distr$p[1], 3)
        prop <- prop +
          geom_hline(yintercept = h/100, colour = "purple", 
                     linetype = "dotted", alpha = 0.8) +
          geom_label(aes(0, h/100, label = h, vjust = -0.05), 
                     size = 2.5, fill="white", alpha = 0.8)
      }
      # Custom colours if wanted...
      if (custom_colours) {
        prop <- prop + gg_fill_customs()
      }
        
    }
    
    # Export file name and folder
    if (save) {
      file_name <- paste0(
        "viz_distr_", 
        cleanText(targets_name), ".vs.", 
        cleanText(variable_name), 
        case_when(type == 2 ~ "_c", type == 3 ~ "_p", TRUE ~ ""),".png")
      if (!is.na(subdir)) {
        options(warn=-1)
        dir.create(file.path(getwd(), subdir), recursive = TRUE)
        file_name <- paste(subdir, file_name, sep="/")
      }
    }
    
    # Plot the results and save if needed
    if (type == 1) {
      count <- count + labs(title = "Distribution and Proportions", 
                            subtitle = subtitle, caption = "") +
        theme(plot.margin = margin(10, 15, -15, 15))
      prop <- prop + guides(fill=FALSE) + labs(caption = note) +
        theme(plot.margin = margin(-5, 15, -15, 15))
      if (save) {
        png(file_name, height = 1000, width = 1300, res = 200)
        gridExtra::arrangeGrob(count, prop, ncol = 1, nrow = 2)
        dev.off()
      }
      p <- invisible(gridExtra::grid.arrange(count, prop, ncol = 1, nrow = 2))
    }
    if (type == 2) {
      count <- count + coord_flip() + 
        labs(title = "Distribution Plot", subtitle = subtitle, caption  = "")
      if (save) {
        count <- count + ggsave(file_name, width = 8, height = 6)
      }
      p <- count
    }
    if (type == 3) {
      prop <- prop + labs(title = "Proportions Plot", subtitle = subtitle, caption  = "")
      if (save) {
        prop <- prop + ggsave(file_name, width = 8, height = 6)
      }
      p <- prop
    }
    
    # Return table with results?
    if (!plot) {
      table <- freqs %>% select(-order, -row)
      return(table)
    }
  }
  
  if (type == 1) {
    return(invisible(p)) 
  } else {
    invisible(return(p)) 
  }
}
