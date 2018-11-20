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
    }
    return(value)
  }
  
  fxclean <- function(value, clean = FALSE, targets = NA) {
    if (clean == TRUE) {
      if (!is.numeric(value)) {
        value <- lares::cleanText(value, spaces = F)
      }
      if (!is.numeric(targets) & !is.na(targets)) {
        targets <- lares::cleanText(targets, spaces = F)
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
    
    if (is.numeric(value) | is.Date(value) | is.POSIXct(value) | is.POSIXlt(value)) {
      # Continuous and date values
      if (is.numeric(value)) {
        p <- ggplot(df, aes(x = value)) + 
          scale_x_continuous(labels = scales::comma)
      } else {
        p <- ggplot(df, aes(x = date(value)))
      }
      p <- p + theme_minimal() +
        geom_density(fill = "deepskyblue", alpha = 0.7, adjust = 1/3) +
        labs(y = "", x = "",
             title = paste("Density Distribution"),
             subtitle = paste("Variable:", variable_name))
      print(p)
    } else {
      # Discrete values
      df %>% freqs(value, plot = T, results = F, variable_name = variable_name)
    }
    # Return table with results?
    if (results == TRUE) {
      output <- df %>% freqs(value)
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
    
    if (length(unique(targets)) >= 8) {
      stop("You should use a 'target' variable with max 8 different values!")
    }
    
    if (na.rm == TRUE & sum(is.na(value)) > 0) { breaks <- breaks + 1 }
    breaks <- breaks + 1
    
    if (is.numeric(value)) {
      quant <- quantile(value, prob = seq(0, 1, length = breaks), na.rm = T)
      if (length(unique(quant)) != breaks) {
        message(paste("When dividing", variable_name, "into", breaks-1, "quantiles,", length(unique(quant)), "groups are created."))
      }
      value <- cut(value, unique(quant))
    }
    
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
    
    if (abc == TRUE) {
      freqs <- freqs %>% mutate(order = rank(as.character(value)))
    }
    
    caption <- paste0("Variables: ", targets_name, " vs. ", variable_name, 
                      ". Obs: ", lares::formatNum(nrow(df), 0))
    
    # Counter plot
    if(type %in% c(1,2)) {
      count <- ggplot(freqs, aes(
        x=reorder(as.character(value), order), y=n, 
        fill=tolower(as.character(targets)), 
        label=n, ymax=max(n)*1.1)) + 
        geom_col(position = "dodge") +
        geom_text(check_overlap = TRUE, 
                  position = position_dodge(0.9), 
                  size=3, vjust = -0.15) +
        labs(x = "", y = "Counter", fill = targets_name, caption = caption) + 
        theme_minimal() + theme(legend.position = "top")
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
    
    # Percentages plot
    if (type %in% c(1,3)) {
      prop <- ggplot(freqs, 
                     aes(x = reorder(value, -order), 
                         y = as.numeric(p/100),
                         fill=as.character(targets), 
                         label = p)) + 
        geom_col(position = "fill") +
        geom_text(check_overlap = TRUE, size = 3.2,
                  position = position_stack(vjust = 0.5)) +
        theme_minimal() + coord_flip() +
        labs(x = "Proportions", y = "", fill = targets_name, caption = caption) +
        theme(legend.position = "top") + ylim(0, 1)
      # Show limit caption when more values than top
      if (length(unique(value)) > top) {
        count <- count + labs(caption = paste("Showing the", top, "most frequent values"))
      }
      # Show a reference line if levels = 2; quite useful when data is unbalanced (not 50/50)
      if (length(unique(targets)) == 2) {
        distr <- df %>% freqs(targets)
        prop <- prop +
          geom_hline(yintercept = (100-distr$pcum[1])/100, 
                     colour = "purple", linetype = "dotted", alpha = 0.8)
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
        lares::cleanText(targets_name), ".vs.", 
        lares::cleanText(variable_name), 
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
