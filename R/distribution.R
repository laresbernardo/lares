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
  if (length(vars) != 2) {
    if (length(vars) < 2) {
      stop(paste("Please, select two variables to continue",
                 "or use `lares::freqs(..., plot=T)` instead."))
    } else {
      stop("Please, select only two variables to continue...") 
    }
  }
  
  targets <- data %>% select(!!!vars[[1]])
  value <- data %>% select(!!!vars[[2]])
  targets_name <- colnames(targets)
  variable_name <- colnames(value)
  targets <- unlist(targets)
  value <- unlist(value)
  
  if (length(targets) != length(value)) {
    message("The targets and value vectors should be the same length.")
    stop(message(paste("Currently, targets has", length(targets),
                       "rows and value has", length(value))))
  }
  
  if (force != "none") {
    if (force == "character" & is.numeric(value)) {
      value <- as.character(value)
    }
    if (force == "numeric" & !is.numeric(value)) {
      value <- as.numeric(value)
    }
    if (force == "date") {
      value <- as.Date(value, origin = '1970-01-01')
    }
  }
  
  if (trim > 0) {
    if (!is.numeric(value)) {
      value <- substr(value, 1, trim)
    }
    if (!is.numeric(targets)) {
      targets <- substr(targets, 1, trim)
    }
  }
  
  if (clean == TRUE) {
    if (!is.numeric(value)) {
      value <- lares::cleanText(value, spaces = F)
    }
    if (!is.numeric(targets)) {
      targets <- lares::cleanText(targets, spaces = F)
    }
  }
  
  if (length(unique(value)) > top & !is.numeric(value)) {
    message(paste("The variable", values, "has", length(unique(value)), "different values!"))
  }
  
  if (length(unique(targets)) > 9) {
    stop("You should use a 'target' variable with max 8 different values!")
  }
  
  if (is.numeric(value)) {
    quant <- quantile(value, prob = seq(0, 1, length = breaks), na.rm = T)
    if (length(unique(quant)) != breaks) {
      message(paste("When dividing", variable_name, "into", breaks, "quantiles,", length(unique(quant)), "groups are created."))
    }
    value <- cut(value, unique(quant))
  }

  df <- data.frame(targets = targets, value = value)
  
  if (na.rm == TRUE) {
    df <- df[complete.cases(df), ]
    if (sum(is.na(df$value)) > 0) {
      breaks <- breaks + 1 
    }
  }
  
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
    freqs <- freqs %>% filter(value %in% which$value)
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
  if(type %in% c(1,3)) {
    distr <- df %>% group_by(targets) %>% 
      tally() %>% arrange(n) %>% 
      mutate(p = round(100*n/sum(n),2), pcum = cumsum(p))
    prop <- ggplot(freqs, 
                   aes(x = value, 
                       y = as.numeric(p/100),
                       fill=as.character(targets), 
                       label = p)) + 
      geom_col(position = "fill") +
      geom_text(check_overlap = TRUE, size = 3.2,
                position = position_stack(vjust = 0.5)) +
      geom_hline(yintercept = distr$pcum[1:(nrow(distr)-1)]/100, 
                 colour = "purple", linetype = "dotted", alpha = 0.8) +
      theme_minimal() + coord_flip() +
      labs(x = "Proportions", y = "", fill = targets_name, caption = caption) +
      theme(legend.position = "top")
    # Show limit caption when more values than top
    if (length(unique(value)) > top) {
      count <- count + labs(caption = paste("Showing the", top, "most frequent values"))
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
    count <- count + labs(caption=targets_name)
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
