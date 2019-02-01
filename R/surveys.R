####################################################################
#' Send Emails with Attachments (Sendgrid)
#' 
#' This function lets the user send Emails with Attachments using Sendgrid
#' 
#' @param answers Dataframe. Answers. Each row a different person. 
#' Each column a different answer.
#' @param ignore Numeric Vector. Which columns are NOT answers?
#' @param title Character. Title for your plot
#' @param subtitle Character. Subtitle for your plot
#' @export
plot_survey <- function(answers, ignore = c(1), title = NA, subtitle = NA){
  if (length(ignore) > 0) {
    answers <- answers[-ignore]
  }
  p <- answers %>%
    # replaceall(1, "Most Important") %>% 
    # replaceall(2, "Important") %>% 
    # replaceall(3, "Less Important") %>%
    # replaceall(4, "Not Important") %>%
    gather() %>% freqs(key, value) %>% 
    ggplot(aes(x = key, y = n, fill = value, group = key, label = p)) +
    geom_bar(stat = "identity") + theme_minimal() +
    geom_text(position = position_stack(vjust = .5), colour = "white", size = 2.7) +
    coord_flip() + 
    labs(title = "Survey Results",
         subtitle = "Answers for the n question",
         x = "", y = "[%]", fill = "",
         caption = paste("Obs.:", nrow(answers)))
  if (!is.na(title)) {
   p <- p + labs(title = title)
  }
  if (!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }
  return(p)
}
