####################################################################
#' Send Slack Message (Webhook)
#' 
#' This function send a Slack message using its Webhooks.
#' 
#' @family Tools
#' @param text,title,pretext Character
#' @param hook Character. Web hook URL
#' @param quiet Boolean
#' @export
slackSend <- function(text, title = "", pretext = "", hook = NA, quiet = FALSE) {
  
  if (is.na(text))
    stop("You must write something for message to be sent!")
  
  if (is.na(hook)) {
    hook <- paste0("https://hooks.slack.com/services/",
                   "T2H307N9E/BP5GM8VEG/","1UDv2cpseBPC3wFB4p8DV0qw")
    if (!quiet)
      warning("Using lares' default hook. Please, use yours for it to work as it should.")
  } 
  
  aux <- paste(
    '{"attachments": [{',
    '"title": "', title, '",',
    '"pretext": "', text, '",',
    '"text": "', pretext, '", "color": "#ee0000"',
    '}]}',
    sep = '')
  
  invisible(POST(hook, body = aux))
}
