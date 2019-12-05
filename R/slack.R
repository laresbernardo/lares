####################################################################
#' Send Slack Message (Webhook)
#' 
#' This function send a Slack message using its Webhooks.
#' 
#' For more help, you can follow the 
#' \href{https://api.slack.com/messaging/webhooks#posting_with_webhooks}{Sending messages using Incoming Webhooks}
#' original documentarion.
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
                   "T5TUJPK1D/BRDPQ34DD/FAvAG6l1tFJP4SiBtyxsp1Va")
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
