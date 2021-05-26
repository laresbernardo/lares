####################################################################
#' Send Slack Message (Webhook)
#' 
#' This function send a Slack message using its Webhooks.
#' 
#' For more help, you can follow the 
#' \href{https://api.slack.com/messaging/webhooks#posting_with_webhooks}{Sending messages using Incoming Webhooks}
#' original documentarion.
#' 
#' @family API
#' @family Credentials
#' @param text,title,pretext Character. Content on you Slack message.
#' @param hook Character. Web hook URL. Ths value will be overwritten by
#' creds if correctly used.
#' @param creds Character. Credential's dir (see \code{get_creds()}). Set
#' hook URL into the "slack" list in your YML file. Will use first value.
#' @return Invisible POST response
#' @examples
#' \dontrun{
#' slackSend(text = "This is a message", title = "TEST", pretext = Sys.info()["user"])
#' }
#' @export
slackSend <- function(text, title = "", pretext = "", 
                      hook = NA, 
                      creds = NA) {
  
  if (is.na(text))
    stop("You must write something for message to be sent!")
  
  if (is.na(hook)) {
    c <- get_credentials(from = "slack", dir = creds)
    hook <- c[[1]]
  } 
  
  aux <- paste(
    '{"attachments": [{',
    '"title": "', title, '",',
    '"pretext": "', text, '",',
    '"text": "', pretext, '", 
    "color": "#ee0000"',
    '}]}',
    sep = '')
  
  invisible(POST(hook, body = aux))
}
