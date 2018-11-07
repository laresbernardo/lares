####################################################################
#' Send Emails with Attachments (Sendgrid)
#' 
#' This function lets the user send Emails with Attachments using Sendgrid
#' 
#' @param body Character. Body for the email
#' @param subject Character. Subject for the email
#' @param attachment Vector. All objects which will be sent as attachment
#' @param to Character. Email of the person who will recevie the email
#' @param from Character. Email of the person who send the email
#' @param creds Character. Credential's user (see get_credentials)
#' @param quite Boolean. Keep quite or display messages?
#' @export
mailSend = function(body, 
                    subject, 
                    attachment = NULL, 
                    to = credentials$to, 
                    from = credentials$from, 
                    creds = NA,
                    quite = FALSE) {
  
  # require(mailR)
  # require(config)
  
  credentials <- get_credentials(from="sendgrid", dir = creds)
  
  smtp <- list(host.name = credentials$host,
               port = credentials$port,
               user.name = credentials$uid,
               passwd = credentials$pwd,
               ssl = TRUE, tls = TRUE)
  
  invisible(
    mailR::send.mail(
      from = from, to = to, subject = subject,
      body = paste(body),
      html = TRUE, inline = TRUE, smtp = smtp,
      authenticate = TRUE, send = TRUE,
      attach.files = attachment)
  )
  
  if(quite == FALSE) {
    message(paste("Email sent to", to)) 
  }
}
