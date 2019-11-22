####################################################################
#' Send Emails with Attachments (POST)
#' 
#' This function lets the user send Emails with Attachments using MailGun
#'
#' @family Tools
#' @param from,to,cc,bcc Character. Emails
#' @param subject Character. Subject for the email
#' @param text,html Character. Text or HTML to send in the body
#' @param attachment Character, plot or data.frame. Will send the file, 
#' plot as PNG or data.frame as CSV, respectively. 
#' @param quiet Boolean. Keep quite or display messages?
#' @param creds Character. Credential's user (see get_credentials).
#' Must contain: url (POST address), api (api key)
#' @export
mailSend <- function(from = "RMail <laresbernardo@gmail.com>",
                     to = "laresbernardo@gmail.com",
                     cc = NA, bcc = NA,
                     subject = "Mail from R",
                     text = NA, html = NA,
                     attachment = NA,
                     quiet = FALSE,
                     creds = NA){

  # MailGun documentation:
  # https://documentation.mailgun.com/en/latest/api-sending.html#sending
  
  credentials <- get_credentials(from = "mailgun", dir = creds)
  url <- credentials$url
  api_key <- credentials$api
  
  the_body <- list(from = from, to = to, subject = subject)
  
  if (grepl("mailgun", url)) the_body[["o:tag"]] <- "R-email"
  if (!is.na(cc)) the_body[["cc"]] <- cc
  if (!is.na(bcc)) the_body[["bcc"]] <- bcc
  if (!is.na(text)) the_body[["text"]] <- text
  if (!is.na(html)) the_body[["html"]] <- html
  
  if (sum(!is.na(attachment)) > 0) {
    if ("ggplot" %in% class(attachment)) {
      as <- "png"
      file <- paste0(getwd(),"/attch.", as)
      attachment + ggsave(file)
    }
    if ("data.frame" %in% class(attachment)) {
      as <- "csv"
      file <- paste0(getwd(),"/attch.", as)
      write.csv(attachment, file, row.names = FALSE)
    }
    if ("character" %in% class(attachment)) {
      as <- "attachment"
      file <- as.character(attachment)
    }
    the_body[["attachment"]] <- upload_file(file) 
  } else {
    as <- NA; file <- NA
  }
  
  # Send email
  req <- POST(url,
              authenticate("api", api_key),
              add_headers("Content-Type" = "multipart/form-data"),
              body = the_body)
  stop_for_status(req)
  
  # Delete temporary files created
  if (!is.na(file) & as != "attachment") file.remove(file)
  
  if (!quiet) {
    message(paste(subject, "sent to", to, ifelse(!is.na(as), paste("with", as, "file"),""))) 
  }
  
}
