####################################################################
#' Send Emails with Attachments (POST)
#'
#' This function lets the user send Emails with Attachments using MailGun's
#' API service.
#'
#' @family Tools
#' @family Credentials
#' @param from,to,cc,bcc Character. Emails
#' @param subject Character. Subject for the email.
#' @param text,html Character. Text or HTML to send in the body.
#' @param attachment Character, plot or data.frame. Will send the file,
#' plot as PNG or data.frame as CSV, respectively.
#' @param service Character. Service platform to search on \code{creds}.
#' @param creds Character. Credential's user (see \code{get_creds()}).
#' Must contain: url (POST address), api (API key).
#' @param quiet Boolean. Keep quite or display messages?
#' @return No return value, called for side effects.
#' @examples
#' \dontrun{
#' myPlot <- noPlot("My plot")
#' mailSend(
#'   from = "BLV <myuser@mail.com>",
#'   to = "youruser@mail.com",
#'   cc = "myuser@mail.com",
#'   subject = paste("Daily report:", Sys.Date()),
#'   attachment = myPlot
#' )
#' }
#' @export
mailSend <- function(from = "RMail <laresbernardo@gmail.com>",
                     to = "laresbernardo@gmail.com",
                     cc = NULL, bcc = NULL,
                     subject = "Mail from R",
                     text = " \n",
                     html = NULL,
                     attachment = NULL,
                     service = "mailgun",
                     creds = NULL,
                     quiet = FALSE) {

  # MailGun documentation:
  # https://documentation.mailgun.com/en/latest/api-sending.html#sending

  credentials <- get_credentials(from = service, dir = creds)
  url <- credentials$url
  api_key <- credentials$api

  the_body <- list(from = from, to = to, subject = subject)

  if (grepl("mailgun", url)) the_body[["o:tag"]] <- "R-email"
  if (!is.null(cc)) the_body[["cc"]] <- cc
  if (!is.null(bcc)) the_body[["bcc"]] <- bcc
  if (!is.null(text)) the_body[["text"]] <- text
  if (!is.null(html)) the_body[["html"]] <- html

  if (!is.null(attachment)) {
    if ("ggplot" %in% class(attachment)) {
      as <- "png"
      file <- paste0(tempdir(), "/attch.", as)
      attachment + ggsave(file)
    }
    if ("data.frame" %in% class(attachment)) {
      as <- "csv"
      file <- paste0(tempdir(), "/attch.", as)
      write.csv(attachment, file, row.names = FALSE)
    }
    if ("character" %in% class(attachment)) {
      as <- "attachment"
      file <- as.character(attachment)
    }
    the_body[["attachment"]] <- upload_file(file)
  } else {
    as <- NULL
    file <- NULL
  }

  # Send email
  req <- POST(url,
    authenticate("api", api_key),
    add_headers("Content-Type" = "multipart/form-data"),
    body = the_body
  )
  stop_for_status(req)

  # Delete temporary files created
  if (!is.null(file) & !"attachment" %in% as) file.remove(file)

  if (!quiet) {
    message(paste(
      subject, "sent to", to, ifelse(!is.null(as), paste("with", as, "file"), "")
    ))
  }
}
