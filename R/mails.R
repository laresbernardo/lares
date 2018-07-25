# Send emails with attachments using Sendgrid

mailSend = function(body, subject, attachment = NULL, to = credentials$to, from = credentials$from, creds = NA) {

  suppressMessages(require(mailR))
  suppressMessages(require(config))

  credentials <- lares::get_credentials(from="sendgrid", dir = creds)

  smtp <- list(host.name = credentials$host,
               port = credentials$port,
               user.name = credentials$uid,
               passwd = credentials$pwd,
               ssl = TRUE, tls = TRUE)
  send.mail(from = from, to = to,  subject = subject,
            body = paste(body),
            html = TRUE, inline = TRUE, smtp = smtp,
            authenticate = TRUE, send = TRUE,
            attach.files = attachment)
}
