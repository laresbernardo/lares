# Send emails with attachments using Sendgrid

mailSend = function(body, subject, attachment = NULL, to = dw$to, from = dw$from) {

  suppressMessages(require(mailR))
  suppressMessages(require(config))

  wd <- getwd()
  setwd("~/Dropbox (ID)/CM Data Science/Library")
  dw <- config::get("sendgrid")
  setwd(wd)

  smtp <- list(host.name = dw$host,
               port = dw$port,
               user.name = dw$uid,
               passwd = dw$pwd,
               ssl = TRUE, tls = TRUE)
  send.mail(from = from, to = to,  subject = subject,
            body = paste(body),
            html = TRUE, inline = TRUE, smtp = smtp,
            authenticate = TRUE, send = TRUE,
            attach.files = attachment)
}
