#' ####################################################################
#' #' Download typeform data
#' #'
#' #' This function lets the user download surveys results from a
#' #' Typeform account.
#' #'
#' #' @family Typeform
#' #' @family Credentials
#' #' @param account Character. Which of the accounts in config.yml
#' #' do you wish to use?
#' #' @param form Character. Define which form you wish to import. Should
#' #' be the same as the form's name at the online platform
#' #' @param creds Character. Credential's user (see \code{get_creds()})
#' #' @export
#' typeform_download <- function(account = "tabunga", form, creds = NA) {
#'
#'   try_require("rtypeform")
#'
#'   accounts <- c("tabunga")
#'
#'
#'   if (account %in% accounts) {
#'     cr <- get_credentials(from = "typeform", dir = creds)
#'     if (account == "tabunga") {
#'       api <- cr$api_tabunga
#'     }
#'
#'     typeforms <- get_typeforms(api)
#'
#'     if (form %in% typeforms$content$name) {
#'       uid = typeforms$content$uid[typeforms$content$name==form]
#'       x = get_questionnaire(uid, api)
#'     } else {
#'       message(paste("Not a valid 'form' name. Try any of the following:\n",
#'                     paste(shQuote(typeforms$content$name), collapse = "\n ")))
#'     }
#'   }
#'
#'   #Some useful transformations
#'   colnames <- data.frame(id = colnames(x$completed)) %>%
#'     left_join(x$questions, by = "id") %>%
#'     mutate(question = ifelse(!is.na(question), question, id))
#'   colnames(x$completed) <- colnames$question
#'
#'   return(x)
#'
#' }
#'
#' # x <- typeform_download(form="Tabunga Créditos")
