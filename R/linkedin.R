####################################################################
#' OAuth Linkedin
#'
#' This function authenticates and creates a token for LinkedIn's
#' API REST
#'
#' @family API
#' @family LinkedIn
#' @param app_name Character. Your App's given name.
#' @param client_id Character. Your App's client ID.
#' @param client_secret Character. Your App's client secret.
#' @return Character. String with token requested.
#' @export
li_auth <- function(app_name = NA, client_id = NA, client_secret = NA) {
  # Idea from https://github.com/mpiccirilli/Rlinkedin/blob/master/R/getProfile.R

  if (is.na(app_name) && is.na(client_id) && is.na(client_secret)) {
    app_name <- "LaresApp"
    client_id <- "78a384k5n1otml"
    client_secret <- "NDKzgFgurKt1M0Ab"
  }

  linkedin <- oauth_endpoint(
    "requestToken", "authorize", "accessToken",
    base_url = "https://api.linkedin.com/uas/oauth"
  )
  myapp <- oauth_app(appname = app_name, client_id, client_secret)
  token <- oauth1.0_token(linkedin, myapp)

  return(token)
}

####################################################################
#' Get My Personal LinkedIn Data
#'
#' This function brings a list with your personal LinkedIn data
#'
#' @family API
#' @family LinkedIn
#' @param token Object. OAuth Authentication: li_auth()'s output.
#' @return List. Results of your own profile data given the \code{token}.
#' @export
li_profile <- function(token = NA) {
  if (is.na(token)) {
    token <- li_auth()
  }

  base_url <- "https://api.linkedin.com/v1/people/"
  profile_fields <- ":(id,first-name,email-address,last-name,headline,picture-url,industry,summary,specialties,positions:(id,title,summary,start-date,end-date,is-current,company:(id,name,type,size,industry,ticker)),educations:(id,school-name,field-of-study,start-date,end-date,degree,activities,notes),associations,interests,num-recommenders,date-of-birth,publications:(id,title,publisher:(name),authors:(id,name),date,url,summary),patents:(id,title,summary,number,status:(id,name),office:(name),inventors:(id,name),date,url),languages:(id,language:(name),proficiency:(level,name)),skills:(id,skill:(name)),certifications:(id,name,authority:(name),number,start-date,end-date),courses:(id,name,number),recommendations-received:(id,recommendation-type,recommendation-text,recommender),honors-awards,three-current-positions,three-past-positions,volunteer)"

  url <- paste0(base_url, "~", profile_fields, "?format=json")
  get <- GET(url, config = token)
  char <- rawToChar(get$content)
  json <- fromJSON(char)

  return(json)
}
