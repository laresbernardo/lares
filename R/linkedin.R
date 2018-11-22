####################################################################
#' OAuth Linkedin
#'
#' This function authenticates and creates a token for LinkedIN's API REST
#'
#' @param app_name Character. Your App's given name
#' @param client_id Character. Client ID
#' @param client_secret Character. Client secret
#' @export
li_auth <- function(app_name = NA, client_id = NA, client_secret = NA){
  
  # Idea from https://github.com/mpiccirilli/Rlinkedin/blob/master/R/getProfile.R
  
  if (is.na(app_name) & is.na(client_id) & is.na(client_secret)) {
    app_name <- "LaresApp"
    client_id <- "78a384k5n1otml"
    client_secret <- "NDKzgFgurKt1M0Ab" 
  }
  
  linkedin <- httr::oauth_endpoint(
    "requestToken", "authorize", "accessToken", base_url = "https://api.linkedin.com/uas/oauth")
  myapp <- httr::oauth_app(appname = app_name, client_id, client_secret)
  token <- httr::oauth1.0_token(linkedin, myapp)
  
  return(token)
  
}

####################################################################
#' Get My Personal LinkedIn Data
#'
#' This function brings a list with my personal Linkedin data
#'
#' @param token Object. li_auth() output
#' @export
li_profile <- function(token = NA) {
  
  if (is.na(token)) {
    token <- li_auth() 
  }
  
  base_url <- "https://api.linkedin.com/v1/people/"
  profile_fields <- ":(id,first-name,email-address,last-name,headline,picture-url,industry,summary,specialties,positions:(id,title,summary,start-date,end-date,is-current,company:(id,name,type,size,industry,ticker)),educations:(id,school-name,field-of-study,start-date,end-date,degree,activities,notes),associations,interests,num-recommenders,date-of-birth,publications:(id,title,publisher:(name),authors:(id,name),date,url,summary),patents:(id,title,summary,number,status:(id,name),office:(name),inventors:(id,name),date,url),languages:(id,language:(name),proficiency:(level,name)),skills:(id,skill:(name)),certifications:(id,name,authority:(name),number,start-date,end-date),courses:(id,name,number),recommendations-received:(id,recommendation-type,recommendation-text,recommender),honors-awards,three-current-positions,three-past-positions,volunteer)"
  
  url <- paste0(base_url, "~",profile_fields,"?format=json")
  get <- GET(url, config = token)
  char <- rawToChar(get$content)
  json <- jsonlite::fromJSON(char)
  
  return(json)
  
}
