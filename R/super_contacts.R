# Bring ALL contacts from API from Hubspot

super_contacts <- function(limit=1000, creds = NA) {

  suppressMessages(require(httr))
  suppressMessages(require(jsonlite))
  suppressMessages(require(dplyr))
  suppressMessages(require(plyr))
  suppressMessages(require(rlist))
  suppressMessages(require(anytime))

  options(warn=-1)

  credentials <- get_credentials("hubspot", dir = creds)
  token <- credentials$token

  API <- "https://api.hubapi.com/contacts/v1/lists/all/contacts/all?"

  order <- c("createdate", "lastmodifieddate", "tipo_contacto", "lifecyclestage",
             # PERSONAL DATA
             "firstname", "lastname", "email", "fecha_de_nacimiento", "edad", "city", "country", "state",
             "phone", "mobilephone", "identificacion", "tipo_documento_identidad", "fecha_expedici_n_documento",
             "tipo_de_contrato", "ingresos", "gastos_mensuales", "fecha_ingreso_trabajo", "actividad_econ_mica",
             "contacto_deudor", "numero_doc_deudor",
             # ANALYTICS
             "hubspotscore", "hs_analytics_average_page_views", "num_conversion_events", "hs_analytics_num_event_completions",
             "hs_analytics_num_visits", "hs_analytics_last_timestamp", "hs_analytics_last_visit_timestamp",
             "hs_social_last_engagement", "hs_email_domain", "hs_lead_status", "hs_analytics_source")

  endfx <- function(x) {
    colnames(x) <- gsub("contacts_", "", gsub("_value", "", gsub("properties_", "", gsub("profile_", "", colnames(x)))))
    x <- data.frame(rlist::list.cbind(lapply(x, unlist(as.character)))) %>%
      mutate_at(vars(contains('valid')), funs(as.logical(.))) %>%
      mutate_at(vars(contains('date')), funs(as.character(
        as.POSIXct(as.numeric(as.character(.))/1000, origin="1970-01-01") - 5*3600))) %>%
      mutate_at(vars(contains('timestamp')), funs(as.character(
        as.POSIXct(as.numeric(as.character(.))/1000, origin="1970-01-01") - 5*3600))) %>%
      select(-identity_profiles, -token, -url, -merge_audits, -has_more, -vid_offset, -addedAt,
             -portal_id, -is_contact, -form_submissions, -canonical_vid, -merged_vids) %>%
      select(vid, one_of(order), everything()) %>% arrange(desc(createdate))
    x[x == "list()"] <- NA
    x[x == "integer(0)"] <- 0
    return(x)
  }

  hubspot_limit <- 100
  n <- ifelse(limit > hubspot_limit, ceiling(limit/hubspot_limit), 1)
  output <- c()

  for (i in 1:n) {

    # Define Parameters
    offset <- ifelse(i == 1, 0, import$vid_offset[nrow(import)])
    limit <- ifelse(i == n, hubspot_limit - ((n*hubspot_limit) - limit), hubspot_limit)
    properties <- paste(rbind("property=", order,"&"), collapse="")
    parameters <- c(paste0("vidOffset=", offset), paste0("count=", limit), hapikey = paste0("hapikey=", token))
    parameters <- paste(parameters, collapse="&")
    URL <- paste0(API, properties, parameters)
    import <- bring_api(URL)
    imports <- select(import, -contains("_versions"), -contains("_source"), -contains("_timestamp"))

    output <- rbind.fill(output, imports)

    # Stop if there are no more deals
    if (i > 1 & import$has_more[nrow(import)] == FALSE) {
      output <- endfx(output)
      output <- mutate_at(output, vars(contains('fecha_')), funs(anytime::anydate(as.character(.))))
      message(paste0("All contacts (", dim(output)[1],") brought. Stopping my work now!"))
      return(output)
      break
    }

    # Finished the loop
    if (i==n) {
      output <- endfx(output)
      output <- mutate_at(output, vars(contains('fecha_')), funs(anytime::anydate(as.character(.))))
      message(paste(dim(output)[1],"contacts brought"))
      return(output)
    }
  }
}
