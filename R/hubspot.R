#' ####################################################################
#' #' Hubspot Contacts
#' #' 
#' #' This function brings ALL contacts with Hubspot's HAPI key
#' #' 
#' #' @family Credentials
#' #' @family Hubspot
#' #' @param limit Integer. Limit how many contacts you wish to bring
#' #' @param creds Character. Credential's user (see \code{get_creds()})
#' #' @export
#' hs_contacts <- function(limit=1000, creds = NA) {
#'   
#'   # require(httr)
#'   # require(jsonlite)
#'   # require(dplyr)
#'   # require(plyr)
#'   # require(rlist)
#'   # require(anytime)
#'   
#'   options(warn=-1)
#'   
#'   credentials <- get_credentials("hubspot", dir = creds)
#'   token <- credentials$token
#'   
#'   API <- "https://api.hubapi.com/contacts/v1/lists/all/contacts/all?"
#'   
#'   order <- c("createdate", "lastmodifieddate", "tipo_contacto", "lifecyclestage",
#'              # PERSONAL DATA
#'              "firstname", "lastname", "email", "fecha_de_nacimiento", "edad", "city", "country", "state",
#'              "phone", "mobilephone", "identificacion", "tipo_documento_identidad", "fecha_expedici_n_documento",
#'              "tipo_de_contrato", "ingresos", "gastos_mensuales", "fecha_ingreso_trabajo", "actividad_econ_mica",
#'              "contacto_deudor", "numero_doc_deudor",
#'              # ANALYTICS
#'              "hubspotscore", "hs_analytics_average_page_views", "num_conversion_events", "hs_analytics_num_event_completions",
#'              "hs_analytics_num_visits", "hs_analytics_last_timestamp", "hs_analytics_last_visit_timestamp",
#'              "hs_social_last_engagement", "hs_email_domain", "hs_lead_status", "hs_analytics_source")
#'   
#'   endfx <- function(x) {
#'     colnames(x) <- gsub("contacts_", "", gsub("_value", "", gsub("properties_", "", gsub("profile_", "", colnames(x)))))
#'     x <- data.frame(rlist::list.cbind(lapply(x, unlist(as.character)))) %>%
#'       mutate_at(vars(contains('valid')), funs(as.logical(.))) %>%
#'       mutate_at(vars(contains('date')), funs(as.character(
#'         as.POSIXct(as.numeric(as.character(.))/1000, origin="1970-01-01") - 5*3600))) %>%
#'       mutate_at(vars(contains('timestamp')), funs(as.character(
#'         as.POSIXct(as.numeric(as.character(.))/1000, origin="1970-01-01") - 5*3600))) %>%
#'       select(-identity_profiles, -token, -url, -merge_audits, -has_more, -vid_offset, -addedAt,
#'              -portal_id, -is_contact, -form_submissions, -canonical_vid, -merged_vids) %>%
#'       select(vid, one_of(order), everything()) %>% arrange(desc(createdate))
#'     x[x == "list()"] <- NA
#'     x[x == "integer(0)"] <- 0
#'     return(x)
#'   }
#'   
#'   hubspot_limit <- 100
#'   n <- ifelse(limit > hubspot_limit, ceiling(limit/hubspot_limit), 1)
#'   output <- NULL
#'   
#'   for (i in 1:n) {
#'     
#'     # Define Parameters
#'     offset <- ifelse(i == 1, 0, import$vid_offset[nrow(import)])
#'     limit <- ifelse(i == n, hubspot_limit - ((n*hubspot_limit) - limit), hubspot_limit)
#'     properties <- paste(rbind("property=", order,"&"), collapse="")
#'     parameters <- c(paste0("vidOffset=", offset), paste0("count=", limit), hapikey = paste0("hapikey=", token))
#'     parameters <- paste(parameters, collapse="&")
#'     URL <- paste0(API, properties, parameters)
#'     import <- bring_api(URL)
#'     imports <- select(import, -contains("_versions"), -contains("_source"), -contains("_timestamp"))
#'     
#'     output <- plyr::rbind.fill(output, imports)
#'     
#'     # Stop if there are no more deals
#'     if (i > 1 & import$has_more[nrow(import)] == FALSE) {
#'       output <- endfx(output)
#'       output <- mutate_at(output, vars(contains('fecha_')), funs(anytime::anydate(as.character(.))))
#'       message(paste0("All contacts (", dim(output)[1],") brought. Stopping my work now!"))
#'       return(output)
#'       break
#'     }
#'     
#'     # Finished the loop
#'     if (i==n) {
#'       output <- endfx(output)
#'       output <- mutate_at(output, vars(contains('fecha_')), funs(anytime::anydate(as.character(.))))
#'       message(paste(dim(output)[1],"contacts brought"))
#'       return(output)
#'     }
#'   }
#' }
#' 
#' 
#' ####################################################################
#' #' Hubspot Deals
#' #' 
#' #' This function brings ALL deals with Hubspot's HAPI key
#' #' 
#' #' @family Credentials
#' #' @family Hubspot
#' #' @param limit Integer. Limit how many contacts you wish to bring
#' #' @param creds Character. Credential's user (see \code{get_creds()})
#' #' @export
#' hs_deals <- function(limit=10000, creds = NA) {
#'   
#'   # require(httr)
#'   # require(jsonlite)
#'   # require(dplyr)
#'   # require(plyr)
#'   # require(rlist)
#'   
#'   options(warn=-1)
#'   
#'   credentials <- get_credentials("hubspot", dir = creds)
#'   token <- credentials$token
#'   
#'   API <- "https://api.hubapi.com/deals/v1/deal/paged?"
#'   
#'   order <- c("dealname", "createdate", "dealstage", "amount",
#'              "num_associated_contacts", "dealtype",
#'              "closed_lost_reason", "closedate",
#'              "utm_source", "utm_medium", "utm_campaign",
#'              # VEHICLE AND APPLICATION DATA
#'              "valor_del_veh_culo", "modelo_del_veh_culo", "marca_del_veh_culo",
#'              "vendedor_del_veh_culo", "paso_de_la_compra", "tipo_de_veh_culo", "monto_cuota_inicial",
#'              "fuente_del_veh_culo", "tipo_de_cr_dito", "web_del_veh_culo",
#'              "plazo_del_cr_dito", "ciudad_de_circulaci_n_del_veh_culo", "solicitantes_del_cr_dito",
#'              "concesionario_del_veh_culo", "periodico_revista_del_veh_culo",
#'              # VALIDATION DATA
#'              "valido_politicas", "validacion_tipo_vehiculo", "validacion_edad", "validacion_marca_vehiculo",
#'              "validacion_cuota_inicial", "validacion_ingresos_mensuales", "validacion_monto_prestamo",
#'              "validacion_sector_empresa", "validacion_tipo_documento", "validacion_modelo_vehiculo",
#'              "validacion_tipo_contrato", "validacion_fecha_ingreso", "validacion_ciudad_vehiculo",
#'              "validacion_ciudad_residencia", "validacion_consulta_buro", "validacion_intencion_compra",
#'              # ANALYTICS
#'              "hs_lastmodifieddate", "num_contacted_notes", "notes_next_activity_date",
#'              "notes_last_updated", "engagements_last_meeting_booked", "notes_last_contacted", "num_notes")
#'   
#'   endfx <- function(x) {
#'     colnames(x) <- gsub("associations_", "", gsub("_value", "", gsub("properties_", "", gsub("deals_", "", colnames(x)))))
#'     x <- data.frame(rlist::list.cbind(lapply(x, unlist(as.character)))) %>%
#'       mutate_at(vars(contains('valid')), funs(as.logical(.))) %>%
#'       mutate_at(vars(contains('date')), funs(as.character(
#'         as.POSIXct(as.numeric(as.character(.))/1000, origin="1970-01-01") - 5*3600)))
#'     x[x == "list()"] <- NA
#'     x[x == "integer(0)"] <- 0
#'     x <- select(x, -portalId, -hasMore, -imports, -stateChanges, -associatedCompanyIds, -offset)
#'     dealstagesAPI <- paste0("https://api.hubapi.com/deals/v1/pipelines?hapikey=",token)
#'     get <- httr::GET(url = dealstagesAPI)
#'     dealstages <- fromJSON(rawToChar(get$content))
#'     dealstages <- dealstages$stages[[1]]
#'     dealstages <- data.frame(code = as.character(dealstages$stageId),
#'                              dealstages = dealstages$label,
#'                              stage_order = dealstages$displayOrder + 1,
#'                              probability = dealstages$probability)
#'     x <- left_join(x, dealstages, by = c("dealstage"="code")) %>% select(-dealstage) %>%
#'       mutate_at(vars(contains('date')), funs(as.POSIXct(.))) %>%
#'       mutate_at(vars(contains('valid')), funs(as.logical(.))) %>%
#'       select(dealId, associatedVids, dealstages, one_of(order), everything()) %>%
#'       arrange(desc(createdate))
#'     colnames(x) <- gsub("veh_culo", "vehiculo", colnames(x))
#'     return(x)
#'   }
#'   
#'   hubspot_limit <- 250
#'   n <- ifelse(limit > hubspot_limit, ceiling(limit/hubspot_limit), 1)
#'   output <- NULL
#'   
#'   for (i in 1:n) {
#'     
#'     # Define Parameters
#'     offset <- ifelse(i == 1, 0, import$offset[nrow(import)])
#'     limit <- ifelse(i == n, hubspot_limit - ((n*hubspot_limit) - limit), hubspot_limit)
#'     properties <- paste(rbind("properties=", order,"&"), collapse="")
#'     parameters <- c(paste0("offset=", offset), paste0("limit=", limit), hapikey = paste0("hapikey=", token))
#'     parameters <- paste(parameters, collapse="&")
#'     URL <- paste0(API, properties, parameters)
#'     import <- bring_api(URL)
#'     
#'     imports <- select(import, everything(),
#'                       -contains("_versions"), -contains("_source"), -contains("_timestamp"),
#'                       contains("utm_source_value"))
#'     
#'     output <- plyr::rbind.fill(output, imports)
#'     
#'     # Stop if there are no more deals
#'     if (i > 1 & import$hasMore[nrow(import)] == FALSE) {
#'       output <- endfx(output)
#'       message(paste0("All deals (", dim(output)[1],") brought. Stopping my work now!"))
#'       return(output)
#'       break
#'     }
#'     
#'     # Finished the loop
#'     if (i==n) {
#'       output <- endfx(output)
#'       message(paste(dim(output)[1], "deals brought"))
#'       return(output)
#'     }
#'   }
#' }
