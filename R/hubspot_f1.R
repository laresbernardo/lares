#' ####################################################################
#' #' Hubspot contacts (Somos F1)
#' #'
#' #' This function brings all contacts from Somos F1's Hubspot account. 
#' #' It also transforms the API's JSON result data into a relational table
#' #' or data.frame. Keep in mind that it will always bring the latest
#' #' value for each parameter (column).
#' #'
#' #' @family Credentials
#' #' @family Hubspot
#' #' @param limit Integer. Limit the amount of contacts to return. 
#' #' Multiples of 100 or round up will occur.
#' #' @param creds Character. Credential's user (see \code{get_creds()})
#' #' @export
#' f1_contacts <- function(limit = 1000, creds = NA) {
#'   
#'   hsdates <- function(date) {
#'     as.character(
#'       as.POSIXct(
#'         as.numeric(
#'           as.character(date)) / 1000, 
#'         origin="1970-01-01"))
#'   }
#'   
#'   properties <- c(
#'     # BASIC
#'     "contacts_vid",
#'     "createdate",
#'     "lastmodifieddate",
#'     "email",
#'     "product",
#'     "firstname",
#'     "secondname",
#'     "lastname",
#'     "secondlastname",
#'     "date_of_birth",
#'     "identification",
#'     "identification_date",
#'     "identification_department",
#'     "identification_type",
#'     "identification_city",
#'     "identification_country",
#'     "birth_city",
#'     "birth_department",
#'     "country_birth",
#'     "address",
#'     "home_city",
#'     "home_locality",
#'     "city",
#'     "departament",
#'     "country",
#'     "home_country",
#'     "mobilephone",
#'     # CREDITS
#'     "amount_requested",
#'     "loan_use",
#'     "debt_payees_counter",
#'     "payroll_deduction",
#'     "credit_value",
#'     "disbursed",
#'     "monthly_rate",
#'     "vehicle_brand",
#'     "vehicle_model",
#'     "vehicle_commercial_value",
#'     "vehicle_line",
#'     "vehicle_class",
#'     "vehicle_service_type",
#'     "vehicle_use",
#'     "sale_value",
#'     "credit_entity",
#'     "credit_installments",
#'     "payment_day",
#'     "dependents",
#'     "country_nationality",
#'     "civil_status",
#'     "educational_level",
#'     "profession",
#'     "contract_type",
#'     "economic_activity",
#'     "independent_type",
#'     "residence_months",
#'     "housing_type",
#'     "stratum",
#'     "sex",
#'     "violence_victim",
#'     "pep_public_resources",
#'     "pep_public_power",
#'     "pep_public_recognition",
#'     "working_continuity",
#'     "company",
#'     "company_nit",
#'     "company_relation",
#'     "company_activity",
#'     "company_position",
#'     "company_address",
#'     "company_locality",
#'     "company_city",
#'     "company_department",
#'     "company_phone",
#'     "company_phone_ext",
#'     "monthly_incomes",
#'     "monthly_outcomes",
#'     "total_assets",
#'     "total_liabilities",
#'     "reference1_firstname",
#'     "reference1_secondname",
#'     "reference1_lastname",
#'     "reference1_secondlastname",
#'     "reference1_city",
#'     "reference1_mobilephone",
#'     "reference2_firstname",
#'     "reference2_secondname",
#'     "reference2_firstlastname",
#'     "reference2_secondlastname",
#'     "family_relationship",
#'     "reference2_city",
#'     "reference2_mobilephone",
#'     "international_transactions",
#'     "country_transactions",
#'     "international_transactions_types",
#'     "international_tax_declaration",
#'     "international_tax_declaration_country",
#'     "foreign_currency",
#'     # FORM 4
#'     "file_identification",
#'     "files_labour_certificate",
#'     "files_payroll",
#'     # OTHER
#'     "lifecyclestage",
#'     "ipaddress",
#'     "ip_city",
#'     "ip_state",
#'     "ip_country",
#'     "hs_ip_timezone",
#'     "currentlyinworkflow",
#'     "hs_analytics_average_page_views",
#'     "hs_analytics_num_visits",
#'     "hs_social_last_engagement",
#'     "num_unique_conversion_events",
#'     "hs_analytics_num_event_completions",
#'     "first_conversion_event_name",
#'     "recent_conversion_event_name",
#'     "hs_analytics_num_page_views",
#'     "hs_lifecyclestage_lead_date",
#'     "hs_email_domain",
#'     "hs_analytics_source",
#'     "num_conversion_events",
#'     "hs_social_num_broadcast_clicks",
#'     "first_conversion_date",
#'     "hs_analytics_first_timestamp",
#'     "hs_analytics_first_visit_timestamp",
#'     "hs_analytics_first_touch_converting_campaign",
#'     "hs_analytics_last_timestamp",
#'     "hs_analytics_last_visit_timestamp",
#'     "hs_analytics_last_touch_converting_campaign",
#'     "hs_email_optout",
#'     "form_submissions")
#'   
#'   credentials <- get_credentials("hubspot", dir = creds)
#'   
#'   # ALL Contacts with properties
#'   properties_string <- paste(rbind("property=", properties, "&"), collapse="")
#'   API <- "https://api.hubapi.com/contacts/v1/lists/all/contacts/recent?"
#'   hapikey <- paste0("hapikey=", credentials$token, "&count=100")
#'   URL <- paste0(API, properties_string, hapikey)
#'   contacts <- bring_api(URL, status = TRUE)
#'   message(paste(nrow(contacts),"contacts ready..."))   
#'   while (!as.logical(contacts$has_more[1]) | nrow(contacts) < limit) {
#'     vidoffset <- contacts$vid_offset[nrow(contacts)]
#'     timeoffset <- contacts$time_offset[nrow(contacts)]
#'     URLi <- paste0(URL, "&vidOffset=", vidoffset, "&timeOffset=",timeoffset)
#'     contactsi <- bring_api(URLi, status = FALSE)
#'     if (!is.na(contactsi)) {
#'       contacts <- dplyr::bind_rows(contacts, contactsi)
#'       message(paste(nrow(contacts),"contacts ready..."))   
#'     } else { break }
#'   }
#'   message(paste(nrow(contacts),"total contacts imported!"))
#'   
#'   contacts$has_more <- contacts$vid_offset <- NULL
#'   colnames(contacts) <- gsub("contacts_properties_|_value", "", colnames(contacts))
#'   contacts <- contacts %>%
#'     dplyr::rename(credit_value = .data$credit,
#'                   #sale_value = .data$sale,
#'                   vehicle_commercial_value = .data$vehicle_commercial) %>%
#'     mutate_at(vars(contains('timestamp')), funs(hsdates(.))) %>%
#'     mutate_at(vars(contains('date')), funs(hsdates(.))) %>%
#'     mutate_at(vars(contains('addedAt')), funs(hsdates(.))) %>% 
#'     mutate(date_of_birth = as.Date(.data$date_of_birth),
#'            identification_date = as.Date(.data$identification_date))
#'   
#'   contacts$last_form_filled <- ifelse(
#'     grepl("146f9e71-b10e-4f7d-b0ef-8b65b122301f", contacts$contacts_form_submissions), "datos_personales",
#'     ifelse(grepl("851c6046-201a-4df9-8dab-8f2bf6a67a23", contacts$contacts_form_submissions), "pre_solicitud", 
#'            ifelse(grepl("4442aa58-bd8a-4c3f-9710-cb3f6ecb0096", contacts$contacts_form_submissions), "solicitud", 
#'                   ifelse(grepl("d9377981-7def-4489-9e2d-09b4b6ab755f", contacts$contacts_form_submissions), "documentos",
#'                          ifelse(grepl("e81e2002-b382-4c9d-90a4-75e6f6615132", contacts$contacts_form_submissions), "lead_soat",
#'                                 ifelse(grepl("4533ec08-dc7d-4c9e-8b5d-f521b6e6f28c", contacts$contacts_form_submissions), "blog_suscripcion",
#'                                        ifelse(grepl("7c8b4b79-f851-4f86-a00f-45d7dec89385", contacts$contacts_form_submissions), "contacto",
#'                                               NA)))))))
#'   
#'   contacts$last_form_filled_credit <- ifelse(
#'     grepl("e81e2002-b382-4c9d-90a4-75e6f6615132", contacts$contacts_form_submissions), "0_lead_soat",
#'     ifelse(grepl("146f9e71-b10e-4f7d-b0ef-8b65b122301f", contacts$contacts_form_submissions), "1_datos_personales",
#'            ifelse(grepl("851c6046-201a-4df9-8dab-8f2bf6a67a23", contacts$contacts_form_submissions), "2_pre_solicitud", 
#'                   ifelse(grepl("4442aa58-bd8a-4c3f-9710-cb3f6ecb0096", contacts$contacts_form_submissions), "3_solicitud", 
#'                          ifelse(grepl("d9377981-7def-4489-9e2d-09b4b6ab755f", contacts$contacts_form_submissions), "4_documentos",
#'                                 NA)))))
#'   
#'   contacts$step_done <- ifelse(
#'     !is.na(contacts$file_identification) | 
#'       !is.na(contacts$files_labour_certificate) | 
#'       !is.na(contacts$files_payroll), "documentos",
#'     ifelse(!is.na(contacts$credit_installments), "solicitud",
#'            ifelse(!is.na(contacts$economic_activity), "pre_solicitud",
#'                   ifelse(!is.na(contacts$firstname), "datos_personales", "lead"))))
#'   
#'   contacts <- contacts %>%
#'     select(-.data$contacts_identity_profiles, -.data$contacts_profile_url, 
#'            -.data$contacts_profile_token, -.data$contacts_portal_id,
#'            -.data$contacts_canonical_vid, -.data$contacts_is_contact, 
#'            -.data$contacts_merge_audits, -.data$contacts_form_submissions,
#'            -contains("files_payroll"), -contains("file_identification"), 
#'            -contains("files_labour")) %>%
#'     select(.data$contacts_vid, .data$step_done, one_of(properties), everything()) %>%
#'     dplyr::rename(., "vid" = "contacts_vid")
#'   
#'   # # One contact
#'   # API <- "https://api.hubapi.com/contacts/v1/contact/vid/1016501/profile?"
#'   # hapikey <- paste0("hapikey=", credentials$token)
#'   # URL <- paste0(API, hapikey)
#'   # get <- httr::GET(url = URL)
#'   # message(paste0("Status: ", ifelse(get$status_code == 200, "OK", "ERROR")))
#'   # char <- rawToChar(get$content)
#'   # import <- jsonlite::fromJSON(char)
#'   # last_property <- data.frame(lapply(import$properties, `[[`, 1)) %>%
#'   #   mutate_at(vars(contains('timestamp')), funs(hsdates(.))) %>%
#'   #   mutate_at(vars(contains('date')), funs(hsdates(.)))
#'   # colnames(last_property)[!colnames(last_property) %in% properties]
#'   
#'   # Final format
#'   contacts <- type.convert(contacts)
#'   posixs <- vector2text(c("timestamp","addedAt","_date", "createdate", "lastmodifieddate"), sep="|", quotes = F)
#'   dates <- vector2text(c("date_of_birth","identification_date"), sep="|", quotes = F)
#'   contacts <- contacts %>%
#'     mutate_at(vars(grep(posixs, colnames(.))), funs(as.POSIXct)) %>% 
#'     mutate_at(vars(grep(dates, colnames(.))), funs(as.Date))
#'   
#'   return(contacts) 
#'   
#' }
