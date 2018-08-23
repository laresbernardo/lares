####################################################################
#' Hubspot contacts (Somos F1)
#'
#' This function brings all contacts from Somos F1's Hubspot account. 
#' It also transforms the API's JSON result data into a relational table
#' or data.frame. Keep in mind that it will always bring the latest
#' value for each parameter (column).
#'
#' @param creds Character. Credential's user (see get_credentials)
#' @export
f1_contacts <- function(creds = NA) {
  require(lares)
  require(httr)
  require(dplyr)
  
  hsdates <- function(date) {
    as.character(
      as.POSIXct(
        as.numeric(
          as.character(date)) / 1000, 
        origin="1970-01-01"))
  }
  
  properties <- c(
    # BASIC
    "contacts_vid",
    "createdate",
    "lastmodifieddate",
    "email",
    "product",
    "firstname",
    "secondname",
    "lastname",
    "secondlastname",
    "identification",
    "identification_date",
    "identification_department",
    "identification_type",
    "identification_city",
    "identification_country",
    "date_of_birth",
    "birth_city",
    "birth_department",
    "country_birth",
    "address",
    "home_city",
    "home_locality",
    "city",
    "departament",
    "country",
    "home_country",
    "mobilephone",
    "amount_requested",
    "credit_value",
    "vehicle_brand",
    "vehicle_model",
    "vehicle_commercial",
    "vehicle_commercial_value",
    "vehicle_line",
    "vehicle_class",
    "vehicle_service_type",
    "vehicle_use",
    "sale_value",
    "sale",
    "credit_entity",
    "credit_installments",
    "payment_day",
    "dependents",
    "country_nationality",
    "civil_status",
    "educational_level",
    "profession",
    "contract_type",
    "economic_activity",
    "independent_type",
    "independent_participation",
    "residence_months",
    "housing_type",
    "stratum",
    "sex",
    "violence_victim",
    "pep_public_resources",
    "pep_public_power",
    "pep_public_recognition",
    "working_continuity",
    "company",
    "company_nit",
    "company_relation",
    "company_activity",
    "company_position",
    "company_address",
    "company_locality",
    "company_city",
    "company_department",
    "company_phone",
    "company_phone_ext",
    "monthly_incomes",
    "monthly_outcomes",
    "patrimony",
    "total_assets",
    "total_liabilities",
    "reference1_firstname",
    "reference1_secondname",
    "reference1_lastname",
    "reference1_secondlastname",
    "reference1_city",
    "reference1_mobilephone",
    "reference2_firstname",
    "reference2_secondname",
    "reference2_firstlastname",
    "reference2_secondlastname",
    "family_relationship",
    "reference2_city",
    "reference2_mobilephone",
    "international_transactions",
    "country_transactions",
    "international_transactions_types",
    "international_tax_declaration",
    "international_tax_declaration_country",
    "foreign_currency",
    # FORM 4
    "file_identification",
    "files_labour_certificate",
    "files_payroll",
    # OTHER
    "lifecyclestage",
    "ipaddress",
    "ip_city",
    "ip_state",
    "ip_country",
    "hs_ip_timezone",
    "hs_analytics_average_page_views",
    "hs_analytics_num_visits",
    "hs_social_last_engagement",
    "num_unique_conversion_events",
    "hs_analytics_num_event_completions",
    "first_conversion_event_name",
    "recent_conversion_event_name",
    "hs_analytics_num_page_views",
    "hs_lifecyclestage_lead_date",
    "hs_email_domain",
    "hs_analytics_source",
    "num_conversion_events",
    "hs_social_num_broadcast_clicks",
    "first_conversion_date",
    "hs_analytics_first_timestamp",
    "hs_analytics_first_visit_timestamp",
    "hs_analytics_first_touch_converting_campaign",
    "hs_analytics_last_timestamp",
    "hs_analytics_last_visit_timestamp",
    "hs_analytics_last_touch_converting_campaign")
  
  credentials <- lares::get_credentials("hubspot", dir = creds)
  
  # ALL Contacts with properties
  properties_string <- paste(rbind("property=", properties, "&"), collapse="")
  API <- "https://api.hubapi.com/contacts/v1/lists/all/contacts/all?"
  hapikey <- paste0("hapikey=", credentials$token)
  URL <- paste0(API, properties_string, hapikey)
  contacts <- lares::bring_api(URL)
  colnames(contacts) <- gsub("contacts_properties_|_value", "", colnames(contacts))
  contacts <- contacts %>%
    mutate_at(vars(contains('timestamp')), funs(hsdates(.))) %>%
    mutate_at(vars(contains('date')), funs(hsdates(.))) %>%
    mutate_at(vars(contains('addedAt')), funs(hsdates(.))) %>% 
    mutate(date_of_birth = as.Date(date_of_birth),
           identification_date = as.Date(identification_date))
  
  contacts$last_form_filled <- ifelse(
    grepl("146f9e71-b10e-4f7d-b0ef-8b65b122301f", contacts$contacts_form_submissions), "datos_personales",
    ifelse(grepl("851c6046-201a-4df9-8dab-8f2bf6a67a23", contacts$contacts_form_submissions), "pre_solicitud", 
           ifelse(grepl("4442aa58-bd8a-4c3f-9710-cb3f6ecb0096", contacts$contacts_form_submissions), "solicitud", 
                  ifelse(grepl("d9377981-7def-4489-9e2d-09b4b6ab755f", contacts$contacts_form_submissions), "documentos", NA))))
  contacts$step_done <- ifelse(
    !is.na(contacts$file_identification) & 
      !is.na(contacts$files_labour_certificate) & 
      !is.na(contacts$files_payroll), "documentos",
    ifelse(!is.na(contacts$credit_installments), "solicitud",
           ifelse(!is.na(contacts$economic_activity), "pre_solicitud",
                  ifelse(!is.na(contacts$firstname), "datos_personales"))))
  
  contacts <- contacts %>%
    select(-contacts_form_submissions, -contacts_identity_profiles, 
           -contacts_profile_url, -contacts_profile_token, -contacts_portal_id,
           -contacts_canonical_vid, -contacts_is_contact, -contacts_merge_audits) %>%
    select(contacts_vid, step_done, one_of(properties), everything()) %>%
    dplyr::rename(., "vid" = "contacts_vid")
  
  # # One contact
  # API <- "https://api.hubapi.com/contacts/v1/contact/vid/1051/profile?"
  # hapikey <- paste0("hapikey=", credentials$token)
  # URL <- paste0(API, hapikey)
  # get <- GET(url = URL)
  # message(paste0("Status: ", ifelse(get$status_code == 200, "OK", "ERROR")))
  # char <- rawToChar(get$content)
  # last_property <- data.frame(lapply(import$properties, `[[`, 1)) %>%
  #   mutate_at(vars(contains('timestamp')), funs(hsdates(.))) %>%
  #   mutate_at(vars(contains('date')), funs(hsdates(.)))
  
  return(contacts) 
  
}
