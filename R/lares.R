#' Analytics, Data Mining & Machine Learning Sidekick
#'
#' R library for better/faster analytics, visualization, data mining, and machine learning tasks.
#'
#' @md
#' @name lares
#' @docType package
#' @author Bernardo Lares (laresbernardo@@gmail.com)
#' @importFrom dplyr any_of arrange as_tibble bind_cols bind_rows case_when contains
#' count desc distinct distinct_at everything filter first group_by inner_join
#' lag last left_join mutate mutate_all mutate_at mutate_if n ntile
#' one_of pull rename rename_at right_join row_number rowwise sample_n
#' select select_if slice starts_with summarise summarise_all
#' summarise_if summarize summarize_all tally tibble transmute_if ungroup vars
#' @importFrom ggplot2 aes after_stat annotate coord_equal coord_flip
#' element_blank element_line element_rect element_text enquo enquos expand_limits
#' facet_grid facet_wrap geom_abline geom_area geom_bar geom_boxplot
#' geom_col geom_density geom_hline geom_jitter geom_label geom_line
#' geom_path geom_point geom_polygon geom_raster geom_rect geom_segment
#' geom_smooth geom_text geom_tile geom_vline ggplot ggsave ggtitle
#' guide_colorbar guide_colourbar guide_legend guides labs last_plot
#' margin position_dodge position_fill position_jitter position_stack
#' quo_name quos rel scale_color_brewer scale_color_identity
#' scale_color_manual scale_colour_brewer scale_colour_continuous
#' scale_colour_gradient scale_colour_identity scale_colour_manual
#' scale_fill_brewer scale_fill_continuous scale_fill_gradient
#' scale_fill_grey scale_fill_identity scale_fill_manual
#' scale_shape_identity scale_size scale_x_continuous scale_x_date
#' scale_x_discrete scale_x_reverse scale_y_continuous sec_axis
#' stat_density_2d stat_ecdf theme theme_minimal unit
#' update_geom_defaults xlab xlim ylab ylim
#' @importFrom graphics box hist plot points rasterImage rect grid legend mtext
#' @importFrom grDevices graphics.off dev.off png dev.size
#' @importFrom h2o as.h2o h2o.automl h2o.accuracy h2o.getModel h2o.performance h2o.init
#' h2o.removeAll h2o.download_mojo h2o.download_pojo h2o.loadModel h2o.no_progress h2o.predict
#' h2o.predict_json h2o.saveModel h2o.varimp h2o.getVersion h2o.glm
#' predict_contributions.H2OModel h2o.import_mojo h2o.no_progress h2o.show_progress
#' @importFrom httr GET POST oauth_endpoint oauth_app oauth1.0_token authenticate
#' stop_for_status upload_file add_headers content http_error set_config config
#' @importFrom jsonlite fromJSON toJSON flatten
#' @importFrom lubridate date day week weeks month year wday dmy_hms dmy ymd_hms ymd days
#' minute hour second %m+% %m-% floor_date ceiling_date years
#' @importFrom magrittr %>% set_colnames set_names
#' @importFrom openxlsx addWorksheet copyWorkbook loadWorkbook read.xlsx removeWorksheet
#' getSheetNames renameWorksheet saveWorkbook sheets write.xlsx
#' @importFrom patchwork guide_area plot_layout plot_annotation wrap_plots
#' @importFrom pROC roc ci
#' @importFrom rlang as_label .data
#' @importFrom rpart rpart rpart.control
#' @importFrom rpart.plot rpart.rules rpart.plot
#' @importFrom rvest html_node html_nodes html_attrs html_attr html_table html_text
#' @importFrom stats cor quantile complete.cases na.omit sd median dist end lm predict
#' reorder start kmeans var xtabs as.formula prcomp p.adjust pt model.matrix qt cor.test
#' @importFrom stringr fixed str_count str_length str_pad str_replace_all str_split
#' str_to_title word
#' @importFrom tidyr gather spread
#' @importFrom utils head tail packageVersion URLencode capture.output data download.file
#' globalVariables installed.packages write.table install.packages remove.packages object.size
#' type.convert flush.console read.table modifyList write.csv combn browseURL type.convert
#' getParseData find read.delim
#' @importFrom yaml read_yaml
"_PACKAGE"


####################################################################
#' Install/Update Additional Recommended Libraries
#'
#' All needed libraries to use (most) lares are already a dependency.
#' There are some functions that many people won't event know exist
#' that will require other additional libraries. Also, this may be
#' used as a Docker way of installing useful libraries on an new instance.
#'
#' @param progress Boolean. Show status bar?
#' @export
install_recommended <- function(progress = TRUE) {
  for (lib in names(recommended)) {
    invisible(install.packages(lib, quiet = TRUE, verbose = FALSE))
    if (progress) {
      statusbar(which(lib == names(recommended)), length(recommended), lib, msg = "")
    }
  }
}

#' Pipe operator
#' @name lares-exports
NULL

#' @name %>%
#' @export
#' @rdname lares-exports
NULL

# Recommended additional libraries to fully take advantage of lares library
recommended <- list(
  beepr = "beep",
  circlize = c("chordDiagram", "uh"),
  DALEX = c("explain.default", "model_performance", "model_profile", "predict_parts"),
  data.table = "fread",
  DBI = c("dbDriver", "dbConnect", "dbSendQuery", "fetch", "dbDisconnect"),
  devtools = c("install", "install_github", "with_proxy"),
  exifr = "read_exif",
  factoextra = "fviz_nbclust",
  forecast = c("Arima", "auto.arima", "forecast"),
  gdata = "read.xls",
  ggbeeswarm = "geom_quasirandom",
  # ggforce = "geom_mark_ellipse",
  # ggrepel = "geom_label_repel",
  googleAnalyticsR = "google_analytics",
  googleAuthR = "gar_auth",
  googledrive = c("drive_auth", "drive_find", "local_drive_quiet"),
  googlesheets4 = c(
    "sheets_auth", "read_sheet", "gs4_create", "range_write",
    "gs4_auth_configure", "gs4_auth", "sheet_append"
  ),
  knitr = c("kable", "knit"),
  methods = "as",
  mice = c("mice", "complete"),
  plotly = c("ggplotly", "plot_ly", "add_markers", "add_markers"),
  prophet = c(
    "prophet", "fit.prophet", "prophet_plot_components",
    "add_country_holidays", "make_future_dataframe"
  ),
  quantmod = c("getDividends", "getSymbols", "getQuote"),
  rdrop2 = c("drop_auth", "drop_dir", "drop_download", "drop_search", "drop_upload"),
  rgdal = c("readOGR", "project"),
  RPostgreSQL = "PostgreSQL",
  Rtsne = "Rtsne",
  rtweet = c("create_token", "search_tweets"),
  rmarkdown = "render",
  skimr = "skim",
  syuzhet = "get_sentiment_dictionary",
  # sp = c("CRS", "over", "coordinates", "proj4string", "proj4string<-", "coordinates<-", "spTransform"),
  threed = c("mesh3dobj", "transform_by", "invert_matrix", "perspective_projection", "look_at_matrix"),
  tidytext = "unnest_tokens",
  tm = c(
    "content_transformer", "Corpus", "removeNumbers", "removePunctuation",
    "removeWords", "readPlain", "stopwords", "stripWhitespace", "TermDocumentMatrix",
    "tm_map", "VectorSource", "VCorpus"
  ),
  udpipe = c("keywords_rake", "udpipe_annotate", "udpipe_download_model", "udpipe_load_model"),
  wordcloud = c("wordcloud", "textplot")
)

# For read.file function... deprecated
# c("read.dta13", "read.spss")

if (getRversion() >= "2.15.1") {
  globalVariables(c(as.vector(unlist(recommended)), "."))
}
