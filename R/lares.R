#' Analytics, Visualization & Machine Learning Tasks Library
#'
#' R library for better/faster analytics, visualization, data mining, and machine learning tasks.
#'
#' @md
#' @name lares
#' @docType package
#' @author Bernardo Lares (laresbernardo@@gmail.com)
#' @importFrom config get
#' @import dplyr
#' @import ggplot2
#' @importFrom graphics box hist plot points rasterImage rect grid legend
#' @importFrom grDevices graphics.off dev.off png dev.size
#' @importFrom h2o as.h2o h2o.automl h2o.accuracy h2o.getModel h2o.performance h2o.init 
#' h2o.removeAll h2o.download_mojo h2o.download_pojo h2o.loadModel h2o.no_progress h2o.predict 
#' h2o.predict_json h2o.saveModel h2o.varimp h2o.getVersion h2o.glm 
#' predict_contributions.H2OModel h2o.import_mojo
#' @importFrom httr GET POST oauth_endpoint oauth_app oauth1.0_token authenticate 
#' stop_for_status upload_file add_headers content http_error set_config config
#' @importFrom jsonlite fromJSON toJSON flatten
#' @importFrom lubridate date day week weeks month year wday dmy_hms dmy ymd_hms ymd days
#' minute hour second %m+% floor_date ceiling_date
#' @importFrom magrittr %>% set_colnames
#' @importFrom openxlsx addWorksheet copyWorkbook loadWorkbook read.xlsx removeWorksheet 
#' getSheetNames renameWorksheet saveWorkbook sheets write.xlsx
#' @importFrom patchwork guide_area plot_layout plot_annotation wrap_plots
#' @importFrom pROC roc ci
#' @import rlist
#' @importFrom rlang as_label .data
#' @importFrom rvest html_node html_nodes html_attrs html_attr html_table html_text 
#' @importFrom scales comma percent dollar
#' @importFrom stats cor quantile complete.cases na.omit sd median dist end lm predict 
#' reorder start kmeans var xtabs as.formula prcomp p.adjust pt model.matrix qt cor.test
#' @import stringr 
#' @importFrom tidyr gather spread
#' @importFrom utils head tail packageVersion URLencode capture.output data download.file 
#' globalVariables installed.packages write.table install.packages remove.packages object.size 
#' type.convert flush.console read.table modifyList write.csv combn browseURL type.convert 
#' getParseData find
#' @importFrom xml2 read_html
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
    if (progress)
      statusbar(which(lib == names(recommended)), length(recommended), lib, msg = "")
  }
}

#' Pipe operator
#' @name lares-exports
NULL

#' @name %>%
#' @export
#' @rdname lares-exports
NULL

# Recommeded additional libraries to fully take advantage of lares library
recommended <- list(
  beepr = c("beep"),
  circlize = c("chordDiagram", "uh"),
  DALEX = c("explain.default", "model_performance", "model_profile", "predict_parts"),
  data.table = c("fread"),
  DBI = c("dbDriver", "dbConnect", "dbSendQuery", "fetch", "dbDisconnect"),
  devtools = c("install", "install_github", "with_proxy"),
  exifr = c("read_exif"),
  factoextra = c("fviz_nbclust"),
  forecast = c("Arima", "auto.arima", "forecast"),
  gdata = c("read.xls"),
  ggbeeswarm = c("geom_quasirandom"),
  ggforce = c("geom_mark_ellipse"),
  ggrepel = c("geom_label_repel"),
  googleAnalyticsR = c("google_analytics"),
  googleAuthR = c("gar_auth"),
  googledrive = c("drive_auth", "drive_find"),
  googlesheets4 = c("sheets_auth", "read_sheet", "gs4_create", "range_write",
                    "gs4_auth_configure", "gs4_auth", "sheet_append"),
  methods = c("as"),
  mice = c("mice", "complete"),
  plotly = c("ggplotly", "plot_ly", "add_markers", "add_markers"),
  prophet = c("prophet", "fit.prophet", "prophet_plot_components",
              "add_country_holidays", "make_future_dataframe"),
  quantmod = c("getDividends", "getSymbols", "getQuote"),
  rdrop2 = c("drop_auth", "drop_dir", "drop_download", "drop_search", "drop_upload"),
  rgdal = c("readOGR", "project"),
  rpart = c("rpart", "rpart.control"),
  rpart.plot = c("rpart.plot"),
  RPostgreSQL = c("PostgreSQL"),
  rtweet = c("create_token", "search_tweets"),
  rmarkdown = c("render"),
  skimr = c("skim"),
  syuzhet = c("get_sentiment_dictionary"),
  sp = c("CRS", "over", "coordinates", "proj4string", "proj4string<-", "coordinates<-", "spTransform"),
  tm = c("content_transformer", "Corpus", "removeNumbers", "removePunctuation", 
         "removeWords","readPlain","stopwords", "stripWhitespace", "TermDocumentMatrix", 
         "tm_map", "VectorSource", "VCorpus"),
  udpipe = c("keywords_rake", "udpipe_annotate", "udpipe_download_model", "udpipe_load_model"),
  wordcloud = c("wordcloud", "textplot"))

# For read.file function... will be deprecated!
temp <- c("read.dta13", "read.spss")

if (getRversion() >= "2.15.1")
  globalVariables(c(as.vector(unlist(recommended)), temp, "."))
