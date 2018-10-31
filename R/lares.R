#' lares: Analytics, Visualization & Machine Learning Library
#'

#' @importFrom beepr beep
#' @importFrom config get
#' @importFrom corrplot corrplot
#' @importFrom DBI dbDriver dbConnect dbSendQuery fetch dbDisconnect
#' @importFrom devtools install install_github
#' @import dplyr
#' @importFrom forecast Arima auto.arima forecast
#' @import ggplot2
#' @importFrom ggrepel geom_label_repel geom_text_repel
#' @importFrom googleAnalyticsR google_analytics
#' @importFrom googleAuthR gar_auth
#' @importFrom googlesheets gs_auth gs_read gs_edit_cells gs_title
#' @importFrom gridExtra grid.arrange
#' @importFrom h2o as.h2o h2o.automl h2o.accuracy h2o.getModel h2o.performance h2o.init h2o.removeAll
#' @importFrom kableExtra kable
#' @importFrom lubridate date day week weeks month year wday dmy_hms dmy ymd_hms ymd minute hour
#' @importFrom openxlsx addWorksheet copyWorkbook loadWorkbook read.xlsx removeWorksheet renameWorksheet saveWorkbook sheets write.xlsx
#' @importFrom pROC roc ci
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rdrop2 drop_auth drop_dir drop_download drop_search drop_upload
#' @importFrom rmarkdown render
#' @import reshape2
#' @import rlist   
#' @importFrom rtweet create_token search_tweets
#' @importFrom scales percent comma
#' @importFrom stats cor quantile complete.cases
#' @importFrom stats na.omit sd median dist end lm predict reorder start
#' @import SnowballC 
#' @importFrom utils head tail packageVersion URLencode capture.output data download.file installed.packages write.table
#' @importFrom wordcloud textplot wordlayout
#' @importFrom graphics box hist plot points rasterImage rect
"_PACKAGE"

utils::globalVariables(names=c(
  "aes","variable","geom_bar","geom_text","position","png","beep","send.mail","roc","render","message","print",
  "select","filter","mutate","mutate_if","mutate_at","select_if","arrange","group_by","tally","count",
  "guides","labs","theme","element_text","scale_y_continuous","position_dodge","ylim","guide_legend","scale_fill_discrete",
  "aes_string", "geom_boxplot","stat_summary", "theme_minimal", "theme_bw", "geom_vline", "geom_density", "margin",
  "scale_colour_continuous",'label','coord_flip','ylab','xlab','labs','geom_label','unit',
  'geom_line','geom_point','xlim','ylim','geom_segment','.','one_of',"head","tail", "type", "cuts",
  'gather','value','key'), 
  package = "lares", add = F)
