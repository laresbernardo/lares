#' lares: R library for Analytics, Visualization & Machine Learning Tasks

#' @importFrom beepr beep
#' @importFrom circlize CELL_META chordDiagram circos.text circos.track uh
#' @importFrom config get
#' @importFrom corrplot corrplot
#' @importFrom DBI dbDriver dbConnect dbSendQuery fetch dbDisconnect
#' @importFrom devtools install install_github
#' @import dplyr
#' @importFrom forecast Arima auto.arima forecast
#' @import ggplot2
#' @importFrom ggrepel geom_label_repel geom_text_repel
#' @importFrom googleAuthR gar_auth
#' @importFrom googlesheets gs_auth gs_read gs_edit_cells gs_title
#' @importFrom graphics box hist plot points rasterImage rect grid 
#' @importFrom grDevices graphics.off dev.off png
#' @importFrom gridExtra grid.arrange tableGrob arrangeGrob ttheme_minimal 
#' @importFrom h2o as.h2o h2o.automl h2o.accuracy h2o.getModel h2o.performance h2o.init 
#' h2o.removeAll h2o.download_mojo h2o.download_pojo h2o.loadModel h2o.no_progress h2o.predict 
#' h2o.predict_json h2o.saveModel h2o.varimp
#' @importFrom httr GET POST oauth_endpoint oauth_app oauth1.0_token authenticate 
#' stop_for_status upload_file add_headers
#' @importFrom jsonlite fromJSON toJSON flatten
#' @importFrom lubridate date day week weeks month year wday dmy_hms dmy ymd_hms ymd 
#' minute hour second %m+% floor_date 
#' @importFrom magrittr "%>%"
#' @importFrom openxlsx addWorksheet copyWorkbook loadWorkbook read.xlsx removeWorksheet 
#' getSheetNames renameWorksheet saveWorkbook sheets write.xlsx
#' @importFrom pROC roc ci
#' @importFrom quantmod getDividends getSymbols getQuote 
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rdrop2 drop_auth drop_dir drop_download drop_search drop_upload
#' @import rlist
#' @importFrom rmarkdown render
# @importFrom recipes recipe step_num2factor step_meanimpute all_numeric step_modeimpute
# all_nominal step_scale all_outcomes step_dummy prep
#' @import reshape2
#' @importFrom rvest html_node html_nodes html_attrs html_attr html_table html_text 
#' @importFrom scales percent comma
#' @importFrom sp over coordinates proj4string proj4string<- coordinates<- spTransform
#' @importFrom stats cor quantile complete.cases na.omit sd median dist end lm predict 
#' reorder start kmeans var xtabs 
#' @importFrom stringr str_locate_all 
#' @importFrom tm content_transformer Corpus removeNumbers removePunctuation removeWords 
#' stopwords stripWhitespace TermDocumentMatrix tm_map VectorSource
#' @importFrom utils head tail packageVersion URLencode capture.output data download.file 
#' globalVariables installed.packages write.table install.packages remove.packages object.size 
#' type.convert flush.console read.table modifyList write.csv combn
#' @importFrom wordcloud wordcloud textplot
#' @importFrom xml2 read_html
"_PACKAGE"

if(getRversion() >= "2.15.1")
  globalVariables(
    c(".","..y..","!!!",".rs.restartR","Amount","Cash","Close","CreateDate","CumCash",
      "CumDiv","CumPortfolio","DailyCash","DailyDiv","DailyExpen","DailyStocks",
      "DailyTrans","DailyValue","Date","DateTimeOriginal","Deciles","DifPer","DifUSD",
      "DivIncome","DivPerc","DivReal","Expenses","FileModifyDate","High","Hist",
      "InvPerc","Invested","Perc","Quant","RealPerc","RelChangePHist","RelChangeUSD",
      "RelPer","RelUSD","StartUSD","StockIniValue","StockValue","Stocks","Symbol","results",
      "TotalPer","TotalUSD","Type","Value","readOGR","read_exif","pcum","fpr","tpr",
      "accuracy","add_headers","addedAt","amount","associatedCompanyIds","associatedVids",
      "canonical_vid","cats[, i]", "ceiling_date","color","contacts_canonical_vid","auc",
      "contacts_form_submissions","contacts_identity_profiles", "google_analytics","perc",
      "contacts_is_contact","contacts_merge_audits","contacts_portal_id","model.matrix",
      "contacts_profile_token","contacts_profile_url","contacts_vid","content","size",
      "createdate","credit","cuts","date_of_birth","days","dealId","send.mail","shapeflag",
      "dealstage","df_status","form_submissions","gather","ceiling_date","trim",
      "get_questionnaire","get_typeforms","endfx","to","Real","Pred","Freq","force_n",
      "hasMore","has_more","identification_date","identity_profiles","ggplotly","facet2",
      "image_darknet_detect","image_darknet_model","image_info","image_read","index",
      "is_contact","key","label","label_colours","label_hjust","max_score","merge_audits",
      "merged_vids","min_score","model_performance","month.lbl","numb","Adjusted",
      "p","p_error","palette_light","portalId","portal_id","pos","get_ip","score",
      "prediction_breakdown","q_na","quantile_tag","question","real_error","dummy","sale",
      "stateChanges","step_done","str_pad","tag","test_auc","test_ll","theme_tq",
      "tk_augment_timeseries_signature","tk_get_timeseries_signature","tk_index","dfl",
      "tk_make_future_timeseries","token","train_auc","train_ll","dft","Ticker",
      "trees","type","upload_file","value","values","variable_response","variables",
      "vehicle_commercial","verbose","vid","vid_offset","wday.lbl","x","y","periods",
      "cols","nums","char","logic","counter","metric","End","Start","cv","label_pos",
      "name","where","deciles","gg_pos","colour","ptag","create_token","search_tweets",
      "..level..","cluster","drop_na","shapes","coords","long","lat","group","longitude",
      "latitude","as","CRS","project","minutes","seconds","Volume","BuySell","groupi",
      "StocksValue","Dividend","Expense","Deposit","Invest","Deposited","Dividends",
      "Portfolio","start_clean","geo","hits","keyword","legend","subject","subject.x",
      "subject.y","type_label","facet","Total","pal","p_real","weight","ACC","AUC",
      "Logloss","yewxname","gain","random","optimal","lift","pred","prophet","fit.prophet",
      "prophet_plot_components","add_country_holidays","make_future_dataframe",
      "percentile","cum_response","response","total")) 
