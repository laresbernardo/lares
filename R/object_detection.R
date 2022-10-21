#' ####################################################################
#' #' Image's Objects Detection using YOLO
#' #'
#' #' Run devtools::install_github("bnosac/image",
#' #' subdir = "image.darknet", build_vignettes = TRUE)
#' #'
#' #' @param filename Character. Name of the file to analize
#' #' @param threshold Numeric. Range from 0 to 1. Detection threshold
#' #' @param type Character. Select 'faster' or 'better' for YOLO method
#' #' @export
#' object_detection <- function(filename, threshold = 0.2, type = "faster") {
#'
#'   # require(image.darknet)
#'
#'   path <- getwd()
#'   file <- paste0(path, "/", filename)
#'   message("File: ", file)
#'
#'   if (!file.exists(file)) {
#'     stop("That file doesn't exist or is not in your working directory!")
#'   }
#'
#'   if (!type %in% c("faster","better")) {
#'     stop("Please try 'faster' or 'better' on your type parameter!")
#'   }
#'
#'   if (threshold > 1 || threshold < 0) {
#'     stop("You should use a threshold value between 0 and 1")
#'   }
#'
#'   if (type == "faster") {
#'     model <- "tiny-yolo-voc.cfg"
#'     weights <- system.file(package="image.darknet", "models", "tiny-yolo-voc.weights")
#'     labels <- system.file(package="image.darknet", "include", "darknet", "data", "voc.names")
#'   }
#'
#'   if (type == "better") {
#'     weights <- file.path(system.file(package="image.darknet", "models"), "yolo.weights")
#'     if (!file.exists(weights)) {
#'       download.file(url = "http://pjreddie.com/media/files/yolo.weights", destfile = weights)
#'     }
#'     model <- "yolo.cfg"
#'     weights <- system.file(package="image.darknet", "models", "yolo.weights")
#'     labels <- system.file(package="image.darknet", "include", "darknet", "data", "coco.names")
#'   }
#'
#'   yolo <- image_darknet_model(
#'     type = 'detect',
#'     model = model,
#'     weights = weights,
#'     labels = labels)
#'
#'   x <- image_darknet_detect(
#'     file = file,
#'     object = yolo,
#'     threshold = threshold)
#'
#'   message("Succesfully exported 'predictions.png' into your working directory")
#'
#' }
