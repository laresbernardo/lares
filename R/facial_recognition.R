#' ####################################################################
#' #' Facial Recognition: Take a Selfie
#' #' 
#' #' This function lets the user take a selfie with its front laptop cam. 
#' #' This tools doesn't work well with RStudio sometimes. Try running it in your Terminal.
#' #' 
#' #' @param save Boolean. Save output as an image
#' #' @param print Boolean. Print output
#' #' @export
#' fr_takeselfie <- function(save=FALSE, print=FALSE) {
#' 
#'   # require(opencv)
#'   options(warn = 0)
#' 
#'   if (!c("opencv") %in% installed.packages()) {
#'     warning(paste("No 'opencv' library installed yet!\n",
#'                   "First, run 'brew install opencv' in your terminal\n",
#'                   "Then, run 'devtools::install_github('ropenscilabs/opencv')' in R\n",
#'                   "NOTE: Works best in the terminal (RStudio doesn't like the popup window)"))
#'   }
#'   
#'   webcam <- ocv_camera()
#'   if (save == TRUE) {
#'     ocv_write(webcam, 'selfie.jpg')
#'   }
#'   if (print == TRUE) {
#'     print(webcam)
#'   }
#' }
#' 
#' 
#' ####################################################################
#' #' Facial Recognition: Live Cam
#' #' 
#' #' This function lets the user establish a live recoring with its front laptop cam. 
#' #' This tools doesn't work well with RStudio sometimes. Try running it in your Terminal.
#' #' 
#' #' @export
#' fr_live <- function() {
#' 
#'   # require(opencv)
#'   
#'   if (!c("opencv") %in% installed.packages()) {
#'     warning(paste("No 'opencv' library installed yet!\n",
#'                   "First, run 'brew install opencv' in your terminal\n",
#'                   "Then, run 'devtools::install_github('ropenscilabs/opencv')' in R\n",
#'                   "NOTE: Works best in the terminal (RStudio doesn't like the popup window)"))
#'   }
#' 
#'   test <- ocv_camera()
#'   bitmap <- ocv_bitmap(test)
#'   width <- dim(bitmap)[2]
#'   height <- dim(bitmap)[3]
#'   # Generates the plot
#'   makeplot <- function(x){
#'     png('bg.png', width = width, height = height, res = 96)
#'     on.exit(unlink('bg.png'))
#'     groups <- seq(0, width, length.out = 4)
#'     left <- rep("left", sum(x < groups[2]))
#'     middle <- rep("middle", sum(x >= groups[2] & x < groups[3]))
#'     right <- rep("right", sum(x >= groups[3]))
#'     f <- factor(c(left, middle, right), levels = c('left', 'middle', 'right'),
#'                 labels = c("Tidy!", "Whatever Works", "Base!"))
#'     color = I(c("#F1BB7B", "#FD6467", "#5B1A18"))
#'     plot(f, ylim = c(0, 5),
#'          main = "Are you a tidyer or baser?", col = color)
#'     dev.off()
#'     ocv_read('bg.png')
#'   }
#' 
#'   # Overlays faces on the plot
#'   ocv_video(function(input){
#'     mask <- ocv_facemask(input)
#'     faces <- attr(mask, 'faces')
#'     bg <- makeplot(faces$x)
#'     return(ocv_copyto(input, bg, mask))
#'   })
#' }
#' 
#' 
#' ####################################################################
#' #' Facial Recognition: Face Counter
#' #' 
#' #' This function lets the user detect and count faces in an image.
#' #' This tools doesn't work well with RStudio sometimes. Try running it in your Terminal.
#' #' 
#' #' @param img Character. Image file
#' #' @export
#' fr_facecounter <- function(img) {
#' 
#'   # require(opencv)
#'   # require(httr)
#'   
#'   if (!c("opencv") %in% installed.packages()) {
#'     warning(paste("No 'opencv' library installed yet!\n",
#'                   "First, run 'brew install opencv' in your terminal\n",
#'                   "Then, run 'devtools::install_github('ropenscilabs/opencv')' in R\n",
#'                   "NOTE: Works best in the terminal (RStudio doesn't like the popup window)"))
#'   }
#' 
#'   pic <- ocv_read(img)
#'   l <- list()
#'   l$facemask <- ocv_facemask(pic)
#'   l$faces <- ocv_face(pic)
#'   return(l)
#' }
#' 
#' fr_getimage <- function(img_url=NA) {
#'   if (!is.na(img_url)) {
#'     f <- tempfile()
#'     download.file(img_url, f, mode="wb")
#'     pic <- upload_file(f)
#'     return(pic)
#'   }
#' }
#' 
#' 
#' ####################################################################
#' #' Facial Recognition: Send Image
#' #' 
#' #' This tools doesn't work well with RStudio sometimes. Try running it in your Terminal.
#' #' 
#' #' @param type Integer
#' #' @param body Character. Body
#' #' @param endpoint Character. Endpoint URL
#' #' @param key Character. Key
#' #' @export
#' fr_sendimage <- function(type, body, endpoint, key) {
#' 
#'   # require(httr)
#'   # require(reshape2)
#' 
#'   content_type <- ifelse(type == 1, 'application/octet-stream', 'application/json')
#'   response <- POST(url = endpoint, body = body,
#'                    add_headers(.headers = c('Content-Type' = content_type,
#'                                             'Ocp-Apim-Subscription-Key' = key)))
#'   result <- content(response)
#' 
#'   if (length(result$error) == 0) {
#'     df2 <- data.frame(result)
#'     return(df2)
#'   } else {
#'     message(paste(content(response)$error$message, body))
#'   }
#' }
#' 
#' 
#' ####################################################################
#' #' Facial Recognition: Plot Face
#' #' 
#' #' This tools doesn't work well with RStudio sometimes. Try running it in your Terminal.
#' #' 
#' #' @param img_url Character. Image's URL
#' #' @param df Dataframe
#' #' @export
#' fr_plotface <- function(img_url, df) {
#' 
#'   # require(magick)
#'   # require(jpeg)
#'   # require(png)
#' 
#'   # Image characteristics
#'   img1props <- image_info(image_read(img_url))
#' 
#'   f <- tempfile()
#'   download.file(img_url, f, mode="wb")
#'   pic <- upload_file(f)
#' 
#'   if (grepl("jpeg|jpg",img_url)) {
#'     img1 <- readJPEG(f)
#'   }
#' 
#'   if (grepl("png",img_url)) {
#'     img1 <- readPNG(f)
#'   }
#' 
#'   # Start plotting
#'   plot(0:1, 0:1,
#'        xlim=range(c(0,img1props$width)),
#'        ylim=range(c(0,img1props$height)), type='n', asp=1)
#'   rasterImage(img1, 0, 0, img1props$width, img1props$height, interpolate=F)
#' 
#'   # Tip of the nose
#'   tip_nose_x = as.numeric(df$value[grepl("faceLandmarks.noseTip.x", df$variable)])
#'   tip_nose_y = as.numeric(df$value[grepl("faceLandmarks.noseTip.y", df$variable)])
#'   points(tip_nose_x, img1props$height - tip_nose_y, pch=19, col="orange")
#' 
#'   # Left pupil location
#'   left_pupil_x = as.numeric(df$value[grepl("faceLandmarks.pupilLeft.x", df$variable)])
#'   left_pupil_y = as.numeric(df$value[grepl("faceLandmarks.pupilLeft.y", df$variable)])
#'   points(left_pupil_x, img1props$height - left_pupil_y, pch=19, col="green")
#' 
#'   # Right pupil location
#'   right_pupil_x = as.numeric(df$value[grepl("faceLandmarks.pupilRight.x", df$variable)])
#'   right_pupil_y = as.numeric(df$value[grepl("faceLandmarks.pupilRight.y", df$variable)])
#'   points(right_pupil_x, img1props$height - right_pupil_y, pch=19, col="green")
#' 
#'   # Face rectangle
#' 
#'   xleft <- as.numeric(df$value[grepl("faceRectangle.left", df$variable)])
#'   ytop <- img1props$height - as.numeric(df$value[grepl("faceRectangle.top", df$variable)])
#'   ybottom <- ytop - as.numeric(df$value[grepl("faceRectangle.height", df$variable)])
#'   xright <- as.numeric(df$value[grepl("faceRectangle.left", df$variable)]) +
#'     as.numeric(df$value[grepl("faceRectangle.width", df$variable)])
#'   rect(xleft, ybottom, xright, ytop, col=NA, border="magenta", lwd=2)
#' }
#' 
#' 
#' ####################################################################
#' #' Facial Recognition: Get REsults
#' #' 
#' #' This tools doesn't work well with RStudio sometimes. Try running it in your Terminal.
#' #' 
#' #' @param imgUrl Character. Image's URL
#' #' @param print Boolean. Print results
#' #' @export
#' fr_getresult <- function(imgUrl, print=T) {
#'   
#'   # require(httr)
#'   # require(jpeg)
#'   # require(reshape2)
#'   # require(magick)
#' 
#'   options(warn=-1)
#' 
#'   url <- paste0("https://westcentralus.api.cognitive.microsoft.com/face/v1.0", "/detect",
#'                  "?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=",
#'                  "age,gender,smile,headPose,facialHair,glasses,emotion")
#'   Key1 <- "03f9ceca5588476e9ee57f780f147fae"
#' 
#'   f1 <- fr_getimage(imgUrl)
#'   df <- fr_sendimage(type = 1, body = f1, endpoint = url, key = Key1)
#'   df <- melt(df, id=c("faceId"))
#'   l <- list()
#'   l$data <- df
#'   if (print == TRUE) {
#'     l$plot_face1 <- fr_plotface(img_url = imgUrl, df = df)
#'   }
#'   return(l)
#' }
#' 
#' 
#' ####################################################################
#' #' Facial Recognition: Compare faces
#' #' 
#' #' This tools doesn't work well with RStudio sometimes. Try running it in your Terminal.
#' #' 
#' #' @param img1Url Character. Image's URL for first subject
#' #' @param img2Url Character. Image's URL for second subject
#' #' @param print1 Boolean. Print first result
#' #' @param print2 Boolean. Print second result
#' #' @export
#' fr_compare <- function(img1Url, img2Url, print1=F, print2=F) {
#' 
#'   # require(httr)
#'   # require(jpeg)
#'   # require(reshape2)
#' 
#'   options(warn=-1)
#' 
#'   Endpoint <- "https://westcentralus.api.cognitive.microsoft.com/face/v1.0"
#'   Key1 <- "03f9ceca5588476e9ee57f780f147fae"
#'   Key2 <- "f0aa742ccb354770b0ad5b71759132d5"
#'   url1 <- paste0(Endpoint, "/detect",
#'                  "?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=",
#'                  "age,gender,smile,headPose,facialHair,glasses,emotion")
#'   url2 <- paste0(Endpoint, "/findsimilars")
#' 
#'   # Download and Process images
#'   f1 <- fr_getimage(img1Url)
#'   f2 <- fr_getimage(img2Url)
#' 
#'   # Send the first picture to Face API
#'   df_first <- fr_sendimage(type = 1, body = f1, endpoint = url1, key = Key1)
#'   df_first <- reshape2::melt(df_first, id=c("faceId"))
#' 
#'   # Send the second picture to Face API
#'   df_second <- fr_sendimage(type = 1, body = f2, endpoint = url1, key = Key1)
#'   df_second <- reshape2::melt(df_second, id=c("faceId"))
#' 
#'   # Detect similarity
#'   bodyFindSimilar <- sprintf('{"faceId": "%s", "faceIds": ["%s", "%s"], "mode": "%s"}',
#'                              df_first$faceId[1], df_first$faceId[1], df_second$faceId[1], "matchPerson")
#' 
#'   # Send both pictures to Face API - Find Similar
#'   df_similar <- fr_sendimage(type = 2, body = bodyFindSimilar, endpoint = url2, key = Key1)
#' 
#'   if (length(df_similar) != 0) {
#'     print(paste("Similarity between first and second picture:", df_similar$confidence.1))
#'   }
#' 
#'   # Final Output
#'   l <- list()
#'   l$similarity <- df_similar
#'   l$first <- df_first
#'   l$second <- df_second
#'   if (print1 == TRUE) {
#'     l$plot_face1 <- fr_plotface(img_url = img1Url, df = df_first)
#'   }
#'   if (print2 == TRUE) {
#'     l$plot_face2 <- fr_plotface(img_url = img2Url, df = df_second)
#'   }
#' 
#'   return(l)
#' 
#' }
#' 
#' # Faces in a picture
#' # faces <- fr_facecounter("https://i.imgur.com/gQWeYKj.png")
#' 
#' # Take a selfie
#' # fr_takeselfie(print=TRUE)
#' 
#' # Live faces detection
#' # faces <- fr_facecounter("https://i.imgur.com/gQWeYKj.png")
#' 
#' # Compare two faces
#' #link1 <- "https://i.imgur.com/zr4DouQ.jpg"
#' #link2 <- "https://i.imgur.com/ludH8hc.png"
#' #df <- fr_compare(img1 = link1, img2 = link2, print1=T, print2=T)
#' #df$similarity
#' 
#' # Get one face data and plot
#' #df <- fr_getresult(link1, plot=T)
#' #df <- fr_getresult("https://www.air-tan.com/wp-content/uploads/2016/05/sunglasses.jpg")
#' #df$data$value[grepl("lasses", df$data$value)]
#' #df$data$value[grepl("male", df$data$value)]
#' 
#' # Images for examples:
#' # cedula_ven <- "https://i.imgur.com/RwUaXwe.jpg"
#' # cedula_col <- "https://i.imgur.com/gQWeYKj.png"
#' # selfie <- "https://i.imgur.com/Mrn10Rr.jpg"
#' 
