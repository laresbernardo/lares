#' ####################################################################
#' #' Get Google's Geodata given the Addresses
#' #'
#' #' This function lets the user obtain Google's Geodata on given addresses
#' #'
#' #' @family Geographical
#' #' @family Credentials
#' #' @family Google
#' #' @param address Character Vector. Addresses you wish to query
#' #' @param country Character. Default Country if known
#' #' @param index Character Vector. If you wish to keep an id on each
#' #' address, set this values
#' #' @param creds Character. Credential's user (see \code{get_creds()})
#' #' @param which Character. Which of the APIs should be used?
#' #' @export
#' geoAddress <- function(address, country = "Argentina", index = NA, creds = NA, which = "api_01") {
#'
#'   try_require("rlist")
#'
#'   message("API Documentation: https://developers.google.com/maps/documentation/geocoding/usage-and-billing")
#'   c <- get_credentials(from = "google_api", dir = creds)
#'   message("API Geocoding user: ", as.character(c[grepl(paste0("user_",right(which, 2)), names(c))]))
#'
#'   getGeoDetails <- function(address){
#'
#'     c <- get_credentials(from = "google_api", dir = creds)
#'     url <- "https://maps.google.com/maps/api/geocode/json?address="
#'     api <- as.character(c[grepl(which, names(c))]) # api_01 is my personal free API
#'     ctry <- paste0("&components=country:", country)
#'     url <- URLencode(paste(url, address, ctry, "&key=", api, sep = ""))
#'     x <- fromJSON(url, simplifyVector = FALSE)
#'
#'     # Return Na's if we didn't get a match:
#'     if (x$status != "OK") return(message(paste(x$status, x$error_message)))
#'
#'     # Else, extract what we need from the Google server reply into a dataframe:
#'
#'     # Note: We bring the first and most probable result or prefered country only
#'     if (length(x$results) > 1) {
#'       country_pref <- data.frame(
#'         is_country = as.integer(as.character(lapply(
#'           x$results, function(x) grep(paste(",",country), x)))))
#'       country_pref$row <- seq_len(nrow(country_pref))
#'       country_pref <- country_pref[!is.na(country_pref$is_country),]
#'       which_list <- min(country_pref$row)
#'       if (nrow(country_pref) == 0) which_list <- 1
#'     } else {
#'       which_list <- 1
#'     }
#'
#'     # Address_components (varies a lot depending on results)
#'     values <- data.frame(list.cbind(x$results[[which_list]]$address_components))[1,]
#'     names <- unlist(do.call(rbind, list.cbind(x$results[[which_list]]$address_components)[3,])[,1])
#'     if (length(colnames(values)) > 1) colnames(values) <- names
#'
#'     out <- data.frame(
#'       status = x$status,
#'       lon = x$results[[which_list]]$geometry$location$lng,
#'       lat = x$results[[which_list]]$geometry$location$lat,
#'       accuracy = x$results[[which_list]]$types[[1]],
#'       address_type = paste(x$results[[which_list]]$types, collapse = ','),
#'       address_formatted = x$results[[which_list]]$formatted_address,
#'       neighborhood = ifelse("neighborhood" %in% colnames(values),
#'                             toupper(as.character(values$neighborhood)), NA),
#'       postal_code = ifelse("postal_code" %in% colnames(values),
#'                            as.integer(as.character(values$postal_code)), NA),
#'       political = ifelse("political" %in% colnames(values),
#'                          toupper(as.character(values$political)), NA),
#'       country = ifelse("country" %in% colnames(values),
#'                        toupper(as.character(values$country)), NA))
#'     return(out)
#'   }
#'
#'   output <- NULL
#'
#'   for (i in seq_along(address)) {
#'     message(paste("Working on address", i, "of", length(address)))
#'     result <- getGeoDetails(address[i])
#'     if (length(result) > 1) {
#'       result$index <- i
#'       result$search <- address[i]
#'       if (!is.na(index)) result$id <- index[i]
#'       output <- rbind(output, result)
#'       message(paste("Found:", result$address_formatted))
#'     }
#'     done <- output
#'   }
#'   return(done)
#' }
#'
#'
#' ####################################################################
#' #' Get Colombia's Stratum given the Coordinates
#' #'
#' #' This function lets the user obtain Colombia's stratum given the coordinates of an address
#' #'
#' #' @family Geographical
#' #' @family Scrapper
#' #' @param lon Numeric Vector. Longitudes
#' #' @param lat Numeric Vector. Latitudes
#' #' @param label Character Vector. If you wish to keep an id on each address, set this values
#' #' @export
#' geoStratum <- function(lon, lat, label = NA) {
#'   # Manzanas: http://serviciosgis.catastrobogota.gov.co/arcgis/rest/services/social/estrato/MapServer?f=jsapi
#'   url <- URLencode(paste0(
#'     "http://serviciosgis.catastrobogota.gov.co/arcgis/rest/services/social/estrato/MapServer/0/query?",
#'     "where=1%3D1&text=&objectIds=&time=&distance=15&",
#'     "geometry=%7B%22x%22%3A+", lon, "%2C+%22y%22%3A+", lat,
#'     "%7D&geometryType=esriGeometryPoint&inSR=4686&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=ESTRATO&",
#'     "returnGeometry=false&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&",
#'     "returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&",
#'     "gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&",
#'     "returnExtentsOnly=false&datumTransformation=&parameterValues=&rangeValues=&f=pjson"))
#'   x <- fromJSON(url, simplifyVector = FALSE)
#'   if (length(x$features) > 0) {
#'     out <- data.frame(lon = lon, lat = lat, stratum = x$features[[1]]$attributes$ESTRATO)
#'     if (!is.na(label)) out$label <- label
#'     return(out)
#'   } else {
#'     message("Stratum not found for those coordinates!")
#'   }
#' }
#'
#'
#' ####################################################################
#' #' Check, Cross, and Plot Coordinates with Polygons
#' #'
#' #' This function checks a series of coordinates and return a join
#' #' with the information of each coordinate and its respective grid.
#' #' Note that the coords and shapes coordinates MUST have the same
#' #' lon/lat reference system for it to work succesfully.
#' #'
#' #' @family Geographical
#' #' @param coords Dataframe. Dataframe containing at least langitud
#' #' and latitud data
#' #' @param map SpatialPolygonsDataFrame or .shp directory
#' #' @param fix_coords Boolean. Transform and fix coordinates system?
#' #' @param plot Boolean. Return plot with coordinates inside the grid?
#' #' @param all Boolean. Include all coordinates in plot, i.e. only the
#' #' ones who are inside the grids?
#' #' @param alpha Numeric. Points transparency for the plot
#' #' @export
#' geoGrid <- function(coords, map, fix_coords = FALSE, plot = FALSE, all = FALSE, alpha = 0.3) {
#'
#'   try_require("methods")
#'   try_require("rgdal")
#'   try_require("sp")
#'
#'   if (!class(map)[1] == "SpatialPolygonsDataFrame") {
#'     message("Importing shapefile...")
#'     map <- readOGR(dsn = file.path(map))
#'   }
#'
#'   cols <- colnames(coords)
#'   if (sum(grepl("lon|lat", tolower(cols))) != 2) {
#'     stop("Your coords dataframe must contain longitude and latitude!")
#'   }
#'   cols[grep("lon",tolower(cols))] <- "longitude"
#'   cols[grep("lat",tolower(cols))] <- "latitude"
#'   colnames(coords) <- cols
#'   coordinates(coords) <- c("longitude", "latitude")
#'
#'   if (fix_coords) map <- spTransform(map, CRS("+proj=longlat +datum=WGS84"))
#'
#'   shapes_sample <- head(map@polygons[[2]]@Polygons[[1]]@coords)
#'   proj4string <- "+proj=utm +units=mm"
#'   project(shapes_sample, proj4string)
#'
#'   # The coords and shapes coordinates MUST have the same lon/lat reference system
#'   proj4string(coords) <- proj4string(map)
#'   inside.park <- !is.na(over(coords, as(map, "SpatialPolygons")))
#'   #inside.park <- !is.na(over(coords, as.SpatialPolygons.PolygonsList(map)))
#'
#'   # What fraction of coords are inside a shape?
#'   frac <- round(100*mean(inside.park), 2)
#'   fracmsg <- paste0("Fraction of coords inside the grid: ", frac,"%")
#'   message(fracmsg)
#'   cross <- over(coords, map)
#'   crossed <- which(!is.na(cross[,1]))
#'
#'   # Join data and return results
#'   results <- data.frame(coords, cross)
#'
#'   # Plot map and coordinates
#'   if (plot) {
#'     if (!all) {
#'       toplot <- results[crossed,]
#'     } else {
#'       toplot <- results
#'     }
#'     plot <- ggplot() +
#'       geom_point(data = toplot, aes(x = .data$longitude, y = .data$latitude),
#'                  colour = "deepskyblue2", alpha = alpha) +
#'       geom_polygon(data = map, aes(x = .data$long, y = .data$lat, group = .data$group),
#'                    colour = "black", fill = "white", alpha = 0)  +
#'       labs(title = "Coordinates & Grid",
#'            subtitle = fracmsg,
#'            x = "Longitude", y = "Latitude") +
#'       theme_bw()
#'     results <- list(results = results, plot = plot)
#'   }
#'   return(results)
#' }
#'
#'
#' ####################################################################
#' #' Plot Map or Shapefile
#' #'
#' #' This function returns a simple plot from a SpatialPolygonsDataFrame
#' #' imported or a .shp file
#' #'
#' #' @family Geographical
#' #' @param map SpatialPolygonsDataFrame or .shp directory
#' #' @param fix_coords Boolean. Transform and fix coordinates system?
#' #' @param title Character. Title for the plot
#' #' @param subtitle Character. Subtitle for the plot
#' #' @export
#' geoMap <- function(map, fix_coords = FALSE, title = NA, subtitle = NA) {
#'
#'   try_require("rgdal")
#'   try_require("sp")
#'
#'   if (!class(map)[1] == "SpatialPolygonsDataFrame") {
#'     message("Importing shapefile...")
#'     map <- readOGR(dsn = file.path(map))
#'   }
#'   if (fix_coords) {
#'     message("Fixing coordinates format...")
#'     map <- spTransform(map, CRS("+proj=longlat +datum=WGS84"))
#'   }
#'   plot <- ggplot() +
#'     geom_polygon(data = map, aes(x = .data$long, y = .data$lat, group = .data$group),
#'                  colour = "black", fill = "white", alpha = 0.1) +
#'     labs(x = "Latitude", y = "Longitude") +
#'     theme_bw()
#'
#'   if (!is.na(title)) plot <- plot + labs(title = title)
#'   if (!is.na(subtitle)) plot <- plot + labs(subtitle = subtitle)
#'
#'   return(plot)
#' }
#'
#'
#' ####################################################################
#' #' Convert from degrees to numeric coordinates [Deprecated]
#' #'
#' #' This function converts degrees (DMS) coordinates into numerical.
#' #' Note that the sign (S or W) should be assigned manually if needed.
#' #'
#' #' @family Calculus
#' #' @param coord Character vector. Coordinate in format c("DD MM SS")
#' #' @param sep Character. Separator
#' #' @examples
#' #' deg2num("11 12 12")
#' #' deg2num("00-00-00", sep = "-")
#' #' @export
#' deg2num <- function(coord, sep = " ") {
#'   z <- str_split_fixed(as.character(coord), sep, 3)
#'   z <- as.numeric(as.character(unlist(z)))
#'   num <- z[1] + z[2]/60 + z[3]/3600
#'   return(num)
#' }
