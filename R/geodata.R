####################################################################
#' Get Google's Geodata given the Addresses
#' 
#' This function lets the user obtain Google's Geodata on given addresses
#' 
#' @param address Character Vector. Addresses you wish to query
#' @param country Character. Default Country if known
#' @param index Character Vector. If you wish to keep an id on each address, set this values
#' @param creds Character. Credential's user (see get_credentials)
#' @export
geodataAddress <- function(address, country = "Colombia", index = NA, creds = NA) {
  
  getGeoDetails <- function(address){   
    
    # require(RJSONIO)
    # require(rlist)
    
    options(warn=-1)
    
    c <- lares::get_credentials(from = "google_api", dir = creds)
    
    # API Documentation: https://developers.google.com/maps/documentation/geocoding
    url <- "https://maps.google.com/maps/api/geocode/json?address="
    api <- c$api_01 # Free Personal API: api_02
    ctry <- paste0("&components=country:", country) # Not working for some reason
    url <- URLencode(paste(url, address, ctry, "&key=", api, sep = ""))
    x <- fromJSON(url, simplifyVector = FALSE)
    
    # Return Na's if we didn't get a match:
    if (x$status != "OK"){
      return(message(paste(x$status, x$error_message)))
    }   
    # Else, extract what we need from the Google server reply into a dataframe:
    
    # Note: We bring the first and most probable result only or prefered country
    if (length(x$results) > 1) {
      country_pref <- data.frame(
        is_country = as.integer(as.character(lapply(x$results, function(x) grep(paste(",",country), x)))))
      country_pref$row <- 1:nrow(country_pref)
      country_pref <- country_pref[!is.na(country_pref$is_country),]
      which_list <- min(country_pref$row)
      if (nrow(country_pref) == 0) {
        which_list <- 1
      }
    } else {
      which_list <- 1
    }
    
    # Address_components (varies a lot depending on results)
    values <- data.frame(rlist::list.cbind(x$results[[which_list]]$address_components))[1,]
    names <- unlist(do.call(rbind, rlist::list.cbind(x$results[[which_list]]$address_components)[3,])[,1])
    if (length(colnames(values)) > 1) {
      colnames(values) <- names
    }
    
    out <- data.frame(
      status = x$status,
      lon = x$results[[which_list]]$geometry$location$lng,
      lat = x$results[[which_list]]$geometry$location$lat,
      accuracy = x$results[[which_list]]$types[[1]],
      address_type = paste(x$results[[which_list]]$types, collapse = ','),
      address_formatted = x$results[[which_list]]$formatted_address,
      neighborhood = ifelse("neighborhood" %in% colnames(values), 
                            toupper(as.character(values$neighborhood)), NA),
      postal_code = ifelse("postal_code" %in% colnames(values), 
                           as.integer(as.character(values$postal_code)), NA),
      political = ifelse("political" %in% colnames(values), 
                         toupper(as.character(values$political)), NA),
      country = ifelse("country" %in% colnames(values), 
                       toupper(as.character(values$country)), NA))
    return(out)
  }
  
  output <- c()
  
  for (i in 1:length(address)) {
    message(paste("Working on address", i, "of", length(address)))
    result <- getGeoDetails(address[i])
    if (length(result) > 1) {
      result$index <- i
      result$search <- address[i]
      if (!is.na(index)) {
        result$id <- index[i]
      }
      output <- rbind(output, result)
      message(paste("Found:", result$address_formatted))
    }
    done <- output
  }
  return(done)
}


####################################################################
#' Get Colombia's Stratum given the Coordinates
#' 
#' This function lets the user obtain Colombia's stratum given the coordinates of an address
#' 
#' @param lon Numeric Vector. Longitudes
#' @param lat Numeric Vector. Latitudes
#' @param label Character Vector. If you wish to keep an id on each address, set this values
#' @export
geoStratum <- function(lon, lat, label = NA) {
  # Manzanas: http://serviciosgis.catastrobogota.gov.co/arcgis/rest/services/social/estrato/MapServer?f=jsapi
  url <- URLencode(paste0(
    "http://serviciosgis.catastrobogota.gov.co/arcgis/rest/services/social/estrato/MapServer/0/query?",
    "where=1%3D1&text=&objectIds=&time=&distance=15&",
    "geometry=%7B%22x%22%3A+", lon, "%2C+%22y%22%3A+", lat,
    "%7D&geometryType=esriGeometryPoint&inSR=4686&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=ESTRATO&",
    "returnGeometry=false&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&",
    "returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&",
    "gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&",
    "returnExtentsOnly=false&datumTransformation=&parameterValues=&rangeValues=&f=pjson"))
  x <- fromJSON(url, simplifyVector = FALSE)
  if (length(x$features) > 0) {
    out <- data.frame(lon = lon, lat=lat, stratum = x$features[[1]]$attributes$ESTRATO)
    if (!is.na(label)) {
      out$label <- label
    }
    return(out)
  } else {
    message("Stratum not found for those coordinates!")
  }
}


####################################################################
#' Check, Cross, and Plot Coordinates with Polygons
#' 
#' This function checks a series of coordinates and return a join
#' with the information of each coordinate and its respective grid.
#' Note that the coords and shapes coordinates MUST have the same 
#' lon/lat reference system for it to work succesfully.
#' 
#' @param coords Dataframe. Dataframe containing at least langitud 
#' and latitud data
#' @param shapes SpatialPolygonsDataFrame. 
#' @param transform Boolean. Transform and fix coordinates system?
#' @param plot Boolean. Return plot with coordinates inside the grid?
#' @export
geoGrid <- function(coords, shapes, transform = FALSE, plot = FALSE) {
  cols <- colnames(coords)
  if (sum(grepl("lon|lat", cols)) != 2) {
    stop("Your coords dataframe must contain longitude and latitude!")
  }
  cols[grep("lon",cols)] <- "longitude"
  cols[grep("lat",cols)] <- "latitude"
  colnames(coords) <- cols
  coordinates(coords) <- c("longitude", "latitude")  
  
  if (transform) {
    shapes <- spTransform(shapes, CRS("+proj=longlat +datum=WGS84"))
  }
  
  coords_sample <- head(coordinates(coords))
  shapes_sample <- head(shapes@polygons[[2]]@Polygons[[1]]@coords)
  
  proj4string <- "+proj=utm +units=mm"
  project(shapes_sample, proj4string)
  
  # The coords and shapes coordinates MUST have the same lon/lat reference system
  proj4string(coords) <- proj4string(shapes)
  inside.park <- !is.na(over(coords, as(shapes, "SpatialPolygons")))
  
  # What fraction of coords are inside a shape?
  frac <- round(100*mean(inside.park), 2)
  fracmsg <- paste0("Fraction of coords inside the grid: ", frac,"%")
  message(fracmsg)
  cross <- over(coords, shapes)
  crossed <- which(!is.na(cross[,1]))
  
  # Join data and return results
  results <- data.frame(coords, cross)
  
  # Plot map and coordinates
  if (plot) {
    plot <- ggplot() + 
      geom_point(data = results[crossed,], aes(x=longitude, y=latitude),
                 colour="deepskyblue2", alpha=0.3) +
      geom_polygon(data = shapes, aes(x=long, y=lat, group=group), 
                   colour="black", fill="white", alpha=0)  +
      labs(title = "Coordinates & Grid",
           subtitle = fracmsg,
           x = "Longitude", y = "Latitude") +
      theme_bw()
    results <- list(results = results, plot = plot)
  }
  return(results)
}
