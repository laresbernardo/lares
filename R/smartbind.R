####################################################################
#' Smart rbind
#'
#' Efficient rbind of data frames, even if the column names don't match.
#' Code based on gtools::smartbind
#'
#' @family Data Wrangling
#' @param ... Dataframes. Dataframes to bind
#' @param list List. Containing dataframes to combine
#' @param fill Value. To use when 'filling' missing columns
#' @param sep Character. String used to separate column names when 
#' pasting them together.
#' @export
rbind_full <- function(..., list, fill = NA, sep = ':') {
  
  # https://github.com/cran/gtools/blob/master/R/smartbind.R
  
  data <- base::list(...)
  if (!missing(list)) data <- modifyList(list, data)
  data <- data[!sapply(data, function(l) is.null(l) | (ncol(l) == 0) | (nrow(l) == 0))]
  
  
  defaultNames <- seq.int(length(data))
  
  if (is.null(names(data)))
    names(data) <- defaultNames
  
  emptyNames <- names(data) == ""
  if (any(emptyNames) )
    names(data)[emptyNames] <- defaultNames[emptyNames]
  
  data <- lapply(data,
                 function(x)
                   if (is.matrix(x) || is.data.frame(x))
                     x
                 else
                   data.frame(as.list(x), check.names = FALSE))
  
  #retval <- new.env()
  retval <- base::list()
  rowLens <- unlist(lapply(data, nrow))
  nrows <- sum(rowLens)
  
  rowNameList <- unlist(lapply(names(data),
                                function(x)
                                  if (rowLens[x] <= 1) x
                                else paste(x, seq(1,rowLens[x]),sep = sep)))
  
  colClassList <- vector(mode = "list", length = length(data))
  factorColumnList <- vector(mode = "list", length = length(data))
  factorLevelList <- vector(mode = "list", length = length(data))
  
  
  start <- 1
  blockIndex <- 1
  for (block in data) {
    colClassList[[blockIndex]] <- base::list()
    factorColumnList[[blockIndex]] <- character(length = 0)
    factorLevelList[[blockIndex]] <- base::list()
    
    end <- start + nrow(block) - 1
    for (col in colnames(block)) {
      classVec <- class(block[,col])
      
      ## store class and factor level information for later use
      colClassList[[blockIndex]][[col]] <- classVec
      if ("factor" %in% classVec) {
        
        factorColumnList[[blockIndex]] <-
          c(factorColumnList[[blockIndex]], col)
        
        factorLevelList[[blockIndex]][[col]] <-
          levels(block[,col])
      }
      
      if ("factor" %in% classVec) {
        newclass <- "character"
      } else newclass <- classVec[1]
      
      ## Coerce everything that isn't a native type to character
      if (!(newclass %in% c("logical", "integer", "numeric",
                            "complex", "character", "raw"))) {
        newclass <- "character"
        warning("Converting non-atomic type column '", col,
                "' to type character.")
      }
      
      if (!(col %in% names(retval)))
        retval[[col]] <- as.vector(rep(fill,nrows), mode = newclass)
      
      ## Handle case when current and previous native types differ
      oldclass <- class(retval[[col]])
      
      if (oldclass != newclass) {
        # handle conversions in case of conflicts
        #   numeric vs integer --> numeric
        #   complex vs numeric or integer --> complex
        #   anything else:  --> character
        if (oldclass %in% c("integer", "numeric") && newclass %in% c("integer", "numeric") )
          class(retval[[col]]) <- mode <- "numeric"
        else if (oldclass == "complex" && newclass %in% c("integer", "numeric") )
          class(retval[[col]]) <- mode <- "complex"
        else if (oldclass %in% c("integer", "numeric") && newclass == "complex")
          class(retval[[col]]) <- mode <- "complex"
        else
        {
          class(retval[[col]]) <- mode <- "character"
          warning("Column class mismatch for '", col, "'. ",
                  "Converting column to class 'character'.")
        }
      }
      else
        mode <- oldclass
      
      if (mode == "character")
        vals <- as.character(block[,col])
      else
        vals <- block[,col]
      retval[[col]][start:end] <- as.vector(vals, mode = mode)
    }
    start <- end + 1
    blockIndex <- blockIndex + 1
  }
  
  all.equal.or.null <- function(x,y){
    if (is.null(x) || is.null(y))
      return(TRUE)
    else
      return(all.equal(x,y))
  }
  
  ## Handle factors, merging levels
  for (col in unique(unlist(factorColumnList))) {
    ## Ensure column classes match across blocks
    colClasses <- lapply(colClassList, function(x) x[[col]])
    firstNotNull <- which(!sapply(colClasses, is.null))[1]
    allSameOrNull <- all(sapply(colClasses[-firstNotNull],
                                function(x) isTRUE(all.equal.or.null(colClasses[[firstNotNull]], x))
    ))
    
    if (allSameOrNull) {
      # grab the first *non-NULL* class information
      colClass <- colClasses[[firstNotNull]]
    } else {
      warning("Column class mismatch for '", col, "'. ",
              "Converting column to class 'character'.")
      next()
    }
    
    
    ## check if factor levels are all the same
    colLevels <- lapply(factorLevelList, function(x) x[[col]])
    firstNotNull <- which(!sapply(colLevels, is.null))[1]
    allSameOrNull <- all(sapply(colLevels[-firstNotNull],
                                function(x) isTRUE(all.equal.or.null(colLevels[[firstNotNull]], x))
    ))
    
    
    if (allSameOrNull) {
      if ("ordered" %in% colClass)
        retval[[col]] <- ordered(retval[[col]], levels = colLevels[[firstNotNull]] )
      else
        retval[[col]] <- factor(retval[[col]], levels = colLevels[[firstNotNull]] )
    } else {
      ## Check if longest set of levels is a superset of all others,
      ## and use that one
      longestIndex  <- which.max( sapply(colLevels, length) )
      longestLevels <- colLevels[[longestIndex]]
      allSubset <- all(sapply(colLevels[-longestIndex],
                              function(l) all(l %in% longestLevels)))
      if (allSubset) {
        if ("ordered" %in% colClass)
          retval[[col]] <- ordered(retval[[col]], levels = longestLevels )
        else
          retval[[col]] <- factor(retval[[col]], levels = longestLevels )
      } else {
        # form superset by appending to longest level set
        levelSuperSet <- unique(c(longestLevels, unlist(colLevels)))
        retval[[col]] <- factor(retval[[col]], levels = levelSuperSet )
        
        if (length(colClass) > 1) {
          warning( "column '", col, "' of class ",
                   paste("'", colClass, "'", collapse = ":", sep = "'"),
                   " converted to class 'factor'. Check level ordering." )
        }
        
      }
    }
  }
  attr(retval,"row.names") <- rowNameList
  class(retval) <- "data.frame"
  return(retval)
}

# df1 <- data.frame(A = 1:10, B = LETTERS[1:10], C = rnorm(10) )
# df2 <- data.frame(A = 11:20, D = rnorm(10), E = letters[1:10] )
# rbind_full(df1, df2)
