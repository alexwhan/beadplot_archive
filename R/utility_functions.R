#' Checks if any non-numeric elements are present
#' 
#' @param x A vector to be inspected.
#' @return TRUE or FALSE
nonNumeric <- function(x) {
  suppressWarnings(any(is.na(as.numeric(x[!is.na(x)]))))
}

#' Identifies factor variables in a data.frame and converts them to character
#' 
#' @param data A data.frame.
#' @param ignore.var Any variables to ignore. Default is NULL.
#' @return The updated data.frame.
deFactorise <- function(data, ignore.var = NULL) {
  facVar <- names(sapply(data, class)[which(sapply(data, class) == "factor")])
  for(var in facVar[!facVar %in% ignore.var]) {
    data[, var] <- as.character(data[, var])
  }
  return(data)
}

#' Converts character data.frame variables to numeric if it will not coerce to NA
#' 
#' @param data A data.frame.
#' @param ignore.var Any variables to ignore. Default is NULL.
#' @return The updated data.frame.
autoNumeric <- function(data, ignore.var = NULL) {
  charVar <- names(sapply(data, class)[which(sapply(data, class) == "character")])
  for(var in charVar[!charVar %in% ignore.var]) {
    if(!nonNumeric(data[[var]])) data[[var]] <- as.numeric(data[[var]])
  }
  return(data)
}

#' A stripped down version of zoo::na.locf()
#'
#' @param object 
#' @return
na.locf <- function(object) {
  
  na.locf.0 <- function(x) {
    L <- !is.na(x)
    idx <- c(NA,which(L))[cumsum(L)+1]
    na.index <- function(x, i) {
      L <- !is.na(i)
      x[!L] <- NA
      x[L] <- x[i[L]]
      x
    }
    xf <- na.index(x, idx)
    naruns <- rle(is.na(x))
    naok <- inverse.rle(naruns)
    ifelse(naok, xf, x)
  }
  object[] <- if (length(dim(object)) == 0)
    na.locf.0(object)
  else
    apply(object, length(dim(object)), na.locf.0)
  return(object)
}
