#' Checks if any non-numeric elements are present
#' 
#' @param x A vector to be inspected.
#' @return TRUE or FALSE
#' @examples
#' vec1 <- 1:10
#' vec2 <- c(vec1, "A")
#' nonNumeric(vec1)
#' nonNumeric(vec2)
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
    if(nonNumeric(data[, var])) data[, var] <- as.numeric(data[, var])
  }
  return(data)
}
