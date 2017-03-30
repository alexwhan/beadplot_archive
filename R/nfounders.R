#' Get number of founders of a multi-parental object
#'
#' @param obj A map or cross object
#'
#' @return The number of founders
#' @export
nfounders <- function(obj) {
  UseMethod("nfounders")
}

#' @export
nfounders.mpInterval <- function(obj) {
  return(obj$nfounders)
}

#' @export
nfounders.mpcross <- function(obj) {
  return(nrow(obj$founders))
}

#' @export
nfounders.cross <- function(obj) {
  return(2)
}

#' @export
nfounders.map <- function(obj) {
  return(2)
}

#' Get founder names
#'
#' @param obj An mpcross or mpInterval object
#'
#' @return The founder names (character vector)
#' @export
founder_names <- function(obj) {
  UseMethod("founder_names")
}

#' @export
founder_names.mpInterval <- function(obj) {
  return(obj$founders)
}

#' @export
founder_names.mpcross <- function(obj) {
  return(row.names(obj$founders))
}

#' @export
founder_names.default <- function(obj) {
  return(1:2)
}
