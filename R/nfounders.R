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