#' Get linkage group extents
#'
#' @param map A map object
#'
#' @return map coordinates as a vector
#' @export
#'
#' @examples
get_lg_lengths <- function(map) {
  purrr::map(map, ~ max(.x))
}

#' Get offsets for linkage groups
#'
#' @param map A map object
#' @param order A vector of linkage group names to sort map by
#'
#' @return offsets as a vector
#' @export
#'
#' @examples
get_lg_offsets <- function(map, order = NULL) {
  if(!is.null(order)) {
    stopifnot(class(order) == "character")
    stopifnot(all(order %in% names(map)))
    stopifnot(all(names(map) %in% order))
    map <- map[order]
  }
  offsets <- purrr::map(map, ~ max(.x)) %>% 
    purrr::accumulate(sum)
  return(offsets)
}