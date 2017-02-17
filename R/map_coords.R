#' Get linkage group extents
#'
#' @param obj An object of class cross, map or tidy_gen_df
#'
#' @return map coordinates as a vector
#' @export
get_lg_lengths <- function(obj) {
  UseMethod("get_lg_lengths")
}

#' @export
get_lg_lengths.cross <- function(obj) {
  obj <- genomap::map2df(obj)
  get_lg_lengths(obj)
}

#' @export
get_lg_lengths.map <- function(obj) {
  obj <- genomap::map2df(obj)
  get_lg_lengths(obj)
}

#' @export
get_lg_lengths.tidy_gen_map <- function(obj) {
  obj %>% 
    dplyr::group_by_("lg") %>% 
    summarise(max_mapdist = max(mapdist))
}


#' Get offsets for linkage groups
#'
#' @param map A map object
#' @param order A vector of linkage group names to sort map by
#'
#' @return offsets as a vector
#' @export
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