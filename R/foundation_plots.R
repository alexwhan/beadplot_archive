#' Makes a linear ggplot object based on genetic map
#'
#' @param obj An object of class 'cross', 'map', 'mpcross', 'tidy_gen_map'
#' @param yvar Variable to map y to (character)
#'
#' @return A ggplot object
#' @export
#'
make_main_plot <- function(obj, yvar = NULL) {
  map_df <- get_long_coords(obj)
  if(is.null(yvar)) {
    gg <- ggplot2::ggplot(map_df, ggplot2::aes_string("mapdist", 1)) +
      ggplot2::geom_line(ggplot2::aes_string())
  } else {
    gg <- ggplot2::ggplot(map_df, ggplot2::aes_string("mapdist", yvar)) +
      ggplot2::geom_line(ggplot2::aes_string())
  }
  return(gg)
}

#' Get long version of map_coords
#'
#' @param obj An object of class 'cross', 'map', 'mpcross', 'tidy_gen_map'
#'
#' @return A data.frame
get_long_coords <- function(obj) {
  map_df <- genomap::get_map_coords(obj)
  map_long <- tidyr::gather_(map_df, "position", "mapdist", 
                             c("lg_start", "lg_end"))
  map_long
}

#' Make a founder plot
#'
#' @param obj An object of class 'cross', 'map', 'mpcross', 'tidy_gen_map'
#' @param yvar Variable to map y to (character)
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' make_founder_plot(m4_cross_qtl)
make_founder_plot <- function(obj, yvar = NULL) {
  map_df <- get_long_coords(obj)
  nf <- nfounders(obj)
  map_df <- purrr::map_df(founder_names(obj), 
                          ~ dplyr::mutate(map_df, founder = .x))
  if(is.null(yvar)) {
    gg <- ggplot2::ggplot(map_df, ggplot2::aes_string("mapdist", 1)) +
      ggplot2::geom_line(ggplot2::aes_string()) +
      ggplot2::facet_grid(founder ~ .)
  } else {
    gg <- ggplot2::ggplot(map_df, ggplot2::aes_string("mapdist", yvar)) +
      ggplot2::geom_line(ggplot2::aes_string()) +
      ggplot2::facet_grid(founder ~ .)
  }
  return(gg)
}
