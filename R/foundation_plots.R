#' Makes a linear ggplot object based on genetic map
#'
#' @param obj An object of class 'cross', 'map', 'mpcross', 'tidy_gen_map'
#'
#' @return A ggplot object
#' @export
#'
make_main_plot <- function(obj) {
  map_df <- get_long_coords(obj)
  gg <- ggplot2::ggplot(map_df, ggplot2::aes_string("mapdist", 1)) +
    ggplot2::geom_line(ggplot2::aes_string(colour = "lg"))
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
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' make_founder_plot(m4_cross_qtl)
make_founder_plot <- function(obj) {
  map_df <- get_long_coords(obj)
  nfounders <- nfounders(obj)
  map_df <- purrr::map_df(seq_along(1:nfounders), 
                          ~ dplyr::mutate_(map_df, "founder" = .x))
  gg <- ggplot2::ggplot(map_df, ggplot2::aes_string("mapdist", 1)) +
    ggplot2::geom_line(ggplot2::aes_string(colour = "lg")) +
    ggplot2::facet_grid(founder ~ .)
}
