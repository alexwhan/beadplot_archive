#' Make main qtl plot
#'
#' @param obj An object of class 'cross', 'map', 'mpcross', 'tidy_gen_map'
#' @param qtl_summary An object of class mpwgaim.summary or a data.frame
#'
#' @return a ggplot object
#' @export
make_main_qtl_plot <- function(obj, qtl_summary) {
  obj_coords <- genomap::get_map_coords(obj)
  main_qtl <- main_qtl_data(qtl_summary)
  
  #Get QTL offsets
  main_qtl <- dplyr::inner_join(main_qtl, obj_coords)
  main_qtl$qtl_centre_offset <- main_qtl$qtl_centre + main_qtl$lg_start
  
  #Make base main plot
  base_main <- make_main_plot(obj) +
    ggplot2::geom_point(data = main_qtl, ggplot2::aes_string(x = "qtl_centre_offset"))
}

#' Make founder qtl plot
#'
#' @param obj An object of class 'cross', 'map', 'mpcross', 'tidy_gen_map'
#' @param qtl_summary An object of class mpwgaim.summary or a data.frame
#'
#' @return a ggplot object
#' @export
make_founder_qtl_plot <- function(obj, qtl_summary) {
  obj_coords <- genomap::get_map_coords(obj)
  founder_qtl <- founder_qtl_data(qtl_summary)
  
  #Get QTL offsets
  founder_qtl <- dplyr::inner_join(founder_qtl, obj_coords)
  founder_qtl$qtl_centre_offset <- founder_qtl$qtl_centre + founder_qtl$lg_start
  
  #Make base main plot
  base_founder <- make_founder_plot(obj) +
    ggplot2::geom_point(data = founder_qtl, ggplot2::aes_string(x = "qtl_centre_offset"))
}

