#' Make main qtl plot
#'
#' @param obj An object of class 'cross', 'map', 'mpcross', 'tidy_gen_map'
#' @param qtl_summary An object of class mpwgaim.summary or a data.frame
#' @param main_bead_size Character - name of the variable to map bead size to (default = qtl_perc_var)
#' @param main_bead_col Character - Name of the variable to map bead colour to (default = qtl_logp)
#'
#' @return a ggplot object
#' @export
make_main_qtl_plot <- function(obj, qtl_summary, main_bead_size = "qtl_perc_var", 
                               main_bead_col = "qtl_logp") {
  obj_coords <- genomap::get_map_coords(obj)
  main_qtl <- main_qtl_data(qtl_summary)
  main_qtl$facet_label <- "Main effects"
  
  #Get QTL offsets
  main_qtl <- dplyr::inner_join(main_qtl, obj_coords)
  main_qtl$qtl_centre_offset <- main_qtl$qtl_centre + main_qtl$lg_start
  
  #Make base main plot
  base_main <- make_main_plot(obj) +
    ggplot2::geom_point(data = main_qtl, 
                        ggplot2::aes_string(x = "qtl_centre_offset",
                                            size = main_bead_size,
                                            colour = main_bead_col)) +
    ggplot2::scale_colour_gradient(low = "white", high = "red") +
    ggplot2::facet_grid(facet_label ~ .)
}

#' Make founder qtl plot
#'
#' @param obj An object of class 'cross', 'map', 'mpcross', 'tidy_gen_map'
#' @param qtl_summary An object of class mpwgaim.summary or a data.frame
#' @param founder_bead_size Character - name of the variable mapped to bead size
#' @param founder_bead_col Character - name of the variable mapped to bead colour
#'
#' @return a ggplot object
#' @export
make_founder_qtl_plot <- function(obj, qtl_summary, founder_bead_size = "founder_logp", 
                                  founder_bead_col = "founder_cont", yvar = NULL) {
  obj_coords <- genomap::get_map_coords(obj)
  founder_qtl <- founder_qtl_data(qtl_summary)
  
  #Get QTL offsets
  founder_qtl <- dplyr::inner_join(founder_qtl, obj_coords)
  founder_qtl$qtl_centre_offset <- founder_qtl$qtl_centre + founder_qtl$lg_start
  
  #Make base main plot
  base_founder <- make_founder_plot(obj) +
    ggplot2::geom_point(data = founder_qtl, 
                        ggplot2::aes_string(x = "qtl_centre_offset",
                                            size = founder_bead_size,
                                            colour = founder_bead_col)) +
    ggplot2::scale_colour_gradient2(low = "green", high = "yellow")
}

