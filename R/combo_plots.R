#' Make main qtl plot
#'
#' @param obj An object of class 'cross', 'map', 'mpcross', 'tidy_gen_map'
#' @param qtl_summary An object of class mpwgaim.summary or a data.frame
#' @param main_bead_size Character - name of the variable to map bead size to (default = qtl_perc_var)
#' @param main_bead_col Character - Name of the variable to map bead colour to (default = qtl_logp)
#' @param founder_bead_size Character - name of the variable mapped to bead size
#' @param founder_bead_col Character - name of the variable mapped to bead colour
#'
#' @return a ggplot object
#' @export


combo_plot <- function(obj, qtl_summary, main_bead_size, main_bead_col,
                       founder_bead_size, founder_bead_col) {
  main <- make_main_qtl_plot(obj, qtl_summary)
  founder <- make_founder_qtl_plot(obj, qtl_summary)
  nf <- nfounders(obj)
  combo_plot <- gridExtra::arrangeGrob(main + ggplot2::theme(legend.position = "bottom"), 
                                       founder + ggplot2::theme(legend.position = "bottom"), 
                                       ncol = 1, heights = c(1.7, nf))
}