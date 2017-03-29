#' Tidy an mpwgaim summary 
#'
#' @param obj An object of class mpwgaim.summary or a data.frame
#'
#' @return A tibble
#' @export
mpwgaim_summary_tidy <- function(obj) {
  UseMethod("mpwgaim_summary_tidy")
}

#' @export
mpwgaim_summary_tidy.summary.wgaim <- function(obj) {
  mpwgaim_tbl <- obj$summary
  mpwgaim_summary_tidy(mpwgaim_tbl)
}

#' @export
mpwgaim_summary_tidy.data.frame <- function(obj) {
  mpwgaim_names <- c("Chromosome", "Left Marker", "dist (cM)",
                     "Right Marker", "dist (cM)", "Founder", "Size",
                     "Founder Prob", "Founder LOGP", "Prob", "% var",
                     "LOGP")
  new_mpwgaim_names <- c("lg", "left_marker", "left_dist", "right_marker", "right_dist",
                         "founder", "founder_cont", "founder_prob", "founder_logp",
                         "qtl_prob", "qtl_perc_var", "qtl_logp")
  if(!all(names() == mpwgaim_names)) stop("data.frame names do not match expected values")
  
  nfounders <- length(unique(obj$founder))
  
  if(!log2(nfounders) %in% 1:3) stop("The number of founders doesn't look right, there should\n
                                     be 2, 4 or 8")
  
  names(obj) <- new_mpwgaim_names
  
  obj <- as_data_frame(apply(obj, 2, function(x) {
    x[x == ""] <- NA
    return(na.locf(x))
  }))
  
  obj <- autoNumeric(obj)
  
  obj$qtl <- rep(1:nrow(obj) / nfounders, each = nfounders)
  
  obj$qtl_centre <- (obj$left_dist + obj$right_dist) / 2
  
  return(obj)
}