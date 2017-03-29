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
mpwgaim_summary_tidy.summary.mpwgaim <- function(obj) {
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
  if(!all(names(obj) == mpwgaim_names)) stop("data.frame names do not match expected values")
  
  names(obj) <- new_mpwgaim_names
  
  nfounders <- length(unique(obj$founder))
  
if(!log2(nfounders) %in% 1:3) stop("The number of founders doesn't look right, there should\n
                                     be 2, 4 or 8")
  
  obj <- as_data_frame(apply(obj, 2, function(x) {
    x[x == ""] <- NA
    return(na.locf(x))
  }))
  
  obj <- autoNumeric(obj)
  obj$qtl <- rep(1:(nrow(obj) / nfounders), each = nfounders)
  
  obj$qtl_centre <- (obj$left_dist + obj$right_dist) / 2
  
  return(obj)
}

#' Get base qtl data
#'
#' @param obj An object of class summary.wgaim or data.frame
#'
#' @return A data.frame
#' @export
base_qtl_data <- function(obj) {
  UseMethod("base_qtl_data")
}

#' @export
base_qtl_data.summary.mpwgaim <- function(obj) {
  mpwgaim_tbl <- obj$summary
  base_qtl_data(mpwgaim_tbl)
}

#' @export
base_qtl_data.data.frame <- function(obj) {
  tidy_qtl <- mpwgaim_summary_tidy(obj)
  base_qtl <- dplyr::select_(tidy_qtl, "qtl", "lg", "qtl_centre",
                             "qtl_prob", "qtl_perc_var", "qtl_logp")
  nfounders <- nrow(obj) / length(unique(tidy_qtl$qtl))
  base_qtl <- base_qtl[((1:(nrow(base_qtl) / nfounders)) - 1) * nfounders + 1, ]
  return(base_qtl)
}

#' Get founder qtl data
#'
#' @param obj An object of class summary.wgaim or data.frame
#'
#' @return A data.frame
#' @export
founder_qtl_data <- function(obj) {
  UseMethod("founder_qtl_data")
}

#' @export
founder_qtl_data.summary.mpwgaim <- function(obj) {
  mpwgaim_tbl <- obj$summary
  founder_qtl_data(mpwgaim_tbl)
}

#' @export
founder_qtl_data.data.frame <- function(obj) {
  tidy_qtl <- mpwgaim_summary_tidy(obj)
  founder_qtl <- dplyr::select_(tidy_qtl, "qtl", "lg", "qtl_centre",
                                "founder", "founder_cont", "founder_prob",
                                "founder_logp")
  return(founder_qtl)
}