#' A 4way mpcross object with qtl
#' 
#' A cross produced by mpMap, with a qtl model
#' 
#' @format A list of class 'mpcross' produced by the qtl package
#' \describe{
#' \item{founders}{A list of the genotyping data of the founders}
#' \item{finals}{A list of the genotyping data of the finals}
#' ...
#' }
"m4_cross_qtl"

#' A summary.mpwgaim object
#' 
#' Summary of qtl output from mpwgaim of m4_int_qtl
#' 
#' @format A list of class 'summary.mpwgaim' produced by the mpwgaim package
#' \describe{
#' \item{summary}{A data.frame of qtl output}
#' \item{LOGPtrace}{A list describing the LOGP trace of qtl detection}
#' ...
#' }
"m4_summary"