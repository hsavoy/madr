#' @include MADproject.R 
NULL

#' Plot the MADproject object
#'
#' \code{plot.MADproject} plots ...
#'
#' @param proj The MADproject object
#' @return NULL.
#'    

setGeneric("plot")  #necessary?

setMethod("plot", 
          signature(x="MADproject"), 
          function(x) {
              message("I'll do my own plots!") 
          }
)