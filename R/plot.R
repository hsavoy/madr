#' @include MADproject.R
NULL

#' Plot the MADproject object
#'
#' \code{plot.MADproject} plots ...
#'
#' @param x The MADproject object
#' @param y not supported
#' @param ... not supported
#' @return NULL.
#'

setGeneric("plot")  #necessary?

setMethod("plot",
          signature(x="MADproject"),
          function(x) {
              message("I'll do my own plots!")
          }
)
