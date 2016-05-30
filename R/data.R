#' madr example data
#'
#' A dataset containing a MADproject object called 'pumping'
#'
#' @format A MADproject object with an observed time series, two samples,
#' and 100 realizations per sample.
#' \describe{
#'   \item{@observations}{a time series of drawdown, in meters}
#'   \item{@numSamples}{the value 2, [unitless]}
#'   \item{@realizations}{a list of length 2 with matrices
#'     containing an ensemble of 100 simulated drawdown time series,
#'     in meters}
#'   ...
#' }
#' @source
"pumping"
