#' madr example data
#'
#' A dataset containing a MADproject object called 'pumping'
#'
#' @format A MADproject object with an observed time series, four samples,
#' and realizations for each sample.
#' \describe{
#'   \item{@observations}{a time series of drawdown, in meters}
#'   \item{@numSamples}{the value 4, [unitless]}
#'   \item{@realizations}{a list of length 4 with matrices
#'     containing an ensemble of 100 simulated drawdown time series,
#'     in meters}
#'   ...
#' }
#' @source
"pumping"
