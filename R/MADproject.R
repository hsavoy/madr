#' @include madr.R
NULL

#' An S4 class to represent a MAD project
#'
#' @slot numLocations A length-one numeric vector for the number of measurement locations
#' @slot numTimesteps A length-one numeric vector for the number of time steps in series
#' @slot numSamples A length-one numeric vector for the number of samples
#' @slot observations A numeric \code{numTimesteps}-by-\code{numLocations} matrix
#'   for the observations at the m
#' @slot realizations A list of length \code{numSamples} with each element containing
#'   a numeric (number of realizations)-by-\code{numLocations*numTimesteps} matrix
#'   to hold the ensemble of simulated measurements.
#'
#' @export
MADproject <- setClass(
  # Set the name for the class
  "MADproject",

  # Define the slots
  slots = c(
    projectname = "character",
    madname = "character",
    resultname = "character",
    xpath = "character",
    numLocations = "numeric",
    numTimesteps = "numeric",
    numSamples   = "numeric",
    numAnchors = "numeric",
    numTheta = "numeric",
    truevalues = "numeric",
    observations = "numeric",
    realizations   = "list",  #length numSamples, if numTimesteps > 1, then matrices inside
    priors = "matrix",   #numSamples by (numTheta + numAnchors)
    likelihoods = "list",  #
    posteriors = "list"
  )

  # Set the default values for the slots. (optional)
#   prototype=list(
#     numLocations = 1,
#     numTimesteps = 1,
#     numSamples   = 2,
#     observations = matrix(0,nrow=numTimesteps,ncol=numLocations),
#     realizations = vector("list", numSamples)
#   ),

  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
#   validity=function(object)
#   {
#     ##ADD file checks
#     if(numLocations<1)  {
#       return("Need at least one Type-B measurement location.")
#     }
#     if(numTimesteps<1) {
#       return("Need at least one time step.")  #Or put in steady-state check
#     }
#     if(numSamples<2)  {
#       return("Need at least two samples.")
#     }
#     return(TRUE)
#   }

)
