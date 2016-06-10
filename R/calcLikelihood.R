#' @include MADproject.R
NULL

#' Calculate the likelihood for a MADproject object.
#'
#' \code{calcLikelihood} returns the likelihood values based on the
#' observation and realization data in MADproject and the requested
#' inversion data type \code{data}.
#'
#' @param proj The MADproject object with data read from the MAD# databases.
#' @param data The kind of data to use for the likelihood calculations.
#' @return proj The updated MADproject object with a list of matching length
#'   as \code{data} the likelihood values
#'   for each sample based on \code{data}.
#'
#' @export
setGeneric("calcLikelihood", function(proj, data) {
  standardGeneric("calcLikelihood")
})

setMethod("calcLikelihood",
          signature(proj="MADproject", data="list"),
          function(proj, data) {
            proj@likelihoods <- vector("list", length(data))
            for (i in 1:length(data)){
              proj@likelihoods[[i]] <- vector("numeric", length(proj@numSamples))
              names(proj@likelihoods)[i] <- paste0(names(data)[i],data[[i]])
              if(names(data)[i]=="timesteps"){
                for(sample in 1:proj@numSamples){
                  proj@likelihoods[[i]][sample] <- np::npudens(tdat=proj@realizations[[sample]][,data[[i]]], #assuming Meas1
                                               edat=t(as.matrix(proj@observations[data[[i]],1])))$dens  #assuming Meas1
                }
              } else {
                message("Unknown inversion data type. See ?calcLikelihood")
              }
            }
            return(proj)
          }
)

setMethod("calcLikelihood",
          signature(proj="MADproject"),
          function(proj) {
            proj@likelihoods <- vector("list", 1)
            proj@likelihoods[[1]] <- vector("numeric", length(proj@numSamples))
            for(sample in 1:proj@numSamples){
              proj@likelihoods[[1]][sample] <- np::npudens(tdat=proj@realizations[[sample]], #assuming Meas1
                                                           edat=t(as.matrix(proj@observations[,1])))$dens  #assuming Meas1
            }
            return(proj)
          }
)
