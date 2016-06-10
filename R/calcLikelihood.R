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
setGeneric("calcLikelihood", function(proj, data, ...) {
  standardGeneric("calcLikelihood")
})

setMethod("calcLikelihood",
          signature(proj="MADproject", data="list"),
          function(proj, data, num_realz=NA, samples=1:proj@numSamples) {
            proj@likelihoods <- vector("list", length(data))
            for (i in 1:length(data)){
              proj@likelihoods[[i]] <- vector("numeric", length(samples))
              names(proj@likelihoods)[i] <- paste0(names(data)[i],data[[i]])
              if(names(data)[i]=="timesteps"){
                for(scount in 1:length(samples)){
                  if(is.na(num_realz)){
                    subset <- 1:(dim(proj@realizations[[samples[scount]]])[1])
                  }else{
                    subset <- 1:num_realz
                  }
                  proj@likelihoods[[i]][scount] <- np::npudens(tdat=proj@realizations[[samples[scount]]][subset,data[[i]]], #assuming Meas1
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
          function(proj, num_realz=NA, samples=1:proj@numSamples) {
            proj@likelihoods <- vector("list", 1)
            proj@likelihoods[[1]] <- vector("numeric", length(samples))
            for(scount in 1:length(samples)){
              if(is.na(num_realz)){
                subset <- 1:(dim(proj@realizations[[samples[scount]]])[1])
              }else{
                subset <- 1:num_realz
              }
              proj@likelihoods[[1]][scount] <- np::npudens(tdat=proj@realizations[[samples[scount]]][subset,], #assuming Meas1
                                                           edat=t(as.matrix(proj@observations[,1])))$dens  #assuming Meas1
            }
            return(proj)
          }
)
