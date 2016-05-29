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
#' @return likes A list of matching length as \code{data} the likelihood values 
#'   for each sample based on \code{data}.
#'    
setGeneric("calcLikelihood", function(proj, data) {
  standardGeneric("calcLikelihood")
})

setMethod("calcLikelihood", 
          signature(proj="MADproject", data="list"), 
          function(proj, data) {
            proj@likelihoods <- vector("list", length(data))
            for (i in 1:length(data)){
              message("I'll do that likelihood for Meas 1!")
              proj@likelihoods[[i]] <- vector("numeric", length(proj@numSamples))
              names(proj@likelihoods)[i] <- paste0(names(data)[i],data[[i]])
              if(names(data)[i]=="timesteps"){
                message("I'm using timesteps!")
                for(sample in 1:proj@numSamples){
                  proj@likelihoods[[i]][sample] <- np::npudens(tdat=proj@realizations[[sample]][,data[[i]]], #assuming Meas1
                                               edat=t(as.matrix(proj@observations[data[[i]],1])))$dens  #assuming Meas1
                }
              } else {
                message("I don't know what I'm doing!")
              }    
            }
            return(proj)
          }
)