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
#' @importFrom np npudens
#' @importFrom plyr daply
#' @importFrom reshape2 dcast
#'
#' @export
setGeneric("calcLikelihood", function(proj, data, ...) {
  standardGeneric("calcLikelihood")
})

setMethod("calcLikelihood",
          signature(proj="MADproject", data="numeric"),
          function(proj, data, num_realz=NA, samples=1:proj@numSamples) {
            # proj@likelihoods <- vector("list", length(data))
            # for (i in 1:length(data)){
            #   proj@likelihoods[[i]] <- vector("numeric", length(samples))
            #   names(proj@likelihoods)[i] <- paste0(names(data)[i],data[[i]])
            #   if(names(data)[i]=="timesteps"){
            #     for(scount in 1:length(samples)){
            #       if(is.na(num_realz)){
            #         #subset <- 1:(dim(proj@realizations[[samples[scount]]])[1])
            #         unique(subset(proj@realizations,sid==scount)$rid)
            #       }else{
            #         rsubset <- 1:num_realz
            #       }
            #       #proj@likelihoods[[i]][scount] <- npudens(tdat=proj@realizations[[samples[scount]]][subset,data[[i]]],
            #       #                             edat=t(as.matrix(proj@observations[data[[i]]])))$dens
            #       realz <- dcast(subset(proj@realizations,sid==scount),rid~zid,sum)[,-1]
            #       proj@likelihoods[[1]][scount] <- npudens(tdat=realz[rsubset,data[[i]]],
            #                                                edat=as.data.frame(t(proj@observations)))$dens
            #     }
            #   } else {
            #     message("Unknown inversion data type. See ?calcLikelihood")
            #   }
            # }
            # return(proj)
            use <- subset(proj@realizations, sid %in% samples & rid <= num_realz & zid %in% data)
            cl <- parallel::makeCluster(4)
            doParallel::registerDoParallel(cl)
            suppressWarnings(proj@likelihoods <- data.frame(sid=unique(use$sid),
                                                            like=daply(use, .(sid), npLike, obs=proj@observations[data],
                                                                       .parallel = TRUE, .paropts=list(.packages=c("np","reshape2"),
                                                                                                       .export=c("proj")
                                                                       )
                                                            )
            ))
            stopCluster(cl)
            return(proj)
          }
)

setMethod("calcLikelihood",
          signature(proj="MADproject"),
          function(proj, num_realz=max(proj@realizations$rid), samples=1:proj@numSamples) {
            #proj@likelihoods <- vector("list", 1)
            #proj@likelihoods[[1]] <- vector("numeric", length(samples))
            # for(scount in 1:length(samples)){
            #   if(is.na(num_realz)){
            #     #subset <- 1:(dim(proj@realizations[[samples[scount]]])[1])
            #     rsubset <- unique(subset(proj@realizations,sid==scount)$rid)
            #   }else{
            #     rsubset <- 1:num_realz
            #   }
            #   #proj@likelihoods[[1]][scount] <- npudens(tdat=proj@realizations[[samples[scount]]][subset,],
            #   #                                             edat=t(as.matrix(proj@observations)))$dens
            #
            #
            # }
            use <- subset(proj@realizations, sid %in% samples & rid <= num_realz)
            cl <- parallel::makeCluster(4)
            doParallel::registerDoParallel(cl)
            suppressWarnings(proj@likelihoods <- data.frame(sid=unique(use$sid),
                                           like=daply(use, .(sid), npLike, obs=proj@observations,
                                      .parallel = TRUE, .paropts=list(.packages=c("np","reshape2"),
                                                                      .export=c("proj")
                                                                      )
                                      )
            ))
            stopCluster(cl)
            return(proj)
          }
)

npLike <- function(realz, obs){
  return(npudens(tdat=dcast(realz,rid~zid,sum)[,-1],
                 edat=as.data.frame(t(obs)))$dens)
}
