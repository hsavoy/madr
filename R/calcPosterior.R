#' @include MADproject.R
NULL

#' Calculate the posterior for a MADproject object.
#'
#' \code{calcPosterior} returns the posterior values ...
#'
#' @param proj The MADproject object with data read from the MAD# databases.
#' @param likes The likelihood values.
#' @return NULL.
#'
#' @export
setGeneric("calcPosterior", function(proj, likes) {
  standardGeneric("calcPosterior")
})

setMethod("calcPosterior",
          signature(proj="MADproject"),
          function(proj) {
            # proj@posteriors <- vector("list", proj@numTheta + proj@numAnchors) #length(proj@likelihoods))
            # for (theta in 1:length(proj@posteriors)){
            #   ##assuming uniform discrete prior
            #   #proj@posteriors[[theta]] <- proj@likelihoods[[i]]/sum(proj@likelihoods[[i]])
            #   ##weighting priors
            #   dens <- npudens(tdat=proj@priors[proj@likelihoods$sid,theta],
            #           edat=proj@priors[proj@likelihoods$sid,theta])$dens
            #   proj@posteriors[[theta]] <- dens*proj@likelihoods$like #[[1]]
            #   proj@posteriors[[theta]] <- proj@posteriors[[theta]]/sum(proj@posteriors[[theta]])
            # }
            postdata <- merge(#expand.grid(sid=proj@likelihoods$sid,
                              #             tid=1:(proj@numTheta + proj@numAnchors)),
                              proj@priors,
                              proj@likelihoods)
            postdata <- dplyr::mutate(postdata, prod=priordens*like)
            products <- dplyr::summarise(group_by(postdata, tid), ptotal=sum(prod))
            postdata <- dplyr::mutate(merge(postdata,products), post=prod/ptotal)
            proj@posteriors <- subset(postdata,select=c(sid,tid,name,priorvalue,post))
            return(proj)
          }
)
