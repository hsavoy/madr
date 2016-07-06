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
            proj@posteriors <- vector("list", proj@numTheta + proj@numAnchors) #length(proj@likelihoods))
            for (theta in 1:length(proj@posteriors)){
              ##assuming uniform discrete prior
              #proj@posteriors[[theta]] <- proj@likelihoods[[i]]/sum(proj@likelihoods[[i]])
              ##weighting priors
              dens <- npudens(tdat=proj@priors[,theta],
                      edat=proj@priors[,theta])$dens
              proj@posteriors[[theta]] <- cbind(cbind(proj@priors[,theta],
                                                      dens*proj@likelihoods[[1]]))
              proj@posteriors[[theta]] <- proj@posteriors[[theta]]/sum(proj@posteriors[[theta]])
            }
            return(proj)
          }
)
