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
            proj@posteriors <- vector("list", length(proj@likelihoods))
            for (i in 1:length(proj@posteriors)){
              ##assuming uniform discrete prioor
              proj@posteriors[[i]] <- proj@likelihoods[[i]]/sum(proj@likelihoods[[i]])
            }
            return(proj)
          }
)
