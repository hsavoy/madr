#' @include MADproject.R
NULL

#' Test the convergence of a MADproject object.
#'
#' \code{test_convergence} returns a plot to help the user to visualize if
#' there are enough realizations in the project
#'
#' @param proj The MADproject object.
#' @return NULL.
#'
#' @export
setGeneric("test_convergence", function(proj, data, ...) {
  standardGeneric("test_convergence")
})

setMethod("test_convergence",
          signature(proj="MADproject", data="list"),
          function(proj, data, samples=1:proj@numSamples) {
            #Need to store likelihood values for a variety of realizations
            likes <- matrix(NA, ncol=length(samples), nrow=10)  ##assuming 10 diff realz counts
            ### Loop through the variety of realizations
            minr <- min(unlist(lapply(proj@realizations,function(x){dim(x)[1]}))[samples])
            rseq <- seq(ceiling(.1*minr),minr,length.out=10)
            for(rcount in 1:10){
              #Need to calculate the likelihood with first rcount realizations
              temp <- calcLikelihood(proj, data, num_realz=rseq[rcount], samples=samples)
              likes[rcount,] <- temp@likelihoods[[1]]
            }
            plot(NULL, xlim=range(rseq), ylim=range(log10(likes)))
            for(scount in 1:length(samples)) {
              lines(rseq, log10(likes[,scount]), col=scount)
            }
          }
)

setMethod("test_convergence",
          signature(proj="MADproject"),
          function(proj, samples=1:proj@numSamples) {
            #Need to store likelihood values for a variety of realizations
            likes <- matrix(NA, ncol=length(samples), nrow=10)  ##assuming 10 diff realz counts
            ### Loop through the variety of realizations
            minr <- min(unlist(lapply(proj@realizations,function(x){dim(x)[1]}))[samples])
            rseq <- seq(ceiling(.1*minr),minr,length.out=10)
            for(rcount in 1:10){
              #Need to calculate the likelihood with first rcount realizations
              temp <- calcLikelihood(proj, num_realz=rseq[rcount], samples=samples)
              likes[rcount,] <- temp@likelihoods[[1]]
            }
            plot(NULL, xlim=range(rseq), ylim=range(log10(likes)),
                 xlab="Number of Realizations", ylab="log(likelihood)")
            for(scount in 1:length(samples)) {
              lines(rseq, log10(likes[,scount]), col=scount)
            }
          }
)
