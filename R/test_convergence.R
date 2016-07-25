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
            .pardefault <- par(no.readonly = TRUE)
            par(xpd=TRUE)
            plot(NULL, xlim=range(rseq), ylim=range(log10(likes), na.rm=TRUE, finite=TRUE))
            for(scount in 1:length(samples)) {
              lines(rseq, log10(likes[,scount]), col=scount)
            }
            graphics::legend("top",legend=c("Obs.",
                                                 paste0("S",samples)),
                             horiz=TRUE, inset=c(0,-0.2),
                             col=c(1,1+samples),lty=1,bg="transparent", bty="n")
            par(.pardefault)
          }
)

setMethod("test_convergence",
          signature(proj="MADproject"),
          function(proj, samples=1:proj@numSamples, NR=10, NS=7) {
            minr <- min(daply(subset(proj@realizations,sid %in% samples),
                              .(sid), function(df) max(df$rid)))
            samps <- sample(samples,NS)
            nr=seq(ceiling(.1*minr),minr,length.out=NR)
            likes <- adply(nr, 1, function(x) calcLikelihood(proj,
                                                             num_realz=x,
                                                             samples=samps)@likelihoods)
            likes$nr <- nr[likes$X1]
            likes$sid <- as.factor(likes$sid)
           ggplot(likes, aes(x=nr, y=like, group=sid, colour=sid))  +
             geom_line() + scale_y_log10() + xlab("Number of Realizations") +
             ylab("Log 10 Likelihood") +  scale_colour_discrete(name = "Sample ID")

          }
)
