#' @include MADproject.R
NULL

#' Plot the MADproject object
#'
#' \code{plot.MADproject} plots ...
#'
#' @param x The MADproject object
#' @param y not supported
#' @param ... not supported
#' @return NULL.
#'
setGeneric("plot")

setMethod("plot",
          signature(x="MADproject"),   #plot all available plots
          function(x) {
            message("I'll do my own plots!")
            #Plot observations
            if(dim(x@observations)[1] > 0){
              plot(x,"observations")
              #Plot realizations if not too many samples
              if((x@numSamples < 5) && (length(x@realizations)>0)){
                plot(x,"realizations")
              }
            }
            #Plot posterior
            if(length(x@posteriors) > 0) plot(x, "posteriors")
          }
)

setMethod("plot",
          signature(x="MADproject", y="character"),
          function(x,y) {
            switch(y,
                   observations = {  #assuming one meas location
                      plot(1:x@numTimesteps,x@observations[,1],
                           main="Observations", xlab="time steps",
                           type="l")
                    },
                   realizations = {
                     #.pardefault <- par(no.readonly = TRUE)
                     #par(mfrow=c())
                     plot(1:x@numTimesteps,x@observations[,1],
                          main="Observations+Realizations", xlab="time steps",
                          type="l")
                     samples <- 1:x@numSamples
                     for(sample in samples){
                       len <- x@numTimesteps
                       diff1 <- apply(x@realizations[[sample]][,1:len],2,  #assumes one meas
                                      stats::quantile,probs=.25, na.rm=TRUE)
                       diff2 <- apply(x@realizations[[sample]][,1:len],2,
                                      stats::quantile,probs=.75, na.rm=TRUE)
                       graphics::polygon(c(1:len,len:1),
                               c(diff1,rev(diff2)),
                               col=grDevices::adjustcolor(sample+1,alpha.f=0.2),
                               border=NA)
                     }
                     graphics::legend("topright",legend=c("Obs.",
                                                paste0("S",samples)),
                            col=c(1,1+samples),lty=1,bg="transparent", bty="n")
                     #par(.pardefault)
                    },
                   posteriors = {  #assumes small sample discrete distribtion
                     for(i in 1:length(x@posteriors)){
                       graphics::barplot(x@posteriors[[i]], main="Posteriors",
                            names.arg=paste0("S",1:x@numSamples))
                     }
                    }
                    )
          }
)
