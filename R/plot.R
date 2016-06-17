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
#' @export
setGeneric("plot")

setMethod("plot",
          signature(x="MADproject"),   #plot all available plots
          function(x,...) {
            #Plot observations
            if(length(x@observations) > 0){
              plot(x,"observations",...)
              #Plot realizations if not too many samples
              if((x@numSamples < 5) && (length(x@realizations)>0)){
                plot(x,"realizations",...)
              }
            }
            #Plot posterior
            if(length(x@posteriors) > 0) plot(x, "posteriors")
          }
)

setMethod("plot",
          signature(x="MADproject", y="character"),
          function(x,y,...) {
            switch(y,
                   observations = {  #assuming one meas location
                     if(length(x@observations) == x@numTimesteps){
                      plot(1:x@numTimesteps,x@observations,
                           main="Observations", xlab="time steps",
                           type="l",...)
                     }
                    },
                   realizations = {
                     #.pardefault <- par(no.readonly = TRUE)
                     #par(mfrow=c())
                     if(length(x@observations) == x@numTimesteps){  #Time series
                       plot(1:x@numTimesteps,x@observations,
                            main="Observations+Realizations", xlab="time steps",
                            type="l",...)
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
                       } else {  #Reduced
                         .pardefault <- par(no.readonly = TRUE)
                         par(mfrow=c(ceiling(sqrt(length(x@observations))),
                                     ceiling(length(x@observations)/
                                             ceiling(sqrt(length(x@observations))))))
                         #par(oma=c(0,0,0,0))
                         #par(mar=c(1,1,1,1))
                        for(param in 1:length(x@observations)) {
                          graphics::hist(x@realizations[[1]][,param],
                               xlim=range(lapply(x@realizations,
                                                 function(sample){range(sample[,param])})),
                               ylim=c(0,dim(x@realizations[[1]])[1]/2),
                               xlab=paste("Parameter",param),
                               col=grDevices::adjustcolor(2,alpha.f=0.2),
                               main="Realization Parameter Histograms")
                          for(sample in 2:x@numSamples){
                            graphics::hist(x@realizations[[sample]][,param], add=TRUE,
                                 col=grDevices::adjustcolor(sample+1,alpha.f=0.2))
                            abline(v=x@observations[param])
                          }
                        }
                         par(.pardefault)
                       }

                     #par(.pardefault)
                    },
                   posteriors = {  #assumes small sample discrete distribtion
                     for(i in 1:length(x@posteriors)){
                       graphics::barplot(x@posteriors[[i]], main="Posteriors",
                            names.arg=paste0("S",1:x@numSamples),
                            col=grDevices::adjustcolor((1:x@numSamples)+1,alpha.f=0.2),
                            ylim=c(0,1),
                            ...
                            )
                     }
                    }
                    )
          }
)
