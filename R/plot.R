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
                       } else if (x@numTimesteps == 1) { #steady
                         .pardefault <- par(no.readonly = TRUE)
                         total <- dim(x@priors)[2]
                         par(mfrow=c(ceiling(sqrt(total)),
                                     ceiling( total/ceiling( sqrt(total) ) )))
                          for(location in 1:length(x@observations)) {
                            plot(density(x@realizations[[1]][,location]),type="l",
                                 xlim=range(lapply(x@realizations,
                                                   function(sample){range(sample[,location])})),
                                 col=grDevices::adjustcolor(2,alpha.f=1),
                                 main="Location Realization Histograms",
                                 xlab=paste("Location", location)
                            )
                             for(sample in 2:x@numSamples){
                               dens <- density(x@realizations[[sample]][,location])
                               lines(dens$x, dens$y,
                                     col=grDevices::adjustcolor(sample+1,alpha.f=1))
                             }
                             abline(v=x@observations[location])
                          }
                     } else {  #Reduced
                         .pardefault <- par(no.readonly = TRUE)
                         par(mfrow=c(ceiling(sqrt(length(x@observations))),
                                     ceiling(length(x@observations)/
                                             ceiling(sqrt(length(x@observations))))))
                        for(param in 1:length(x@observations)) {
                          plot(density(x@realizations[[1]][,param]),type="l",
                               xlim=range(lapply(x@realizations,
                              function(sample){range(sample[,param])})),
                              col=grDevices::adjustcolor(2,alpha.f=1),
                              main="Realization Parameter Histograms",
                              xlab=paste("Parameter", param)
                              )
                          for(sample in 2:x@numSamples){
                            dens <- density(x@realizations[[sample]][,param])
                            lines(dens$x, dens$y,
                                  col=grDevices::adjustcolor(sample+1,alpha.f=1))
                          }
                          abline(v=x@observations[param])

                        }
                         legend("left", legend=paste("Sample",1:x@numSamples),
                                col=1+(1:x@numSamples))
                         par(.pardefault)
                       }
                    },
                   posteriors = {  #assumes small sample discrete distribution
                     .pardefault <- par(no.readonly = TRUE)
                     total <- x@numTheta + x@numAnchors
                     par(mfrow=c(ceiling(sqrt(total)),
                                 ceiling( total/ceiling( sqrt(total) ) )
                     )
                     )
                     for(theta in 1:total){
                       if(x@numSamples< 10){  #do discrete
                         graphics::barplot(x@posteriors[[theta]][,1], main="Posteriors",
                                           names.arg=paste0("S",1:x@numSamples),
                                           col=grDevices::adjustcolor((1:x@numSamples)+1,alpha.f=0.2),
                                           ylim=c(0,1),
                                           ...
                         )
                       } else {  #do continuous
                         graphics::plot(density(x@posteriors[[theta]][,1]), main="Posteriors",
                                           #col=grDevices::adjustcolor((1:x@numSamples)+1,alpha.f=0.2),
                                           #ylim=c(0,1),
                                           ...
                         )
                       }
                         if(length(x@truevalues) > 0) abline(v=x@truevalues[theta])
                     }
                    },
                   priors = {  ### always doing histogram
                     .pardefault <- par(no.readonly = TRUE)
                     total <- dim(x@priors)[2]
                     par(mfrow=c(ceiling(sqrt(total)),
                              ceiling( total/ceiling( sqrt(total) ) )
                              )
                         )
                     for(param in 1:total){
                       graphics::hist(x@priors[,param], main=paste("Prior",param),
                                      freq=FALSE,
                                         #col=grDevices::adjustcolor(param+1,alpha.f=0.2),
                                         #ylim=c(0,1),
                                         ...
                       )
                     }
                     par(.pardefault)
                   }
                    )
          }
)
