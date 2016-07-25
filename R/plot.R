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
              if((x@numSamples < 5)) { #&& (length(x@realizations)>0)){
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
                   observations = {
                     if(length(x@observations) == x@numTimesteps){
                      plot(1:x@numTimesteps,x@observations,
                           main="Observations", xlab="time steps",
                           type="l",...)
                      df <- data.frame(obs=x@observations,
                                       time=1:length(x@observations))
                      ggplot(df) + geom_line(aes(x=time,y=obs)) +
                        ylab("zb Observation") + xlab("Time Steps")
                     }
                    },
                   realizations = {
                     if(x@numSamples>6){
                       return("Too many samples for plotting realizations!")
                     }
                     if(length(x@observations) == x@numTimesteps){  #Time series
                         diff <- dplyr::summarise(dplyr::group_by(x@realizations, sid, zid),
                                   p25=quantile(value,probs=0.25),
                                   p75=quantile(value,probs=0.75))
                         diff$obs <- x@observations

                        plots <- dlply(diff, "sid", function(d){
                          ggplot(d)  +
                            geom_ribbon(aes(x=zid, ymin=p25,ymax=p75)) +
                            geom_line(aes(x=zid,y=obs)) +
                            guides(color="none") +
                            xlab("Time steps") +
                            ylab(paste("zb, Observed and Sample",d$sid))
                          }
                        )
                        multiplot(plotlist=plots,cols=round(sqrt(x@numSamples)))
                       } else if (x@numTimesteps == 1) { #steady
                         obs <- data.frame(zid=1:length(x@observations), obs=x@observations)
                         plots <- dlply(merge(x@realizations,obs), "zid",
                                        function(d){
                                          ggplot(d, aes(x=value,fill=sid))  +
                                            geom_density(alpha=0.25) +
                                            geom_vline(aes(xintercept=obs))
                                        }
                         )
                         multiplot(plotlist=plots,cols=round(sqrt(length(x@observations))))
                     } else {  #Reduced
                        obs <- data.frame(zid=1:length(x@observations), obs=x@observations)
                         plots <- dlply(merge(x@realizations,obs), "zid",
                                        function(d){
                                          ggplot(d, aes(x=value, group=as.factor(sid),
                                                        colour=as.factor(sid)))  +
                                            geom_density(alpha=0.25) +
                                            scale_colour_discrete(name = "Sample ID") +
                                            geom_vline(aes(xintercept=obs))
                                        }
                         )
                         multiplot(plotlist=plots,cols=round(sqrt(x@numTheta+x@numAnchors)))
                       }
                    },
                   posteriors = {
                     plots <- dlply(merge(x@posteriors,x@truevalues), "tid",
                                    function(d){
                                      ggplot(d, aes(x=priorvalue, weight=post))  +
                                        geom_density(weight=1, fill=NA, colour="red") +
                                        geom_density(fill=NA, colour="blue") +
                                        geom_vline(aes(xintercept = value), na.rm=TRUE) +
                                        xlab(unique(d$name))
                                    }
                     )
                     multiplot(plotlist=plots,cols=round(sqrt(x@numTheta+x@numAnchors)))
                    },
                   priors = {  ### always doing histogram
                     plots <- dlply(x@priors, "tid",
                                    function(d){
                                      ggplot(d, aes(priorvalue,y=priordens))  +
                                      geom_bar(stat="identity", width=1/length(d$priorvalue)) +
                                        geom_density(aes(priorvalue,..density..),fill=NA, colour="red")+
                                        xlab(unique(d$name)) + ylab("density")
                                    }
                     )
                     suppressWarnings(multiplot(plotlist=plots,cols=round(sqrt(x@numTheta+x@numAnchors))))
                   }
                    )
          }
)
