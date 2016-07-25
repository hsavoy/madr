#' @include MADproject.R
NULL

#' Apply the requested dimension reduction technique to the
#' inversion data for the given MADproject
#'
#' \code{reduceData} returns ...
#'
#' @param proj ...
#' @param method ...
#' @param params ...
#'
#' @return proj
#'
#' @export
setGeneric("reduceData", function(proj, method, params, ...) {
  standardGeneric("reduceData")
})

setMethod("reduceData",
          signature(proj="MADproject", method="function"), #just function
          function(proj, method) {
            proj@observations <- match.fun(method)(proj@observations)
            # proj@realizations <- lapply(proj@realizations,
            #                             FUN=function(sample){
            #                               as.matrix(apply(sample,1,match.fun(method)))
            #                             }
            #                             )
            reduced <- ddply(proj@realizations, .(sid,rid),
                                       function(df){match.fun(method)(df$value)}
                                       )
            proj@realizations <- data.frame(sid=reduced$sid,
                                            rid=reduced$rid,
                                            zid=1,
                                            value=reduced$V1)
            return(proj)
          }
)

setMethod("reduceData",
          signature(proj="MADproject", method="function", params="function"), #then fit w/ params
          function(proj, method, params, ...) {
            fit <- nls(y~match.fun(method)(t,init.params),
                data=data.frame(t=1:proj@numTimesteps,y=proj@observations),
                start=list(init.params=match.fun(params)(proj@observations)),
                nls.control(warnOnly=TRUE),
                ...
            )
            proj@observations <- coefficients(fit)
            # proj@realizations <- lapply(proj@realizations,
            #                             FUN=function(sample){
            #                               t(apply(sample,1,function(realization){
            #                                 fit <- nls(y~match.fun(method)(t,init.params),
            #                                            data=data.frame(t=1:proj@numTimesteps,
            #                                                            y=realization),
            #                                            start=list(init.params=
            #                                                         match.fun(params)(realization)),
            #                                            nls.control(warnOnly=TRUE),
            #                                            ...
            #                                 )
            #                                 return(coefficients(fit))
            #                               }
            #                               ))
            #                             }
            #                             )
            reduced <- ddply(proj@realizations, .(sid,rid),
                                        function(df){
                                            fit <- nls(y~match.fun(method)(t,init.params),
                                                       data=data.frame(t=df$zid,
                                                                       y=df$value),
                                                       start=list(init.params=
                                                                    match.fun(params)(df$value)),
                                                       nls.control(warnOnly=TRUE),
                                                       ...
                                            )
                                            coeffs <- coefficients(fit)
                                            names(coeffs) <- 1:length(coeffs)
                                            return(coeffs)
                                        }
            )
            reduced <- melt(reduced, c("sid","rid"))
            names(reduced)[3] <- "zid"
            proj@realizations <- reduced
            return(proj)
          }
)
