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
            proj@observations <- as.matrix(match.fun(method)(proj@observations))
            proj@realizations <- lapply(proj@realizations,
                                        FUN=function(sample){
                                          as.matrix(apply(sample,1,match.fun(method)))
                                        }
                                        )
            return(proj)
          }
)

setMethod("reduceData",
          signature(proj="MADproject", method="function", params="function"), #then fit w/ params
          function(proj, method, params, ...) {
            fit <- nls(y~match.fun(method)(t,init.params),
                data=data.frame(t=1:proj@numTimesteps,y=proj@observations[,1]),
                start=list(init.params=match.fun(params)(proj@observations[,1])),
                nls.control(warnOnly=TRUE)
            )
            proj@observations <- as.matrix(coefficients(fit))
            proj@realizations <- lapply(proj@realizations,
                                        FUN=function(sample){
                                          t(apply(sample,1,function(realization){
                                            fit <- nls(y~match.fun(method)(t,init.params),
                                                       data=data.frame(t=1:proj@numTimesteps,
                                                                       y=realization),
                                                       start=list(init.params=
                                                                    match.fun(params)(realization)),
                                                       nls.control(warnOnly=TRUE),
                                                       ...
                                            )
                                            return(coefficients(fit))
                                          }
                                          ))
                                        }
                                        )
            return(proj)
          }
)
