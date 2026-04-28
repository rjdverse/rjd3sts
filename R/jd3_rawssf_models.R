#' @include jd3_rawssf.R
NULL

#' Transforms a time invariant state space form based on functions into a state space models represented by matrices.
#'
#' @param jssf The object oriented (java) state space form, which should be time invariant
#'
#' @returns A new Java object based on matrices
#' @export
#'
#' @examples
#' ll<-.local_linear_trend(0.1, 0.1)
#' s<-.seasonal(12, var=.5)
#' m<-.composite(list(ll, s))
#' ssf1<-.ssf(m, .loading(c(0,2)), 1)
#' ssf2<-.ssf_as_time_invariant(ssf1)
#' ll1<-.akf_likelihood(ssf1, rjd3toolkit::ABS$X0.2.09.10.M)
#' ll2<-.akf_likelihood(ssf2, rjd3toolkit::ABS$X0.2.09.10.M)
#' print(ll1$ll-ll2$ll)
.ssf_as_time_invariant<-function(jssf){
    if (! is(jssf, RAWSSF))
        stop("Not a ssf")
    jrslt<-.jcall("jdplus/sts/base.r/StateSpaceModels", "Ljdplus/toolkit/base/core/ssf/univariate/ISsf;", "asTimeInvariant",
                  jssf$internal)
    return(rjd3toolkit::.jd3_object(jrslt, RAWSSF))
}

#' Title
#'
#' @param component
#' @param loading
#' @param evar
#'
#' @returns
#' @export
#'
#' @examples
.ssf<-function(component, loading, evar=0){
    if (! is(loading, RAWLOADING))
        stop("Not a loading")
    if (! is(component, RAWSTATEBLOCK))
        stop("Not a state block")
    jrslt<-.jcall("jdplus/sts/base.r/StateSpaceModels", "Ljdplus/toolkit/base/core/ssf/univariate/ISsf;", "ssf",
                  component$internal, loading$internal, evar)

    return(rjd3toolkit::.jd3_object(jrslt, RAWSSF))

}

#' Title
#'
#' @param component
#' @param measurements
#'
#' @returns
#' @export
#'
#' @examples
.mssf<-function(component, measurements){
    if (! is(measurements, RAWMEASUREMENTS))
        stop("Not measurements")
    if (! is(component, RAWSTATEBLOCK))
        stop("Not a state block")
    jrslt<-.jcall("jdplus/sts/base.r/StateSpaceModels", "Ljdplus/toolkit/base/core/ssf/multivariate/IMultivariateSsf;", "ssf",
                  component$internal, loading$measurements)

    return(rjd3toolkit::.jd3_object(jrslt, RAWMSSF))

}



