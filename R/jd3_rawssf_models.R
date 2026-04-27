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
#' @param ssf
#'
#' @returns
#' @export
#'
#' @examples
.ssf_component<-function(ssf){
    if (! is(ssf, RAWSSF))
        stop("Not a State space form")
    jrslt<-.jcall("jdplus/sts/base.r/StateSpaceModels", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "componentOf", ssf$internal)
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}

#' Title
#'
#' @param ssf
#'
#' @returns
#' @export
#'
#' @examples
.ssf_loading<-function(ssf){
    if (! is(ssf, RAWSSF))
        stop("Not a State space form")
    jrslt<-.jcall(ssf$internal, "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "loading")
    return(rjd3toolkit::.jd3_object(jrslt, RAWLOADING))
}

#' Title
#'
#' @param state
#'
#' @returns
#' @export
#'
#' @examples
.state_initialization<-function(state){
    if (! is(state, RAWSTATEBLOCK))
        stop("Not a State block")
    jrslt<-.jcall(state$internal, "Ljdplus/toolkit/base/core/ssf/ISsfInitialization;", "initialization")
    return(rjd3toolkit::.jd3_object(jrslt))
}
