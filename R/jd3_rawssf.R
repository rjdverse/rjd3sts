#' @include utils.R
NULL

RAWSTATEBLOCK<-'JD3_RawStateBlock'
RAWLOADING<-'JD3_RawLoading'
RAWMEASUREMENT<-'JD3_RawMeasurement'
RAWMEASUREMENTS<-'JD3_RawMeasurements'
RAWSSF<-'JD3_RawSSF'
RAWMSSF<-'JD3_RawMSSF'

.dynamics<-function(x){
    if (is(x, RAWSTATEBLOCK))
        return (.jcall(x$internal, "Ljdplus/toolkit/base/core/ssf/ISsfDynamics;", "dynamics"))
    else if (is(x, RAWSSF))
        return (.jcall(x$internal, "Ljdplus/toolkit/base/core/ssf/ISsfDynamics;", "dynamics"))
    else if (is(x, RAWSSF))
        return (.jcall(x$internal, "Ljdplus/toolkit/base/core/ssf/ISsfDynamics;", "dynamics"))
    stop("Invalid parameter")
}

.initialization<-function(x){
    if (is(x, RAWSTATEBLOCK))
        return (.jcall(x$internal, "Ljdplus/toolkit/base/core/ssf/ISsfInitialization;", "initialization"))
    else if (is(x, RAWSSF))
        return (.jcall(x$internal, "Ljdplus/toolkit/base/core/ssf/ISsfInitialization;", "initialization"))
    else if (is(x, RAWSSF))
        return (.jcall(x$internal, "Ljdplus/toolkit/base/core/ssf/ISsfInitialization;", "initialization"))
    stop("Invalid parameter")
}

.loading<-function(ssf){
    if (! is(ssf, RAWSSF))
        stop("Not a State space form")
    jrslt<-.jcall(ssf$internal, "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "loading")
    return(rjd3toolkit::.jd3_object(jrslt, RAWLOADING))
}

#' Gets the state of the state space form
#'
#' @param ssf A state space form
#'
#' @returns A state block
#' @export
#'
#' @examples
#'
.state_of<-function(ssf){
    if (! is(ssf, RAWSSF))
        stop("Not a State space form")
    jrslt<-.jcall("jdplus/sts/base.r/StateSpaceModels", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "componentOf", ssf$internal)
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}

#' Gets the transition matrix.
#'
#' @param x A state block or a state space form.
#' @param pos The 0-based position (use 0 for time invariant models)
#'
#' @returns The requested matrix.
#' @export
#'
#' @examples
#' sarima<-.sarma(12, .9, -.6, .9, -.5)
#' .ssf_T(sarima)
.ssf_T<-function(x, pos=0){
    if (is(x, RAWSTATEBLOCK))
        block<-x$internal
    else if (is(x, RAWSSF)){
        block<-.jcall("jdplus/sts/base.r/StateSpaceModels", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "componentOf", ssf$internal)
    }else
        stop("Invalid argument")
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "T", block, as.integer(pos))
    return (rjd3toolkit::.jd2r_matrix(jrslt))
}

#' Gets the covariance of the innovations in the transition equation.
#'
#' @param x A state block or a state space form.
#' @param pos The 0-based position (use 0 for time invariant models)
#'
#' @returns The requested matrix.
#' @export
#'
#' @examples
#' sarima<-.sarma(12, .9, -.6, .9, -.5)
#' .ssf_V(sarima)
.ssf_V<-function(x, pos=0){
    if (is(x, RAWSTATEBLOCK))
        block<-x$internal
    else if (is(x, RAWSSF)){
        block<-.jcall("jdplus/sts/base.r/StateSpaceModels", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "componentOf", ssf$internal)
    }else
        stop("Invalid argument")
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "V", block, as.integer(pos))
    return (rjd3toolkit::.jd2r_matrix(jrslt))
}

#' Title
#'
#' @param x
#' @param pos
#'
#' @returns
#' @export
#'
#' @examples
.ssf_S<-function(x, pos=0){
    if (is(x, RAWSTATEBLOCK))
        block<-x$internal
    else if (is(x, RAWSSF)){
        block<-.jcall("jdplus/sts/base.r/StateSpaceModels", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "componentOf", ssf$internal)
    }else
        stop("Invalid argument")
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "S", block, as.integer(pos))
    return (rjd3toolkit::.jd2r_matrix(jrslt))
}

#' Retrieves the dimension of a state block
#'
#' @param x A state block
#'
#' @returns The length of the state block
#' @export
#'
#' @examples
#' s<-.seasonal(12)
#' .state_dim(s)
.state_dim<-function(x){
    jinitialization<-.initialization(x)
    return (.jcall(jinitialization, "I", "getStateDim"))
}

#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
.state_diffuse_dim<-function(x){
    jinitialization<-.initialization(x)
    return (.jcall(jinitialization, "I", "getDiffuseDim"))
}


#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
.ssf_P0<-function(x){
    if (is(x, RAWSTATEBLOCK))
        block<-x$internal
    else if (is(x, RAWSSF)){
        block<-.jcall("jdplus/sts/base.r/StateSpaceModels", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "componentOf", ssf$internal)
    }else
        stop("Invalid argument")
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "P0", block)
    return (rjd3toolkit::.jd2r_matrix(jrslt))
}

#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
.ssf_B<-function(x){
    if (is(x, RAWSTATEBLOCK))
        block<-x$internal
    else if (is(x, RAWSSF)){
        block<-.jcall("jdplus/sts/base.r/StateSpaceModels", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "componentOf", ssf$internal)
    }else
        stop("Invalid argument")
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "B", block)
    return (rjd3toolkit::.jd2r_matrix(jrslt))
}






