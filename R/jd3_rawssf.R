#' @include utils.R
NULL

RAWSTATEBLOCK<-'JD3_RawStateBlock'
RAWLOADING<-'JD3_RawLoading'
RAWMEASUREMENT<-'JD3_RawMeasurement'
RAWMEASUREMENTS<-'JD3_RawMeasurements'
RAWSSF<-'JD3_RawSSF'
RAWMSSF<-'JD3_RawMSSF'

#' Title
#'
#' @param ar
#' @param delta
#' @param ma
#' @param var
#'
#' @returns
#' @export
#'
#' @examples
#' sb<-.arima(c(1, -.5), NULL, c(1,-.8))
#' .ssf_P0(sb)
.arima<-function(ar, delta, ma, var=1){
    if (is.null(ar))
        jar<-.jnull("[D")
    else
        jar<-.jarray(ar)
    if (is.null(delta))
        jdelta<-.jnull("[D")
    else
        jdelta<-.jarray(delta)
    if (is.null(ma))
        jma<-.jnull("[D")
    else
        jma<-.jarray(ma)

    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "arima", jar, jdelta, jma, as.numeric(var))
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}

#' Title
#'
#' @param period
#' @param phi
#' @param d
#' @param theta
#' @param bphi
#' @param bd
#' @param btheta
#'
#' @returns
#' @export
#'
#' @examples
.sarima<-function(period, phi, d=1, theta, bphi, bd=1, btheta){
    if (is.null(phi))
        jphi<-.jnull("[D")
    else
        jphi<-.jarray(phi)
    if (is.null(theta))
        jtheta<-.jnull("[D")
    else
        jtheta<-.jarray(theta)
    if (is.null(bphi))
        jbphi<-.jnull("[D")
    else
        jbphi<-.jarray(bphi)
    if (is.null(btheta))
        jbtheta<-.jnull("[D")
    else
        jbtheta<-.jarray(btheta)

    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "sarima", as.integer(period),
                  jphi, as.integer(d), jtheta,
                  jbphi, as.integer(bd), jbtheta
                  )
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}

#' Title
#'
#' @param period
#' @param phi
#' @param d
#' @param theta
#' @param bphi
#' @param bd
#' @param btheta
#'
#' @returns
#' @export
#'
#' @examples
.sarma<-function(period, phi, theta, bphi, btheta){
    if (is.null(phi))
        jphi<-.jnull("[D")
    else
        jphi<-.jarray(phi)
    if (is.null(theta))
        jtheta<-.jnull("[D")
    else
        jtheta<-.jarray(theta)
    if (is.null(bphi))
        jbphi<-.jnull("[D")
    else
        jbphi<-.jarray(bphi)
    if (is.null(btheta))
        jbtheta<-.jnull("[D")
    else
        jbtheta<-.jarray(btheta)

    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "sarma", as.integer(period),
                  jphi, jtheta,
                  jbphi, jbtheta
    )
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}

#' Title
#'
#' @param period
#' @param phi
#' @param d
#' @param theta
#' @param bphi
#' @param bd
#' @param btheta
#'
#' @returns
#' @export
#'
#' @examples
.sarma2<-function(period, phi, theta, bphi, btheta){
    if (is.null(phi))
        jphi<-.jnull("[D")
    else
        jphi<-.jarray(phi)
    if (is.null(theta))
        jtheta<-.jnull("[D")
    else
        jtheta<-.jarray(theta)
    if (is.null(bphi))
        jbphi<-.jnull("[D")
    else
        jbphi<-.jarray(bphi)
    if (is.null(btheta))
        jbtheta<-.jnull("[D")
    else
        jbtheta<-.jarray(btheta)

    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "sarma2", as.integer(period),
                  jphi, jtheta,
                  jbphi, jbtheta
    )
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}


#' Title
#'
#' @param ar
#' @param delta
#' @param ma
#' @param var
#'
#' @returns
#' @export
#'
#' @examples
#' sb<-.arima2(c(1, -.5), NULL, c(1,-.8))
#' .ssf_P0(sb)
.arima2<-function(ar, delta, ma, var=1){
    if (is.null(ar))
        jar<-.jnull("[D")
    else
        jar<-.jarray(ar)
    if (is.null(delta))
        jdelta<-.jnull("[D")
    else
        jdelta<-.jarray(delta)
    if (is.null(ma))
        jma<-.jnull("[D")
    else
        jma<-.jarray(ma)

    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "arima2", jar, jdelta, jma, as.numeric(var))
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}


#' Title
#'
#' @param period
#' @param type
#' @param var
#'
#' @returns
#' @export
#'
#' @examples
.seasonal<-function(period, type=c("Trigonometric", "Crude", "HarrisonStevens", "Dummy"), var=1){
    type <- match.arg(type)

    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "seasonal", type, as.integer(period), as.numeric(var))
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}

#' Title
#'
#' @param var
#'
#' @returns
#' @export
#'
#' @examples
.noise<-function(var=1){
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "noise", as.numeric(var))
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}

#' Title
#'
#' @param var
#' @param start
#'
#' @returns
#' @export
#'
#' @examples
.local_level<-function(var=1, start=NaN){
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "localLevel", as.numeric(var), as.numeric(start))
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}

#' Title
#'
#' @param lvar
#' @param svar
#'
#' @returns
#' @export
#'
#' @examples
.local_linear_trend<-function(lvar, svar=0){
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "localLinearTrend", as.numeric(lvar), as.numeric(svar))
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}

#' Title
#'
#' @param cmps
#'
#' @returns
#' @export
#'
#' @examples
.composite<-function(cmps){
    if (! is.list(cmps))
        stop("cmps should be a list of raw state components")

    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "composite",
                  .jarray(sapply(cmps, function(z)z$internal), "jdplus/toolkit/base/core/ssf/StateComponent"))
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
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
.ssf_T<-function(x, pos=0){
    if (! is(x, RAWSTATEBLOCK))
        stop("Not a state block")
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "T", x$internal, as.integer(pos))
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
.ssf_V<-function(x, pos=0){
    if (! is(x, RAWSTATEBLOCK))
        stop("Not a state block")
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "V", x$internal, as.integer(pos))
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
    if (! is(x, RAWSTATEBLOCK))
        stop("Not a state block")
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "S", x$internal, as.integer(pos))
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
#' .ssf_dim(s)
.ssf_dim<-function(x){
    if (! is(x, RAWSTATEBLOCK))
        stop("Not a state block")
    return (jrslt<-.jcall(x$internal, "I", "dim"))
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
    if (! is(x, RAWSTATEBLOCK))
        stop("Not a state block")
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "P0", x$internal)
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
    if (! is(x, RAWSTATEBLOCK))
        stop("Not a state block")
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "B", x$internal)
    return (rjd3toolkit::.jd2r_matrix(jrslt))
}

#' Title
#'
#' @param pos
#' @param weights
#'
#' @returns
#' @export
#'
#' @examples
.loading<-function(pos, weights = NULL){
    ipos<-as.integer(pos)
    if (anyNA(ipos))
        stop("Invalid positions")
    if (length(ipos) == 1){
        if (is.null(weights)){
            jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "fromPosition",
                          ipos)
        }else{
            jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "from",
                          ipos, as.numeric(weights))

        }
    }else{
        if (is.null(weights)){
            jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "fromPositions",
                          .jarray(ipos))
        }else{
            jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "from",
                          .jarray(ipos), .jarray(as.numeric(weights)))

        }
    }
    return(rjd3toolkit::.jd3_object(jrslt, RAWLOADING))
}

#' Title
#'
#' @param period
#' @param start
#'
#' @returns
#' @export
#'
#' @examples
.circular_loading<-function(period, start=0){
    jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "circular",
                  as.integer(period), as.integer(start))

    return(rjd3toolkit::.jd3_object(jrslt, RAWLOADING))
}

#' Title
#'
#' @param pos
#' @param start
#'
#' @returns
#' @export
#'
#' @examples
.cyclical_loading<-function(period, start=0){
    jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "cyclical",
                  as.integer(period), as.integer(start))

    return(rjd3toolkit::.jd3_object(jrslt, RAWLOADING))
}



#' Gets the loading vector. It should have the same length as the corresponding state block.
#'
#' @param x Loading
#' @param pos 0-based position of the filter
#' @param n the Length of the corresponding state block (on which the loading will be applied)
#' @returns the loading vector
#' @export
#'
#' @examples
#'
.ssf_Z<-function(x, n, pos){
    if (! is(x, RAWLOADING))
        stop("Not a loading")
    return (.jcall("jdplus/sts/base/r/Measurements", "[D", "Z", x$internal, as.integer(n), as.integer(pos)))
}

.ssf_invariant<-function(a0=NULL, P0=NULL, B0=NULL, T, V=NULL, S=NULL, Z, h=0){

}

#' Creates a composite loading
#'
#' @param dims The dimensions of the corresponding state blocks
#' @param cmps The loadings
#'
#' @returns
#' @export
#'
#' @examples
#' l<-.loading_composite(c(1,2,3), list(.loading(0), .loading(0), .loading(0)))
#' .ssf_Z(l, 6, 0)
.loading_composite<-function(dims, cmps){
    if (! is.list(cmps))
        stop("cmps should be a list of raw loadings")

    jrslt<-.jnew("jdplus/toolkit/base/core/ssf/composite/CompositeLoading", .jarray(as.integer(dims)),
                  .jarray(sapply(cmps, function(z)z$internal), "jdplus/toolkit/base/core/ssf/ISsfLoading"))

    return(rjd3toolkit::.jd3_object(.jcast(jrslt, "jdplus/toolkit/base/core/ssf/ISsfLoading"), RAWLOADING))
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
#' @param Z
#' @param H
#'
#' @returns
#' @export
#'
#' @examples
.mssf_measurements<-function(Z, H){
    jz<-rjd3toolkit::.r2jd_matrix(Z)
    jh<-rjd3toolkit::.r2jd_matrix(H)
    jrslt<-.jcall("jdplus/sts/base.r/Measurements", "Ljdplus/toolkit/base/core/ssf/multivariate/ISsfMeasurements;", "of",
                  jz, jh)

    return(rjd3toolkit::.jd3_object(jrslt, RAWMEASUREMENTS))
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
#' @param ssf
#' @param data
#' @param all
#' @param qtype
#'
#' @returns
#' @export
#'
#' @examples
.ssf_smooth<-function(ssf, data, all=TRUE, qtype=c("NORMAL", "PARTIAL_TRIANGULARIZATION", "FULL_TRIANGULARIZATION", "QR")){
    qtype<-match.arg(qtype)
    jrslt<-.jcall("jdplus/sts/base.r/Algorithms", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "smooth", ssf$internal, as.numeric(data), as.logical(all), qtype)
    return (rjd3toolkit::.jd2r_matrix(jrslt))
}

#' Title
#'
#' @param ssf
#' @param data
#' @param rescalingFactor
#' @param qtype
#'
#' @returns
#' @export
#'
#' @examples
.akf_likelihood<-function(ssf, data, qtype=c("NORMAL", "PARTIAL_TRIANGULARIZATION", "FULL_TRIANGULARIZATION", "QR"), collapsing = TRUE, rescalingFactor=TRUE){
    qtype<-match.arg(qtype)
    jrslt<-.jcall("jdplus/sts/base.r/Algorithms", "Ljdplus/toolkit/base/core/ssf/likelihood/DiffuseLikelihood;", "akfLikelihood", ssf$internal, as.numeric(data),
                  qtype, as.logical(collapsing), as.logical(rescalingFactor))
    return (.jd2r_diffuse_likelihood(jrslt))
}

#' Title
#'
#' @param ssf
#' @param data
#' @param sqr
#' @param rescalingFactor
#'
#' @returns
#' @export
#'
#' @examples
.dk_likelihood<-function(ssf, data, sqr = TRUE, rescalingFactor=TRUE){
    jrslt<-.jcall("jdplus/sts/base.r/Algorithms", "Ljdplus/toolkit/base/core/ssf/likelihood/DiffuseLikelihood;", "dkLikelihood", ssf$internal, as.numeric(data),
                  as.logical(sqr), as.logical(rescalingFactor))
    return (.jd2r_diffuse_likelihood(jrslt))
}

#' Title
#'
#' @param ssf
#' @param data
#' @param rescalingFactor
#'
#' @returns
#' @export
#'
#' @examples
.ckms_likelihood<-function(ssf, data, rescalingFactor=TRUE){
    jrslt<-.jcall("jdplus/sts/base.r/Algorithms", "Ljdplus/toolkit/base/core/stats/likelihood/Likelihood;", "ckmsLikelihood", ssf$internal,
                  as.numeric(data), as.logical(rescalingFactor))
    return (jrslt)
}


#' Title
#'
#' @param jssf
#'
#' @returns
#' @export
#'
#' @examples
.ssf_as_time_invariant<-function(jssf){
    if (! is(jssf, RAWSSF))
        stop("Not a ssf")
    jrslt<-.jcall("jdplus/sts/base.r/StateSpaceModels", "Ljdplus/toolkit/base/core/ssf/univariate/ISsf;", "asTimeInvariant",
                  jssf$internal)
     return(rjd3toolkit::.jd3_object(jrslt, RAWSSF))
}
