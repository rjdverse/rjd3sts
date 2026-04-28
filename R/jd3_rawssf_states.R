#' @include jd3_rawssf.R
NULL

#' Creates an ARIMA state block (representation I)
#'
#' @param ar Stationary auto-regressive polynomial, including the constant (=1). True signs.
#' @param delta Non-stationary auto-regressive polynomial, including the constant (=1). True signs.
#' @param ma Moving average polynomial, including the constant (=1). True signs.
#' @param var Variance of the innovations
#'
#' @returns A raw java state block.
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

#' Creates an ARMA state block
#'
#' @param period Period of the seasonality
#' @param phi Regular stationary auto-regressive polynomial. True signs, without the constant
#' @param d Regular differencing order
#' @param theta Regular moving average polynomial. True signs, without the constant
#' @param bphi Seasonal stationary auto-regressive polynomial. True signs, without the constant
#' @param bd Seasonal differencing order
#' @param btheta Seasonal moving average polynomial. True signs, without the constant
#'
#' @returns A raw java state block
#' @export
#'
#' @examples
#' sb<-.sarima(12, c(0.5, -.2, .1), 1, -.8, NULL, 1, -.6)
#' .ssf_P0(sb)
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

#' Creates an ARMA state block (representation I)
#'
#' @param period Period of the seasonality
#' @param phi Regular stationary auto-regressive polynomial. True signs, without the constant
#' @param theta Regular moving average polynomial. True signs, without the constant
#' @param bphi Seasonal stationary auto-regressive polynomial. True signs, without the constant
#' @param btheta Seasonal moving average polynomial. True signs, without the constant
#'
#' @returns A raw java state block
#' @export
#'
#' @examples
#' sb<-.sarma(12, c(0.5, -.2, .1), -.8, NULL, -.6)
#' .ssf_P0(sb)
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

#' Creates an ARMA state block (representation II)
#'
#' @param period Period of the seasonality
#' @param phi Regular stationary auto-regressive polynomial. True signs, without the constant
#' @param theta Regular moving average polynomial. True signs, without the constant
#' @param bphi Seasonal stationary auto-regressive polynomial. True signs, without the constant
#' @param btheta Seasonal moving average polynomial. True signs, without the constant
#'
#' @returns A raw java state block
#' @export
#'
#' @examples
#' sb<-.sarma2(12, c(0.5, -.2, .1), -.8, NULL, -.6)
#' .ssf_P0(sb)
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


#' Creates an ARIMA state block (representation II)
#'
#' @param ar Stationary auto-regressive polynomial, including the constant (=1). True signs.
#' @param delta Non-stationary auto-regressive polynomial, including the constant (=1). True signs.
#' @param ma Moving average polynomial, including the constant (=1). True signs.
#' @param var Variance of the innovations
#'
#' @returns A raw java state block
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


#' Creates a seasonal component, corresponding to a multivariate random walk,
#' with an aggregation constraint to 0 and various covariances for the innovations of the transition equation.
#'
#' @param period Period of the seasonality
#' @param type Type of the innovations of the transition equation
#' @param var Variance of the innovations
#'
#' @returns A wrapper around the java object (class JD3_RawStateBlock)
#' @export
#'
#' @examples
#' sb<-.seasonal(4, "HarrisonStevens", .01)
#' .ssf_V(sb, 0)
.seasonal<-function(period, type=c("Trigonometric", "Crude", "HarrisonStevens", "Dummy"), var=1){
    type <- match.arg(type)

    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "seasonal", type, as.integer(period), as.numeric(var))
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}

#' Creates a white noise.
#'
#' @param var Variance of the noise.
#'
#' @returns A wrapper around the java object (class JD3_RawStateBlock).
#' @export
#'
#' @examples
#' sb<-.noise(.01)
#' .ssf_T(sb, 0)
.noise<-function(var=1){
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "noise", as.numeric(var))
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}

#' Creates a local level state block
#'
#' @param var Innovation variance of local level
#' @param start Initial value of the state block. Should be NaN for a diffuse initialization
#'
#' @returns A wrapper around the java object (class JD3_RawStateBlock).
#' @export
#'
#' @examples
#' sb<-.local_level(1.5, 0)
#' .ssf_T(sb, 0)
.local_level<-function(var=1, start=NaN){
    jrslt<-.jcall("jdplus/sts/base/r/StateBlocks", "Ljdplus/toolkit/base/core/ssf/StateComponent;", "localLevel", as.numeric(var), as.numeric(start))
    return(rjd3toolkit::.jd3_object(jrslt, RAWSTATEBLOCK))
}

#' Creates a local linear trend state block.
#'
#' @param lvar Innovation variance of the level equation.
#' @param svar Innovation variance of the slope equation.
#'
#' @returns A wrapper around the java object (class JD3_RawStateBlock).
#' @export
#'
#' @examples
#' sb<-.local_linear_trend(1.5, 0.5)
#' .ssf_T(sb, 0)
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

#' Creates the state space form of an airline model;
#'
#' @param period Period of the model.
#' @param theta Regular moving average parameter.
#' @param btheta Seasonal moving average parameter.
#'
#' @returns The raw state space form of the Airline model.
#' @export
#'
#' @examples
#' ssf<-.airline(24)
#' .dk_likelihood(ssf, rjd3toolkit::ABS$X0.2.09.10.M)
.airline<-function(period=12, theta=-.6, btheta=-.8){
    sarima<-.sarima(period, NULL, 1,  theta, NULL, 1, btheta)
    return (.ssf(sarima, .loading(0)))
}
