#' @include utils.R
NULL

RAWSTATEBLOCK<-'JD3_RawStateBlock'

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
#' @param type
#' @param var
#'
#' @returns
#' @export
#'
#' @examples
.seasonal<-function(period, type=c("Trigonometric", "Crude", "HarrisonStevens", "Dummy"), var){
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
.noise<-function(var){
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
.local_level<-function(var, start=NaN){
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

