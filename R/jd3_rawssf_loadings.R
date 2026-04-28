#' @include jd3_rawssf.R
NULL

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

