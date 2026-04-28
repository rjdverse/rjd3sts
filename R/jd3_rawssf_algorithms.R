#' @include jd3_rawssf.R
NULL

#' Computes smoothed states by means of the augmented Kalman filter in the case
#' of diffuse initialization
#'
#' @param ssf A state space form.
#' @param data Arrays of data.
#' @param all True if the covariances of the smoothed states are compted.
#' @param qtype Type of the initialization in the augmented Kalman filter
#'
#' @returns A matrix with the smoothed states and - if requested - their standard deviations.
#' @export
#'
#' @examples
#'
.ssf_smooth<-function(ssf, data, all=TRUE, qtype=c("NORMAL", "PARTIAL_TRIANGULARIZATION", "FULL_TRIANGULARIZATION", "QR")){
    qtype<-match.arg(qtype)
    jrslt<-.jcall("jdplus/sts/base.r/Algorithms", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "smooth", ssf$internal, as.numeric(data), as.logical(all), qtype)
    return (rjd3toolkit::.jd2r_matrix(jrslt))
}

#' Computes the diffuse likelihood by means of the augmented Kalman filter.
#'
#' @param ssf A state space form.
#' @param data Arrays of data.
#' @param rescalingFactor True if the innovation covariance matrix is defined up to a scaling factor.
#' @param qtype Type of the initialization in the augmented Kalman filter
#'
#' @returns The diffuse likelihood
#' @export
#'
#' @examples
#' sarima<-.sarima(12, NULL, 1,  -.6, NULL, 1, -.5)
#' ssf<-.ssf(sarima, .loading(0))
#' .akf_likelihood(ssf, rjd3toolkit::ABS$X0.2.09.10.M)
.akf_likelihood<-function(ssf, data, qtype=c("NORMAL", "PARTIAL_TRIANGULARIZATION", "FULL_TRIANGULARIZATION", "QR"), collapsing = TRUE, rescalingFactor=TRUE){
    jrslt<-.jcall(ssf$internal, "Ljdplus/toolkit/base/core/ssf/ISsfInitialization;", "initialization")
    ndiffuse<-.jcall(jrslt, "I", "getDiffuseDim")
    if (ndiffuse == 0) return (.dk_likelihood(ssf, data, FALSE, rescalingFactor))
    qtype<-match.arg(qtype)
    jrslt<-.jcall("jdplus/sts/base.r/Algorithms", "Ljdplus/toolkit/base/core/ssf/likelihood/DiffuseLikelihood;", "akfLikelihood", ssf$internal, as.numeric(data),
                  qtype, as.logical(collapsing), as.logical(rescalingFactor))
    return (.jd2r_diffuse_likelihood(jrslt))
}

#' Computes the diffuse likelihood by means of the diffuse Kalman filter (Durbin-Koopman).
#'
#' @param ssf A state space form.
#' @param data Arrays of data.
#' @param sqr Square root diffuse initialization, if any
#' @param rescalingFactor True if the innovation covariance matrix is defined up to a scaling factor.
#'
#' @returns The diffuse likelihood
#' @export
#'
#' @examples
#' sarima<-.sarima(12, NULL, 1,  -.6, NULL, 1, -.5)
#' ssf<-.ssf(sarima, .loading(0))
#' .dk_likelihood(ssf, rjd3toolkit::ABS$X0.2.09.10.M)
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
    return (.jd2r_likelihood(jrslt))
}
