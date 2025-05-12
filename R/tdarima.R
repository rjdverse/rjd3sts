#' @include utils.R
NULL


#' Title
#'
#' @param data
#' @param period
#' @param th
#' @param bth
#' @param se
#'
#' @returns
#' @export
#'
#' @examples
tdairline_decomposition<-function(data, parameters, se=FALSE){
    th0<-parameters[1]
    th1<-parameters[2]
    bth0<-parameters[3]
    bth1<-parameters[4]
    if (th0< -.98) th0=-.98
    if (th1< -.98) th1=-.98
    if (bth0< -.98) bth0=-.98
    if (bth1< -.98) bth1=-.98
    th=linear(length(data),th0, th1)
    bth=linear(length(data), bth0, bth1)
  if (! is.ts(data)) stop("data should be a time series (ts)")
  jmatrix<-.jcall('jdplus/advancedsa/base/r/TimeVaryingArimaModels', 'Ljdplus/toolkit/base/api/math/matrices/Matrix;', 'airlineDecomposition',
            as.numeric(data), as.integer(frequency(data)), as.numeric(th), as.numeric(bth), as.logical(se))
  return (rjd3toolkit::.jd2r_matrix(jmatrix))
}


#' Title
#'
#' @param s
#' @param td
#' @param vartd
#' @param precision
#'
#' @returns
#' @export
#'
#' @examples
tdairline_estimation<-function(s, td=NULL, vartd=FALSE, precision=1e-9){
  if (! is.ts(s)) stop("s should be a time series (ts)")
  arima<-sarima("arima", frequency(s), c(0,1,1), c(0,1,1), var = 1, fixedvariance = TRUE)
  if (! is.null(td)){
    if (vartd) tdvar<-1 else tdvar<-0
    vtd<-reg_td("td", frequency(s), start(s), length(s), groups=td, variance=tdvar, !vartd)
  }else
    vtd<-NULL
  model0<-model()
  rjd3sts::add(model0, arima)
  if (! is.null(vtd)){
    add(model0, vtd)
  }
  rslt0<-rjd3sts::estimate(model0, s, marginal = TRUE)
  parameters<-rjd3toolkit::result(rslt0, "parameters")
  likelihood<-rjd3toolkit::result(rslt0, "likelihood.ll")
  if (! is.null(td)){
    ss<-rjd3sts::smoothed_states(rslt0)
    spos<-rjd3toolkit::result(rslt0, "ssf.cmppos")
    d<-ss[,(1+spos[2]):dim(ss)[2]]
    #    d<--cbind(d,rowSums(d))
    ctd<-ts(d, frequency=frequency(s), start=start(s))
  }else
    ctd<-NULL
  arima_model<-list(
    parameters=parameters,
    likelihood=likelihood,
    td=ctd
  )
  ltdarima<-ltd_airline("ltd", length(s), frequency(s),
                                      th0 = parameters[2],th1=parameters[2],
                                      bth0 = parameters[3],bth1=parameters[3],
                                      variance = 1, fixedvariance = TRUE)
  model1<-model()
  rjd3sts::add(model1, ltdarima)
  if (! is.null(vtd)){
    add(model1, vtd)
  }
  rslt1<-estimate(model1, s, marginal = TRUE)


  parameters<-rjd3toolkit::result(rslt1, "parameters")
  likelihood<-rjd3toolkit::result(rslt1, "likelihood.ll")
  if (! is.null(td)){
    ss<-rjd3sts::smoothed_states(rslt1)
    spos<-rjd3toolkit::result(rslt1, "ssf.cmppos")
    d<-ss[,(1+spos[2]):dim(ss)[2]]
    #    d<--cbind(d,rowSums(d))
    ctd<-ts(d, frequency=frequency(s), start=start(s))
  }else
    ctd<-NULL
  ltd_arima_model<-list(
    parameters=parameters,
    likelihood=likelihood, td=ctd
  )
  return (list(sarima=arima_model, ltd_sarima=ltd_arima_model))
}

linear<-function(n,a,b){
  a+(0:(n-1))*((b-a)/(n-1))
}





