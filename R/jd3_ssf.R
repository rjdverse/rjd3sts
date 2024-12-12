#' @include utils.R
NULL

SSF<-'JD3_Ssf'
EQUATION<-'JD3_SsfEquation'
LOADING<-'JD3_SsfLoading'
MODELESTIMATION<-'JD3_SsfModelEstimation'
MODEL<-'JD3_SsfModel'
STATEBLOCK<-'JD3_SsfStateBlock'

#' Add a building block to the considered equation
#'
#' @param equation the equation
#' @param item the block of the state array that will be linked to the observation corresponding to this equation through the specified loading and coefficient
#' @param coeff the value of the coefficient associated to the block of latent variables defined by `item`.
#' @param fixed logical that triggers estimation of coeff (FALSE) or fixes it (TRUE) to a pre-specified value
#' @param loading the loading that links the block to the observation
#'
#' @return
#' @export
#'
#' @examples
#' model<- equation('eq1)
#' ll<-locallevel('ll')
#' n<-noise('n')
#' add_equation(model, ll)
#' add_equation(model, n)
add_equation<-function(equation, item, coeff=1, fixed=TRUE, loading=NULL){
  if (! is(equation, EQUATION))
    stop("Not an equation")
  if (is.null(loading))
    .jcall(equation$internal, "V", "add", item, coeff, as.logical(fixed), .jnull("jdplus/toolkit/base/core/ssf/ISsfLoading"))
  else if (is(loading, LOADING))
    .jcall(equation$internal, "V", "add", item, coeff, as.logical(fixed), loading$internal)
  else
    stop("Not a loading")
}


#' Title
#'
#' @param object
#' @param obs
#' @param pos
#' @param loading
#' @param stdev
#'
#' @return
#' @export
signal<-function(object, obs=1, pos=NULL, loading=NULL, stdev=FALSE){
  if (! is(object, MODELESTIMATION))
    stop("Not a model estimation")
  if (is.jnull(object$internal)){
    return(NULL)
  } else {
    if (! is.null(loading)){
      if (stdev){
        return(.jcall(object$internal, "[D", "stdevSignal", rjd3toolkit::.r2jd_matrix(loading)))
      } else {
        return(.jcall(object$internal, "[D", "signal", rjd3toolkit::.r2jd_matrix(loading)))
      }
    } else {
      if (is.null(pos))
        jpos<-.jnull("[I")
      else
        jpos<-.jarray(as.integer(pos-1))
      if (stdev){
        return(.jcall(object$internal, "[D", "stdevSignal", as.integer(obs-1), jpos))
      } else {
        return(.jcall(object$internal, "[D", "signal", as.integer(obs-1), jpos))
      }
    }
  }
}

#' Title
#'
#' @param object
#' @param m
#' @param pos
#' @param stdev
#'
#' @return
#' @export
#'
#' @examples
msignal<-function(object, m, pos=NULL, stdev=FALSE){
  if (! is(object, MODELESTIMATION))
    stop("Not a model estimation")
  if (is.jnull(object$internal)){
    return(NULL)
  }
  if (! is.matrix(m)){
    stop("Invalid matrix")
  } else {
    if (is.null(pos))
      jpos<-.jnull("[I")
    else {
      if (length(pos) != dim(m)[2])
        stop("Invalid input")
      jpos<-.jarray(as.integer(pos-1))
    }
    jm <- rjd3toolkit::.r2jd_matrix(m)
    if (stdev){
      return(.jcall(object$internal, "[D", "stdevSignal", jm, jpos))
    } else {
      return(.jcall(object$internal, "[D", "signal", jm, jpos))
    }
  }
}

#' Title
#'
#' @param object
#' @param obs
#'
#' @return
#' @export
#'
#' @examples
loading<-function(object, obs=1){
  if (! is(object, MODELESTIMATION))
    stop("Not a model estimation")
  if (is.jnull(object$internal)){
    return
  } else {
      jm<-.jcall(object$internal, "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "loading", as.integer(obs-1))
      return(rjd3toolkit::.jd2r_matrix(jm))
  }
}

#' Title
#'
#' @param model
#' @param item
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom methods is
add<-function(model, item){
  if (! is(model, MODEL))
    stop("Not a model")
  if (is.jnull(model$internal))
    return(NULL)
  if (is(item, EQUATION) || is(item, STATEBLOCK)){
    if (! is.jnull(item$internal))
      .jcall(model$internal, "V", "add", item$internal)
  } else {
    stop("Invalid item")
  }
}

#' Estimate a SSF Model
#'
#' @param model the model
#' @param data a matrix containing the data (one time series per column, time series dimension on the rows)
#' @param marginal logical value used to specify whether the marginal likelihood definition is used (TRUE) or
#' not (FALSE) during the optimization. The marginal likelihood is recommended when there is at least one variable that loads
#' on a non-stationary latent variable and the loading coefficient needs to be estimated.
#' @param concentrated logical value used to specify whether the likelihood is concentrated (TRUE) or not (FALSE) during the optimization
#' @param initialization initialization method.
#' @param optimizer
#' @param precision indicating the largest likelihood deviations that make the algorithm stop.
#' @param initialParameters
#'
#' @return
#' @export
#'
#' @examples
estimate<-function(model, data, marginal=FALSE, concentrated=TRUE,
              initialization=c("Augmented_Robust", "Diffuse", "SqrtDiffuse", "Augmented", "Augmented_NoCollapsing"), optimizer=c("LevenbergMarquardt", "MinPack", "BFGS", "LBFGS"), precision=1e-15, initialParameters=NULL){
  initialization <- match.arg(initialization)
  optimizer <- match.arg(optimizer)
  if (! is(model, MODEL))
    stop("Not a model")
  if (is.jnull(model$internal)){
    return(NULL)
  } else {
    jparams<-.jnull("[D")
    if (! is.null(initialParameters))
      jparams<-.jarray(initialParameters)
    jdata<-rjd3toolkit::.r2jd_matrix(data)
    jrslt<-.jcall("jdplus/sts/base/r/CompositeModels", "Ljdplus/sts/base/r/CompositeModels$Results;", "estimate",model$internal, jdata, marginal, concentrated, initialization, optimizer, precision, jparams)
    return(rjd3toolkit::.jd3_object(jrslt, MODELESTIMATION, TRUE))
  }
}

compute<-function(model, data, parameters, marginal=FALSE, concentrated=TRUE){
  if (! is(model, MODEL))
    stop("Not a model")
  if (is.jnull(model$internal)){
    return(NULL)
  } else {
    jdata<-rjd3toolkit::.r2jd_matrix(data)
    jrslt<-.jcall("jdplus/sts/base/r/CompositeModels", "Ljdplus/sts/base/r/CompositeModels$Results;", "compute", model$internal, jdata, .jarray(parameters), marginal, concentrated)
    return(rjd3toolkit::.jd3_object(jrslt, MODELESTIMATION, TRUE))
  }
}



#' Autoregressive model
#'
#' Functions to create an autoregressive model (`ar`) or a
#' modified autoregressive model (`ar2`)
#'
#' @param ar vector of the AR coefficients (\eqn{\varphi_1, \dots, \varphi_p}).
#' @param fixedar boolean that triggers the estimation of the AR coefficients (`FALSE`)
#' or fixed it (`TRUE`) to a pre-specified value set by the parameter `ar`.
#' @param variance the variance (\eqn{\sigma^2_{ar}}).
#' @param fixedvariance boolean that triggers the estimation of the variance (`FALSE`)
#' or fixed it (`TRUE`) to a pre-specified value set by the parameter `variance`.
#' @param nlags integer specifying how many lags of the state variable are needed
#' @param zeroinit boolean determining the initial condition for the state variable,
#' which is equal to zero if `zeroinit = TRUE`.
#' The default (`zeroinit = FAKSE`) triggers the an initialization based on the
#' unconditional mean and variance of the AR(p) process.
#'
#' @details
#' The AR process is defined by
#' \deqn{\Phi\left(B\right)y_t=\epsilon_t}
#' where
#' \deqn{\Phi\left(B\right)=1+\varphi_1 B + \cdots + \varphi_p B^p}
#' is an auto-regressive polynomial.
#' @return
#'
#' @export
#'
#' @examples
ar<-function(name, ar, fixedar=FALSE, variance=.01, fixedvariance=FALSE, nlags=0, zeroinit=FALSE){

  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "ar", name, .jarray(ar), fixedar, variance, fixedvariance, as.integer(nlags), zeroinit)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' @rdname ar
#' @param nfcasts integer specifying how many forecasts of the state variable are needed
#' @export
ar2<-function(name, ar, fixedar=FALSE, variance=.01, fixedvariance=FALSE, nlags=0, nfcasts=0){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "ar", name, .jarray(ar), fixedar, variance, fixedvariance, as.integer(nlags), as.integer(nfcasts))
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Title
#'
#' @param name
#' @param factor
#' @param period
#' @param fixed
#' @param variance
#' @param fixedvariance
#'
#' @return
#' @export
#'
#' @examples
cycle<-function(name, factor=.9, period=60, fixed=FALSE, variance=.01, fixedvariance=FALSE){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "cycle", name, factor, period, fixed, variance, fixedvariance)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Title
#'
#' @param name
#' @param period
#' @param harmonics
#' @param variance
#' @param fixedvariance
#'
#' @return
#' @export
#'
#' @examples
periodic<-function(name, period, harmonics, variance=.01, fixedvariance=FALSE){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "periodicComponent", name, period, .jarray(as.integer(harmonics)), variance, fixedvariance)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}


#' Title
#'
#' @param name
#' @param ar
#' @param fixedar
#' @param lag
#' @param zeroinit
#'
#' @return
#' @export
#'
#' @examples
sae<-function(name, ar, fixedar=FALSE, lag=1, zeroinit=FALSE){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "sae", name, .jarray(ar), fixedar, as.integer(lag), zeroinit)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Modeling errors in surveys with overlapping panels
#'
#' @inheritParams locallevel
#' @param nwaves integer representing the number of waves
#' @param ar matrix representing the covariance structure of the wave specific survey error.
#' @param fixedar logical that triggers the estimation of the correlation patterns (`TRUE`) or
#' fixes them to the values given by the entries `ar` (`FALSE`)
#' @param lag integer specifying the number of time periods (in the base frequency) that compose the survey period.
#' This coincides with the number of time periods an individual has to wait between two different waves.
#' Note that if the survey period is one quarter, all of them have already responded in the previous wave exactly 3 months ago
#' (because individuals are always interviewed at the same stint during each survey period).
#'
#' @return
#' @export
#'
#' @examples
msae<-function(name, nwaves, ar, fixedar=TRUE, lag=1){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "waveSpecificSurveyError", name, as.integer(nwaves), rjd3toolkit::.r2jd_matrix(ar), fixedar, as.integer(lag))
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' @rdname msae
#' @export
msae2<-function(name, vars, fixedvars=FALSE, ar, fixedar=TRUE, lag=1){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "waveSpecificSurveyError", name, vars, fixedvars, rjd3toolkit::.r2jd_matrix(ar), fixedar, as.integer(lag))
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' @param k
#' @rdname msae
#' @export
msae3<-function(name, vars, fixedvars=FALSE, ar, fixedar=TRUE, k, lag=1){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "waveSpecificSurveyError", name, vars, fixedvars, .jarray(ar), fixedar, rjd3toolkit::.r2jd_matrix(k), as.integer(lag))
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Local Level
#'
#' @param name name of the component.
#' @param variance the value of the variance (\eqn{\sigma^2_l}).
#' @param fixed boolean that triggers estimation of \eqn{\sigma^2_l} (`FALSE`) or
#' fixes it (`TRUE`) to a pre-specified  value set by the parameter `variance`.
#' @param initial initial value of the level (\eqn{l_0}).
#'
#' @details
#' \deqn{\begin{cases}l_{t+1} = l_t + \mu_t \\
#'  \mu_t \sim N(0, \sigma^2 \sigma^2_l)
#'  \end{cases}
#' }
#'
#' @return
#' @export
#'
#' @examples
locallevel<-function(name, variance=.01, fixed=FALSE, initial=NaN){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "localLevel", name, variance, fixed, initial)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}


#' Local Linear Trend
#'
#' @inheritParams locallevel
#' @param levelVariance variance of the level (\eqn{\sigma^2_l})
#' @param slopevariance variance of the slope (\eqn{\sigma^2_n$)
#' @param fixedLevelVariance,fixedSlopeVariance boolean that triggers
#' the estimation of the variances \eqn{\sigma^2_l} and \eqn{\sigma^2_n} (`FALSE`) or
#' fixes it (`TRUE`) to a pre-specified value set by the parameters `levelVariance` and `slopevariance`.
#' @details
#'
#' \deqn{\begin{cases}l_{t+1} = l_t + n_t +  \xi_t \\
#'  n_{t+1} = n_t + \mu_t \\
#'  \xi_t \sim N(0, \sigma^2\sigma^2_l)\\
#'  \mu_t \sim N(0, \sigma^2\sigma^2_n)
#'  \end{cases}}
#' @return
#' @export
#'
#' @examples
locallineartrend<-function(name, levelVariance=.01, slopevariance=.01, fixedLevelVariance=FALSE, fixedSlopeVariance=FALSE){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "localLinearTrend", name, levelVariance, slopevariance, fixedLevelVariance, fixedSlopeVariance)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Title
#'
#' @param name
#' @param period
#' @param type
#' @param variance
#' @param fixed
#'
#' @return
#' @export
#'
#' @examples
seasonal<-function(name, period, type=c("Trigonometric", "Crude", "HarrisonStevens", "Dummy"), variance=.01, fixed=FALSE){
  type <- match.arg(type)
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "seasonalComponent", name, type, as.integer(period), variance, fixed)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Noise component
#'
#' @param name
#' @param variance
#' @param fixed
#'
#' @return
#' @export
#'
#' @examples
noise<-function(name, variance=.01, fixed=FALSE){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "noise", name, variance, fixed)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Title
#'
#' @param name
#' @param std
#' @param scale
#' @param fixed
#'
#' @return
#' @export
#'
#' @examples
var_noise<-function(name, std, scale=1, fixed=FALSE){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "noise", name, as.double(std), scale, fixed)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Title
#'
#' @param name
#' @param std
#' @param scale
#' @param fixed
#' @param initial
#'
#' @return
#' @export
#'
#' @examples
var_locallevel<-function(name, std, scale=1, fixed=FALSE, initial=NaN){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "localLevel", name, as.double(std), scale, fixed, initial)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Title
#'
#' @param name
#' @param lstd
#' @param sstd
#' @param levelScale
#' @param slopeScale
#' @param fixedLevelScale
#' @param fixedSlopeScale
#'
#' @return
#' @export
#'
#' @examples
var_locallineartrend<-function(name, lstd, sstd=NULL, levelScale=1, slopeScale=1, fixedLevelScale=FALSE, fixedSlopeScale=FALSE){

  if (is.null(sstd)){
    jsstd<-.jnull("[D")
  } else {
    jsstd<-.jarray(sstd)
  }
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "localLinearTrend", name, lstd, jsstd, levelScale, slopeScale,
                fixedLevelScale, fixedSlopeScale)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Title
#'
#' @param name
#' @param period
#' @param type
#' @param std
#' @param scale
#' @param fixed
#'
#' @return
#' @export
#'
#' @examples
var_seasonal<-function(name, period, type=c("Trigonometric", "Crude", "HarrisonStevens", "Dummy"), std, scale=1, fixed=FALSE){

  type <- match.arg(type)
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "seasonalComponent", name, type, as.integer(period), as.double(std), scale, fixed)


  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Create Composite Model
#'
#' @return
#' @export
#'
#' @examples
model<-function(){
  jrslt<-.jnew("jdplus/sts/base/core/msts/CompositeModel")
  return(rjd3toolkit::.jd3_object(jrslt, MODEL))
}

#' Create equation
#'
#' @param name
#' @param variance
#' @param fixed
#'
#' @return
#' @export
#'
#' @examples
equation<-function(name, variance=0, fixed=TRUE){
  jrslt<-.jnew("jdplus/sts/base/core/msts/ModelEquation", name, variance, fixed)
  return(rjd3toolkit::.jd3_object(jrslt, EQUATION))
}

#' Title
#'
#' @param pos defines the position of each one of the elements of the block of states defined.
#' `NULL` indicates by default the first state included in the block (pos=0)
#' @param weights defines the weights associated to each one of the state variables included in the block.
#'
#' @return
#' @export
#'
#' @examples
loading<-function(pos=NULL, weights=NULL){
  if (is.null(pos)){
    jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "fromPosition", as.integer(0))
    return(rjd3toolkit::.jd3_object(jrslt, LOADING))
  }
  else if (length(pos) == 1){
    if (is.null(weights)){
      jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "fromPosition", as.integer(pos))
    } else {
      if (length(pos) != length(weights)){
        return(NULL)
      }
      jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "from", as.integer(pos), weights[1])
    }
    return(rjd3toolkit::.jd3_object(jrslt, LOADING))
  } else {
    if (is.null(weights))
      jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "fromPositions", as.integer(pos))
    else {
      if (length(pos) != length(weights))
        return(NULL)
      jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "from", as.integer(pos), weights)
    }
    return(rjd3toolkit::.jd3_object(jrslt, LOADING))
  }
}

#' Title
#'
#' @param pos
#' @param weights
#'
#' @return
#' @export
#'
#' @examples
var_loading<-function(pos, weights){
  if (is.null(pos)){
    jl<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "fromPosition", as.integer(0))
  }
  else if (length(pos) == 1){
    jl<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "fromPosition", as.integer(pos))
  } else {
    jl<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "fromPositions", as.integer(pos))
  }
  jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "rescale", jl, as.numeric(weights))
  return(rjd3toolkit::.jd3_object(jrslt, LOADING))
}

#' Title
#'
#' @param length
#'
#' @return
#' @export
#'
#' @examples
loading_sum<-function(length=0){
  if (length == 0)
    jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "sum")
  else
    jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "createPartialSum", as.integer(length))
  return(rjd3toolkit::.jd3_object(jrslt, LOADING))
}

#' Title
#'
#' @param period
#' @param startpos
#'
#' @return
#' @export
#'
#' @examples
loading_cyclical<-function(period, startpos){
  jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "cyclical", as.integer(period), as.integer(startpos-1))
  return(rjd3toolkit::.jd3_object(jrslt, LOADING))
}

#' Title
#'
#' @param period
#' @param startpos
#'
#' @return
#' @export
#'
#' @examples
loading_periodic<-function(period, startpos){
  jrslt<-.jcall("jdplus/toolkit/base/core/ssf/basic/Loading", "Ljdplus/toolkit/base/core/ssf/ISsfLoading;", "periodic", as.integer(period), as.integer(startpos-1))
  return(rjd3toolkit::.jd3_object(jrslt, LOADING))
}

#' Title
#'
#' @param initialization
#' @param dynamics
#' @param measurement
#'
#' @return
#' @export
#'
#' @examples
ssf<-function(initialization, dynamics, measurement){
  jrslt<-.jcall("rssf/Ssf", "Ljdplus/toolkit/base/core/ssf/univariate/Issf;", "of", initialization$internal, dynamics$internal, measurement$internal)
  return(rjd3toolkit::.jd3_object(jrslt, SSF, TRUE))
}

#' Autoregressive Moving Average (ARMA) Model
#'
#' @param name
#' @param ar
#' @param fixedar
#' @param ma
#' @param fixedma
#' @param var
#' @param fixedvar
#'
#' @return
#' @export
#'
#' @examples
arma<-function(name, ar, fixedar=FALSE, ma, fixedma=FALSE, var=1, fixedvar =FALSE){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "arma", name, as.double(ar), fixedar,
                as.double(ma), fixedma, var, fixedvar)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Autoregressive Integrated Moving Average (ARIMA) Model
#'
#' @param name
#' @param ar
#' @param diff
#' @param ma
#' @param var
#' @param fixed
#'
#' @return
#' @export
#'
#' @examples
arima<-function(name, ar, diff, ma, var=1, fixed =FALSE){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "arima", name, as.double(ar), as.double(diff), as.double(ma), var, fixed)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Title
#'
#' @param name
#' @param period
#' @param orders
#' @param seasonal
#' @param parameters
#' @param fixedparameters
#' @param var
#' @param fixedvariance
#'
#' @return
#' @export
#'
#' @examples
sarima<-function(name, period, orders, seasonal, parameters=NULL, fixedparameters=FALSE, var=1, fixedvariance =FALSE){
  if (is.null(parameters))
    jp<-.jnull("[D")
  else
    jp<-.jarray(parameters)
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "sarima", name, as.integer(period), as.integer(orders), as.integer(seasonal), jp, fixedparameters, var, fixedvariance)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Title
#'
#' @param name
#' @param core
#' @param period
#' @param start
#'
#' @return
#' @export
#'
#' @examples
cumul<-function(name, core, period, start=0){
  jrslt<-.jcall("jdplus/sts/base/core/msts/DerivedModels", "Ljdplus/sts/base/core/msts/StateItem;", "cumulator", name, core$internal
                , as.integer(period), as.integer(start))
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Title
#'
#' @param name
#' @param components
#'
#' @return
#' @export
#'
#' @examples
aggregation<-function(name, components){
  if (!is.list(components) || length(components)<2) {
    stop("incorrect argument, components should be a list of at least 2 items")}
  plist<-list()
  for (i in seq_along(components)){
    plist[[i]]<-components[[i]]$internal
  }
  jcmps<-.jarray(plist, contents.class = "jdplus/sts/base/core/msts/StateItem")
  jrslt<-.jcall("jdplus/sts/base/core/msts/DerivedModels", "Ljdplus/sts/base/core/msts/StateItem;", "aggregation", name, jcmps)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Time Varying Regressors
#'
#' @param name
#' @param x matrix containing the regressors
#' @param var
#' @param fixed
#'
#' @return
#' @export
#'
#' @examples
reg<-function(name, x, var=NULL, fixed=FALSE){

  if (is.null(var)){
    jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "regression", name, rjd3toolkit::.r2jd_matrix(x))
  } else {
    jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "timeVaryingRegression", name, rjd3toolkit::.r2jd_matrix(x), as.numeric(var), fixed)
  }
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Time Varying Regressor
#'
#' @param name
#' @param x Regression variable. Numerics
#' @param stderr Standard error of the innovations of the coefficient (1 in extrapolation)
#' @param scale Scaling factor
#' @param fixed Fixed scaling factor
#'
#' @return
#' @export
#'
#' @examples
#'  x<-rjd3toolkit::retail$BookStores
#'  std<-rep(1, length(x))
#'  std[c(20, 50, 150)]<-5
#'  v<-var_reg("vx", x, std, 0.1)
var_reg<-function(name, x, stderr, scale=1, fixed=FALSE){

   jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "timeVaryingRegression", name
                 , as.numeric(x), as.numeric(stderr), as.numeric(scale), fixed)
   return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Title
#'
#' @param name
#' @param period
#' @param start
#' @param length
#' @param groups
#' @param contrast
#' @param variance
#' @param fixed
#'
#' @return
#' @export
#'
#' @examples
reg_td<-function(name, period, start, length, groups=c(1,2,3,4,5,6,0), contrast=TRUE, variance=1, fixed=FALSE){
  .jdomain<-rjd3toolkit::.r2jd_tsdomain(period, startYear = start[1], startPeriod = start[2], length = length)
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "tdRegression", name, .jdomain, as.integer(groups), contrast, variance, fixed)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Title
#'
#' @param name
#' @param period
#' @param nnodes
#' @param nodes
#' @param start
#' @param variance
#' @param fixed
#'
#' @return
#' @export
#'
#' @examples
splines_regular<-function(name, period, nnodes=0, nodes=NULL, start=1, variance=1, fixed=FALSE){
  if (is.null(nodes)){
    if (nnodes == 0)
      stop('Invalid parameters. nnodes should be greater than 0 or nodes should be defined')
    jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "regularSplines",
                  name, period, as.integer(nnodes), as.integer(start-1), variance, fixed)
  } else {
    jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "regularSplines",
                  name, period, as.numeric(nodes), as.integer(start-1), variance, fixed)
  }
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}


#' Title
#'
#' @param name
#' @param startYear
#' @param nodes
#' @param start
#' @param variance
#' @param fixed
#'
#' @return
#' @export
#'
#' @examples
splines_daily<-function(name, startYear, nodes, start=1, variance=1, fixed=FALSE){
  jrslt<-.jcall("jdplus/sts/base/core/msts/AtomicModels", "Ljdplus/sts/base/core/msts/StateItem;", "dailySplines",
                  name, as.integer(startYear), as.integer(nodes-1), as.integer(start-1), variance, fixed)
  return(rjd3toolkit::.jd3_object(jrslt, STATEBLOCK))
}

#' Title
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
smoothed_states<-function(model){
  if (! is(model, MODELESTIMATION))
    stop("Not a model")
  if (is.jnull(model$internal)){
    return(NULL)
  }
  return(rjd3toolkit::result(model, "ssf.smoothing.states"))
}

#' Retrieves the components of the model (univariate case) or the
#' components corresponding to a given equation (multivariate case)
#'
#' @param model Estimated state space model
#' @param equation Equation containing the components
#' @param fast if true, only the components are computed. Otherwise, their stdev
#'  are also computed (not returned but available for future use).
#' @return A matrix with the components
#' @export
#'
#' @examples
smoothed_components<-function(model, equation=1, fast=TRUE){
  if (! is(model, MODELESTIMATION))
    stop("Not a model")
  if (is.jnull(model$internal)){
    return(NULL)
  }
  if (fast)
    return(rjd3toolkit::result(model,paste0("ssf.smoothing.fastcomponents(",equation-1,')')))
  else
    return(rjd3toolkit::result(model,paste0("ssf.smoothing.components(",equation-1,')')))
}

#' Retrieves the stdev of the components of the model (univariate case) or of the
#' components corresponding to a given equation (multivariate case)
#'
#' @param model Estimated state space model
#' @param equation Equation containing the components
#'
#' @return A matrix with the stdev of the components
#' @export
#'
#' @examples
smoothed_components_stdev<-function(model, equation=1){
  if (! is(model, MODELESTIMATION))
    stop("Not a model")
  if (is.jnull(model$internal)){
    return(NULL)
  }
  return(sqrt(rjd3toolkit::result(model,paste0("ssf.smoothing.vcomponents(",equation-1,')'))))
}



#' Title
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
smoothed_states_stdev<-function(model){
  if (! is(model, MODELESTIMATION))
    stop("Not a model")
  if (is.jnull(model$internal)){
    return(NULL)
  }
  return(sqrt(rjd3toolkit::result(model, "ssf.smoothing.vstates")))
}

#' Title
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
filtered_states<-function(model){
  if (! is(model, MODELESTIMATION))
    stop("Not a model")
  if (is.jnull(model$internal)){
    return(NULL)
  }
  return(rjd3toolkit::result(model, "ssf.filtered.states"))
}

#' Title
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
filtered_states_stdev<-function(model){
  if (! is(model, MODELESTIMATION))
    stop("Not a model")
  if (is.jnull(model$internal)){
    return(NULL)
  }
  return(sqrt(rjd3toolkit::result(model, "ssf.filtered.vstates")))
}

#' Title
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
filtering_states_stdev<-function(model){
  if (! is(model, MODELESTIMATION))
    stop("Not a model")
  if (is.jnull(model$internal)){
    return(NULL)
  }
  return(sqrt(rjd3toolkit::result(model, "ssf.filtering.vstates")))
}

#' Title
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
filtering_states<-function(model){
  if (! is(model, MODELESTIMATION))
    stop("Not a model")
  if (is.jnull(model$internal)){
    return(NULL)
  }
  return(rjd3toolkit::result(model, "ssf.filtering.states"))
}

#' Get Parameters of SSF Model
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
parameters<-function(model){
  if (! is(model, MODELESTIMATION))
    stop("Not a model")
  if (is.jnull(model$internal)){
    return(NULL)
  }
  res <- rjd3toolkit::result(model, "parameters")
  names(res) <- rjd3toolkit::result(model, "parametersnames")
  return()
}
