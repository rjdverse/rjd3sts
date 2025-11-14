#' @include utils.R jd3_ssf.R
NULL

JD3_BSM = "JD3_BasicStructuralModel"

#' Title
#'
#' @param period
#' @param noise.var
#' @param level.var
#' @param slope.var
#' @param seasonal.var
#' @param seasonal.model
#' @param cycle.var
#' @param cycle.dumpingfactor
#' @param cycle.length
#'
#' @returns
#' @export
#'
#' @examples
bsm_model<-function(period, noise.var=1, level.var=1, slope.var=NULL, seasonal.var=1, seasonal.model=c("HarrisonStevens", "Trigonometric", "Dummy", "Crude", "Fixed")
                     , cycle.var=NULL, cycle.dumpingfactor=.9, cycle.length=6){
    seasonal.model<-match.arg(seasonal.model)

    if (is.null(seasonal.var))
        seasonal=NULL
    else
        seasonal = list(var=seasonal.var, model=seasonal.model)
    if (is.null(cycle.var))
        cycle=NULL
    else
        cycle=list(var=cycle.var, dumping_factor=dumpingfactor, length=cyccle.length)
    return (structure(
        list(
            period=period,
            noise=noise.var,
            level=level.var,
            slope=slope.var,
            seasonal=seasonal,
            cycle=cycle
        )
        , class = JD3_BSM))
}

#' Title
#'
#' @param bsm
#'
#' @returns
#' @export
#'
#' @examples
.r2jd_bsm<-function(bsm){
    if (! inherits(bsm, JD3_BSM))
        stop('Invalid model')

    if (is.null(bsm$noise)){nv<--1} else {nv<-as.numeric(bsm$noise)}
    if (is.null(bsm$level)){lv<--1} else {lv<-as.numeric(bsm$level)}
    if (is.null(bsm$slope)){sv<--1} else {sv<-as.numeric(bsm$slope)}
    if (is.null(bsm$seasonal)){seasv=-1; seasm="HarrisonStevens"} else{seasv<-as.numeric(bsm$seasonal$var); seasm<-bsm$seasonal$model}
    if (is.null(bsm$cycle)){cv<--1; cdump=0.9; clength<-6} else{
        cv<-as.numeric(bsm$cycle$var);cdum<-as.numeric(bsm$cycle$dumping_factor);clength<-as.numeric(bsm$cycle$length)}

    return (.jcall("jdplus/sts/base/r/Bsm", "Ljdplus/sts/base/core/BsmData;", "bsm",
        as.integer(bsm$period), nv, lv, sv, seasv, seasm, cv, cdump, clength))
}


#' Title
#'
#' @param jbsm
#' @param normalized
#'
#' @returns
#' @export
#'
#' @examples
#' bsm<-bsm_model(12)
#' jbsm<-.r2jd_bsm(bsm)
#' jucm<-.bsm2ucm(jbsm)
.bsm2ucm<-function(jbsm, normalized=TRUE){
    return (.jcall("jdplus/sts/base/core/BsmUtility", "Ljdplus/toolkit/base/core/ucarima/UcarimaModel;", "ucm", jbsm, as.logical(normalized)))
}

#' Title
#'
#' @param jbsm
#' @param fixed
#' @param fixedCycle
#'
#' @returns
#' @export
#'
#' @examples
#' bsm<-bsm_model(12)
#' jbsm<-.r2jd_bsm(bsm)
#' jspec<-.bsm2spec(jbsm)
.bsm2spec<-function(jbsm, fixed=FALSE, fixedCycle=TRUE){
    return (.jcall("jdplus/sts/base/r/Bsm", "Ljdplus/sts/base/api/BsmSpec;", "specOf", jbsm, as.logical(fixed), as.logical(fixedCycle)))
}


#' Title
#'
#' @param bsm
#' @param normalized
#'
#' @returns
#' @export
#'
#' @examples
bsm_to_ucm<-function(bsm, normalized=TRUE){
    jbsm<-.r2jd_bsm(bsm)
    jucm<-.bsm2ucm(jbsm, normalized = normalized)
    return (rjd3toolkit::.jd2r_ucarima(jucm))
}


#' Title
#'
#' @inheritParams seasonalbreaks
#' @param cycle
#' @param diffuse.regs
#' @param tol
#'
#' @return
#' @export
#'
#' @examples
#'  x<-rjd3toolkit::Retail$BookStores
#'  sts(x)
sts<-function(y, X=NULL, X.td=NULL, level=1, slope=1, cycle=-1, noise=1
              , seasonal=c("Trigonometric", "Dummy", "Crude", "HarrisonStevens", "Fixed", "Unused"), diffuse.regs=TRUE, tol=1e-9){

  if (!is.ts(y)){
    stop("y must be a time series")
  }
  seasonal<-match.arg(seasonal)
  if (! is.null(X.td)){
    td<-rjd3toolkit::td(s = y, groups = X.td)
    X<-cbind(X, td)
  }
  jts<-rjd3toolkit::.r2jd_tsdata(y)
  jx<-rjd3toolkit::.r2jd_matrix(X)
  jsts<-.jcall("jdplus/sts/base/r/Bsm", "Ljdplus/sts/base/core/LightBasicStructuralModel;", "process", jts, jx,
              as.integer(level), as.integer(slope), as.integer(cycle), as.integer(noise), seasonal, as.logical(diffuse.regs), tol)
  buffer<-.jcall("jdplus/sts/base/r/Bsm", "[B", "toBuffer", jsts)
  p<-RProtoBuf::read(sts.Bsm, buffer)
  return(p2r_sts_rslts(p))
}

#' Title
#'
#' @param y
#' @param period
#' @param X
#' @param X.td
#' @param level
#' @param slope
#' @param cycle
#' @param noise
#' @param seasonal
#' @param diffuse.regs
#' @param tol
#'
#' @return
#' @export
#'
#' @examples
#'
sts_raw<-function(y, period=NA, X=NULL, X.td=NULL, level=1, slope=1, cycle=-1, noise=1
                  , seasonal=c("Trigonometric", "Dummy", "Crude", "HarrisonStevens", "Fixed", "Unused"), diffuse.regs=TRUE, tol=1e-9){

  data<-as.numeric(y)
  if (is.ts(y)){
    period<-frequency(y)
  } else {
    if (! is.null(X.td)){
      stop("y must be a time series when X.td is used")
    }
    if (is.na(period)){
      stop("y must be a time series or period must be specified")
    }
  }
  seasonal<-match.arg(seasonal)
  if (! is.null(X.td)){
    td<-rjd3toolkit::td(s = y, groups = X.td)
    X<-cbind(X, td)
  }
  bsm<-model()
  # create the components and add them to the model
  add(bsm, locallineartrend("ll"))
  add(bsm, seasonal("s", period, type=seasonal))
  add(bsm, noise("n"))
  if (! is.null(X)){
    add(bsm, reg("X", X))
  }
  # create the equation
  eq<-equation("eq")
  add_equation(eq, "ll")
  add_equation(eq, "s")
  add_equation(eq, "n")
  if (! is.null(X)){
    add_equation(eq, "X")
  }
  add(bsm, eq)
  #estimate the model
  rslt<-estimate(bsm, data, marginal=TRUE, concentrated=TRUE, precision = 1e-9)
  return(rslt)
}



#' Forecast with STS model
#'
#' @param y Series
#' @param model Model for calendar effects
#' \itemize{
#'   \item{td2: }{leap year + week days (week-end derived)}
#'   \item{td3: }{leap year + week days + saturdays (sundays derived)}
#'   \item{td7: }{leap year + all days (sundays derived)}
#'   \item{full: }{td3 + easter effect}
#'   \item{none: }{no calendar effect}
#'   }
#' @param nf number of forecasts
#'
#' @return
#' @export
#'
#' @examples
#' fcasts<-sts_forecast(rjd3toolkit::ABS$X0.2.09.10.M)
sts_forecast<-function(y, model=c("none", "td2", "td3", "td7", "full"), nf=12){
  model<-match.arg(model)
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  jf<-.jcall("jdplus/sts/base/r/Bsm", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "forecast", rjd3toolkit::.r2jd_tsdata(y), model, as.integer((nf)))
  return(rjd3toolkit::.jd2r_matrix(jf))

}

p2r_sts_rslts<-function(p){

  return(structure(list(
    description=p2r_sts_description(p$description),
    estimation=p2r_sts_estimation(p$estimation),
    decomposition=p2r_sts_components(p$components)),
    class="JD3STS")
  )
}

p2r_sts_estimation<-function(p){
  return(list(
    y=p$y,
    X=rjd3toolkit::.p2r_matrix(p$x),
    parameters=rjd3toolkit::.p2r_parameters_estimation(p$parameters),
    b=p$b,
    bvar=rjd3toolkit::.p2r_matrix(p$bcovariance),
    likelihood=.p2r_likelihood(p$likelihood),
    res=p$residuals))
}

p2r_sts_description<-function(p){
  return(list(
    log=p$log,
    preadjustment = rjd3toolkit::.enum_extract(modelling.LengthOfPeriod, p$preadjustment),
    bsm=p2r_spec_bsm(p$bsm),
    variables=rjd3toolkit::.p2r_variables(p$variables)))
}

p2r_sts_components<-function(p){
  return(list(
    level=p2r_sts_component(p$level),
    slope=p2r_sts_component(p$slope),
    cycle=p2r_sts_component(p$cycle),
    seasonal=p2r_sts_component(p$seasonal),
    noise=p2r_sts_component(p$noise)
  ))
}

p2r_sts_component<-function(p){
  if (is.null(p)) return(NULL) else return(p$as.list())
}




p2r_spec_bsm<-function(p){
  return(list(
    level=rjd3toolkit::.p2r_parameter(p$level),
    slope=rjd3toolkit::.p2r_parameter(p$slope),
    seas=rjd3toolkit::.p2r_parameter(p$seas),
    seasmodel=rjd3toolkit::.enum_extract(sts.SeasonalModel, p$seasonal_model),
    noise=rjd3toolkit::.p2r_parameter(p$noise),
    cycle=rjd3toolkit::.p2r_parameter(p$cycle),
    cyclelength=rjd3toolkit::.p2r_parameter(p$cycle_period),
    cyclefactor=rjd3toolkit::.p2r_parameter(p$cycle_factor)
  ))

}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.JD3STS<-function(x, ...){
  cat("Structural time series", "\n\n")
  cat("Variances:\n")
  s<-x$description$bsm$level
  if (! is.null(s)) cat("level: ", format(round(s$value, 6), scientific = FALSE), "\n")
  s<-x$description$bsm$slope
  if (! is.null(s)) cat("slope: ", format(round(s$value, 6), scientific = FALSE), "\n")
  s<-x$description$bsm$seas
  if (! is.null(s)) cat("seasonal: ", format(round(s$value, 6), scientific = FALSE), "\n")
  s<-x$description$bsm$noise
  if (! is.null(s)) cat("noise: ", format(round(s$value, 6), scientific = FALSE), "\n\n")
  s<-x$description$bsm$cycle
  if (! is.null(s)) {
    cat("cycle: ", format(round(s$value, 6), scientific = FALSE), "\n\n")
  }

  s<-x$estimation$likelihood$ll
  cat("LogLikelihood: ", format(round(s, 5), scientific = FALSE), "\n")
  s<-x$estimation$likelihood$aic
  cat("AIC: ", format(round(s, 5), scientific = FALSE), "\n\n")

  if (length(x$description$variables) > 0){
    cat("Regression:\n")
    regs<-do.call("rbind", lapply(x$description$variables, function(z){z$coeff}))
    xregs<-cbind(regs, stde=NA, t=NA, pvalue=NA)
    stde<-sqrt(diag(x$estimation$bvar))
    sel<-xregs$type=='ESTIMATED'
    t<-xregs$value[sel]/stde
    ndf<-x$estimation$likelihood$nobs-x$estimation$likelihood$ndiffuse-x$estimation$likelihood$nparams+1
    pval<-2*pt(abs(t), ndf, lower.tail = FALSE)
    xregs$stde[sel]<-stde
    xregs$t[sel]<-t
    xregs$pvalue[sel]<-pval
    print(xregs[-2])
  } else {
    cat("No regression variable\n")
  }
}
