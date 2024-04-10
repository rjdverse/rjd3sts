#' @include utils.R
NULL


#' Title
#'
#' @inheritParams seasonalbreaks
#' @param ao,ls,so boolean indicating if additive outliers (`ao`), level shift (`ls`) and seasonal
#' outliers (`so`) should be detected.
#' @param cv 
#' @param tcv 
#' @param estimation.forward 
#' @param estimation.backward 
#'
#' @return
#' @export
#'
#' @examples
#'  x<-rjd3toolkit::retail$BookStores
#'  sts_outliers(x)
sts_outliers<-function(y, period=NA, X=NULL, X.td=NULL, level=1, slope=1, noise=1, seasonal=c("Trigonometric", "Dummy", "Crude", "HarrisonStevens", "Fixed", "Unused"),
              ao=TRUE, ls=TRUE, so=FALSE, 
              cv=0, tcv=0, estimation.forward=c("Score", "Point", "Full"), 
              estimation.backward=c("Point", "Score", "Full")){
  
  data<-as.numeric(y)
  if (is.ts(y)){
    period<-frequency(y)
  } else{
    if (! is.null(X.td)){
      stop("y must be a time series when X.td is used")
    }
    if (is.na(period)){
      stop("y must be a time series or period must be specified")
    }
  }
  seasonal<-match.arg(seasonal)
  estimation.forward<-match.arg(estimation.forward)
  estimation.backward<-match.arg(estimation.backward)
  
  
  if (! is.null(X.td)){
    td<-rjd3toolkit::td(s = y, groups = X.td)
    X<-cbind(X, td)
  }
      
  
  jsts<-.jcall("jdplus/sts/base/r/StsOutliersDetection", "Ljdplus/sts/base/r/StsOutliersDetection$Results;", "process", data, as.integer(period), 
              as.integer(level), as.integer(slope), as.integer(noise), seasonal, rjd3toolkit::.r2jd_matrix(X),
              ao, ls, so, cv, tcv, estimation.forward, estimation.backward)
  model<-list(
    y=as.numeric(y),
    variables=rjd3toolkit::.proc_vector(jsts, "variables"),
    X=rjd3toolkit::.proc_matrix(jsts, "regressors"),
    b=rjd3toolkit::.proc_vector(jsts, "b"),
    bcov=rjd3toolkit::.proc_matrix(jsts, "bvar"),
    components=rjd3toolkit::.proc_matrix(jsts, "cmps"),
    linearized=rjd3toolkit::.proc_vector(jsts, "linearized")
  )
  
  l0<-rjd3toolkit::.proc_numeric(jsts, "initialbsm.levelvar")
  s0<-rjd3toolkit::.proc_numeric(jsts, "initialbsm.slopevar")
  seas0<-rjd3toolkit::.proc_numeric(jsts, "initialbsm.seasvar")
  n0<-rjd3toolkit::.proc_numeric(jsts, "initialbsm.noisevar")
  tau0 <- rjd3toolkit::.proc_matrix(jsts, "initialtau")
  
  
  l1<-rjd3toolkit::.proc_numeric(jsts, "finalbsm.levelvar")
  s1<-rjd3toolkit::.proc_numeric(jsts, "finalbsm.slopevar")
  seas1<-rjd3toolkit::.proc_numeric(jsts, "finalbsm.seasvar")
  n1<-rjd3toolkit::.proc_numeric(jsts, "finalbsm.noisevar")
  tau1 <- rjd3toolkit::.proc_matrix(jsts, "finaltau")
  
  ll0<-rjd3toolkit::.proc_likelihood(jsts, "initiallikelihood.")
  ll1<-rjd3toolkit::.proc_likelihood(jsts, "finallikelihood.")
  
  return(structure(list(
    model=model,
    bsm=list(
      initial=list(
        level=l0,
        slope=s0,
        seasonal=seas0,
        noise=n0,
        tau=tau0
      ),
      final=list(
        level=l1,
        slope=s1,
        seasonal=seas1,
        noise=n1,
        tau=tau1
      )
    ),
    likelihood=list(initial=ll0, final=ll1)),
    class="JD3_STS_OUTLIERS"))
}
