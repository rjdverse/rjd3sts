#' @include utils.R
NULL

#' Title
#'
#' @param y input time series.
#' @param period annual frequency.
#' @param level -1 = no level, 0 = fixed level, 1 = sotchastic level
#' @param slope 
#' @param noise 
#' @param seasonal Seasonal model
#' @param X Regression variables (same length as y) or NULL
#' @param X.td Groups of days for trading days regressors. The length of the array must be 7. 
#' It indicates to what group each week day belongs. 
#' The first item corresponds to Mondays and the last one to Sundays. The group used for contrasts (usually Sundays) is identified by 0. The other groups are identified by 1, 2,... n (<= 6). 
#' For instance, usual trading days are defined by `c(1,2,3,4,5,6,0)`, week days by `c(1,1,1,1,1,0,0)`, etc...
#' @return
#' @export
#'
#' @examples
#'  x<-rjd3toolkit::retail$BookStores
#'  seasonalbreaks(x)
seasonalbreaks<-function(y, period=NA, level=1, slope=1, noise=1, seasonal=c("HarrisonStevens", "Trigonometric", "Dummy", "Crude", "Fixed", "Unused"),
                       X=NULL,X.td=NULL){
  
  data<-as.numeric(y)
  if (is.ts(y)){
    period<-frequency(y)
  }else{
    if (! is.null(X.td)){
      stop("y must be a time series when X.td is used")
    }
    if (is.na(period)){
      stop("y must be a time series or period must be specified")
    }
  }
  seasonal<-match.arg(seasonal)
  if (! is.null(X.td)){
    td <- rjd3toolkit::td(s = y, groups = X.td)
    X<-cbind(X, td)
  }

  so<-.jcall("jdplus/sts/base/r/StsOutliersDetection", "[D", "seasonalBreaks", data, as.integer(period), 
               as.integer(level), as.integer(slope), as.integer(noise), seasonal, rjd3toolkit::.r2jd_matrix(X))
  
  if (is.ts(y)){
    return (ts(so, frequency = period, start=start(y)))
  }else{
    return (so)
  }
}
