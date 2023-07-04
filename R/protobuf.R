#' @include utils.R

p2r_diffuselikelihood<-function(p){
  return (structure(list(nobs=p$nobs, ndiffuse=p$ndiffuse, nparams=p$nparams, ndf=p$degrees_of_freedom,
                         ll=p$log_likelihood, adjll=p$adjusted_log_likelihood,
                         aic=p$aic, aicc=p$aicc, bic=p$bic, ssq=p$ssq, ldet=p$ldet, dcorr=p$dcorrection),
                    class = "JD3DIFFUSELIKELIHOOD"))
}

.proc_diffuselikelihood<-function(jrslt, prefix){
  return (list(
    ll=rjd3toolkit::.proc_numeric(jrslt, paste(prefix,"ll", sep="")),
    adjustedll=rjd3toolkit::.proc_numeric(jrslt, paste(prefix,"adjustedll", sep="")),
    ssq=rjd3toolkit::.proc_numeric(jrslt, paste(prefix,"ssqerr", sep="")),
    nobs=rjd3toolkit::.proc_int(jrslt, paste(prefix,"nobs", sep="")),
    ndiffuse=rjd3toolkit::.proc_int(jrslt, paste(prefix,"ndiffuse", sep="")),
    nparams=rjd3toolkit::.proc_int(jrslt, paste(prefix,"nparams", sep="")),
    df=rjd3toolkit::.proc_int(jrslt, paste(prefix,"df", sep="")))
  )
}

