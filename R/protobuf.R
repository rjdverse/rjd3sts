#' @include utils.R

p2r_diffuselikelihood<-function(p){
  return(structure(list(nobs=p$nobs, nparams=p$nparams, ndf=p$degrees_of_freedom,
                         ll=p$log_likelihood, adjll=p$adjusted_log_likelihood,
                         aic=p$aic, aicc=p$aicc, bic=p$bic, ssq=p$ssq, ldet=p$ldet, dcorr=p$dcorrection),
                    class = "JD3DIFFUSELIKELIHOOD"))
}


.proc_diffuselikelihood<-function(jrslt, prefix){
  return(list(
    ll=rjd3toolkit::.proc_numeric(jrslt, paste(prefix,"ll", sep="")),
    adjustedll=rjd3toolkit::.proc_numeric(jrslt, paste(prefix,"adjustedll", sep="")),
    ssq=rjd3toolkit::.proc_numeric(jrslt, paste(prefix,"ssqerr", sep="")),
    nobs=rjd3toolkit::.proc_int(jrslt, paste(prefix,"nobs", sep="")),
#    ndiffuse=rjd3toolkit::.proc_int(jrslt, paste(prefix,"ndiffuse", sep="")),
    nparams=rjd3toolkit::.proc_int(jrslt, paste(prefix,"nparams", sep="")),
    df=rjd3toolkit::.proc_int(jrslt, paste(prefix,"df", sep="")))
  )
}

# TO SUPPRESS IN NEXT RELEASE (EXPORTED FROM RJD3TOOLKIT 3.2.2)

likelihood<-function(nobs, neffectiveobs=NA, nparams=0, ll, adjustedll=NA, aic, aicc, bic, bicc, ssq){

  if (is.na(neffectiveobs)) neffectiveobs <- nobs
  if (is.na(adjustedll)) adjustedll <- ll

  return(structure(list(nobs=nobs, neffectiveobs=neffectiveobs, nparams=nparams,
                         ll=ll, adjustedll=adjustedll,
                         aic=aic, aicc=aicc, bic=bic, bicc=bicc, ssq=ssq),
                    class = "JD3_LIKELIHOOD"))
}

.p2r_likelihood<-function(p){
  return(likelihood(p$nobs, p$neffectiveobs, p$nparams,
                     p$log_likelihood, p$adjusted_log_likelihood,
                     p$aic, p$aicc, p$bic, p$bicc, p$ssq))
}
