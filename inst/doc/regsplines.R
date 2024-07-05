## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE---------------------------------------------------------------

suppressPackageStartupMessages(library(rjd3toolkit))
suppressPackageStartupMessages(library(rjd3sts))
library(knitr)

## ----data---------------------------------------------------------------------
s<-log(ABS$X0.2.09.10.M)

## ----bsm----------------------------------------------------------------------
model<-model()

llt<-locallineartrend('l')
seas<-seasonal("s", 12, "HarrisonStevens")
n<-noise('n')
add(model, llt)
add(model, seas)
add(model, n)

rslt<-estimate(model, s)

sa1<-result(rslt, "ssf.smoothing.components")

## ----bsm_splines1-------------------------------------------------------------
model<-model()
seas<-splines_regular("s", 12, nodes=c(0:11))
add(model, llt)
add(model, seas)
add(model, n)

rslt<-estimate(model, s)

sa2<-result(rslt, "ssf.smoothing.components")

summary(sa1[,2]-sa2[,2])

## ----bsm_splines2, fig.width=6, fig.height=5----------------------------------
model<-model()
seas<-splines_regular("s", 12, nodes=c(1,2,6,7,8,9,10,11))
add(model, llt)
add(model, seas)
add(model, n)

rslt<-estimate(model, s)

sa3<-result(rslt, "ssf.smoothing.components")


matplot(cbind(sa1[301:336,2],sa2[301:336,2],sa3[301:336,2]), type='l', ylab="Seasonal")
