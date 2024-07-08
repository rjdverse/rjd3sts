## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE---------------------------------------------------------------
suppressPackageStartupMessages(library(rjd3toolkit))
suppressPackageStartupMessages(library(rjd3sts))
library(knitr)

## -----------------------------------------------------------------------------
b_ar<-ar("ar", c(-.2, .4, -.1), nlags=5)
knit_print(block_t(b_ar))


## -----------------------------------------------------------------------------
b_ar2<-ar2("ar2", c(-.2, .4, -.1), nlags=3, nfcasts=2)
knit_print(block_t(b_ar2))


## -----------------------------------------------------------------------------
b_arma<-arma("arma", ar=c(-.2, .4, -.1), ma=c(.3, .6))
knit_print(block_t(b_arma))
knit_print(block_p0(b_arma))


