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
# take a (transformed) series
s<-log(rjd3toolkit::ABS$X0.2.09.10.M)

## ----tdvar1-------------------------------------------------------------------

# create the model
model<-rjd3sts::model()

# create the components and add them to the model
rjd3sts::add(model, rjd3sts::sarima("airline", 12, c(0,1,1), c(0,1,1)))

rjd3sts::add(model, vtd<-rjd3sts::reg_td("td", 12, start(s), length(s), contrast = F))
#estimate the model
rslt<-rjd3sts::estimate(model, s)

ss<-rjd3toolkit::result(rslt, "ssf.smoothing.states")
vss<-rjd3toolkit::result(rslt, "ssf.smoothing.vstates")


## -----------------------------------------------------------------------------
print(rjd3toolkit::dictionary(rslt))

## ----tdvar1_chart, fig.width=6, fig.height=4----------------------------------

colfunc<-colorRampPalette(c("red","blue","green","#196F3D"))
colors <- (colfunc(7))

pos<-rjd3toolkit::result(rslt, "ssf.cmppos")
plot(ss[,pos[2]+1], type='l', col=colors[1], ylim=c(-0.04, 0.03), ylab='coeff')
lines(ss[,pos[2]+2], col=colors[2])
lines(ss[,pos[2]+3], col=colors[3])
lines(ss[,pos[2]+4], col=colors[4])
lines(ss[,pos[2]]+5, col=colors[5])
lines(ss[,pos[2]+6], col=colors[6])
lines(-rowSums(ss[,pos[2]+(1:6)]), col=colors[7])


## ----tdvar2-------------------------------------------------------------------
# take a (transformed) series
s<-log(rjd3toolkit::ABS$X0.2.09.10.M)

# create the model
model<-rjd3sts::model()

# create the components and add them to the model
llt<-rjd3sts::locallineartrend('l')
seas<-rjd3sts::seasonal("s", 12, "HarrisonStevens")
n<-rjd3sts::noise('n')
rjd3sts::add(model, llt)
rjd3sts::add(model, seas)
rjd3sts::add(model, n)
rjd3sts::add(model, vtd<-rjd3sts::reg_td("td", 12, start(s), length(s), contrast = F))
#estimate the model
rslt<-rjd3sts::estimate(model, s)

ss<-rjd3toolkit::result(rslt, "ssf.smoothing.states")
vss<-rjd3toolkit::result(rslt, "ssf.smoothing.vstates")


## ----tdvar2_chart, fig.width=6, fig.height=4----------------------------------

colfunc<-colorRampPalette(c("red","blue","green","#196F3D"))
colors <- (colfunc(7))

pos<-rjd3toolkit::result(rslt, "ssf.cmppos")
plot(ss[,pos[4]+1], type='l', col=colors[1], ylim=c(-0.04, 0.03), ylab='coeff')
lines(ss[,pos[4]+2], col=colors[2])
lines(ss[,pos[4]+3], col=colors[3])
lines(ss[,pos[4]+4], col=colors[4])
lines(ss[,pos[4]]+5, col=colors[5])
lines(ss[,pos[4]+6], col=colors[6])
lines(-rowSums(ss[,pos[4]+(1:6)]), col=colors[7])


