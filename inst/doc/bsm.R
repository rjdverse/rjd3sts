## ----echo=FALSE---------------------------------------------------------------

suppressPackageStartupMessages(library(rjd3toolkit))
suppressPackageStartupMessages(library(rjd3sts))
library(knitr)



## ----data---------------------------------------------------------------------
s<-log(retail$BookStores)

## ----bsm1---------------------------------------------------------------------

# create the model
bsm<-model()
# create the components and add them to the model
add(bsm, locallineartrend("ll"))
add(bsm, seasonal("s", 12, type="HarrisonStevens"))
add(bsm, noise("n"))
rslt<-estimate(bsm, log(s), marginal=T)



## ----bsm2---------------------------------------------------------------------

# create the model
bsm<-model()
# create the components and add them to the model
add(bsm, locallineartrend("ll"))
add(bsm, seasonal("s", 12, type="HarrisonStevens"))
  # create the equation (fix the variance to 1)
eq<-equation("eq", 1,T)
add_equation(eq, "ll")
add_equation(eq, "s")
add(bsm, eq)
rslt<-estimate(bsm, log(s), marginal=T)

## ----bsm3---------------------------------------------------------------------

# create the model
bsm<-model()
  # create the components, with fixed variances, and add them to the model
add(bsm, locallineartrend("ll", 
                             levelVariance = 1, fixedLevelVariance = TRUE) )
add(bsm, seasonal("s", 12, type="HarrisonStevens", 
                     variance = 1, fixed = TRUE))
add(bsm, noise("n", 1, fixed=TRUE))
  # create the equation (fix the variance to 1)
eq<-equation("eq", 0, TRUE)
add_equation(eq, "ll", .01, FALSE)
add_equation(eq, "s", .01, FALSE)
add_equation(eq, "n")
add(bsm, eq)
rslt<-estimate(bsm, log(s), marginal=TRUE)
p<-result(rslt, "parameters")

## ----bsm4---------------------------------------------------------------------

# create the model
bsm<-model()
  # create the components and add them to the model
add(bsm, locallevel("l", initial = 0) )
add(bsm, locallineartrend("lt", levelVariance = 0, 
                             fixedLevelVariance = TRUE) )
add(bsm, seasonal("s", 12, type="HarrisonStevens"))
add(bsm, noise("n", 1, fixed=TRUE))
  # create the equation (fix the variance to 1)
rslt<-estimate(bsm, log(s), marginal=TRUE)


## ----fig.width=6--------------------------------------------------------------
ss<-smoothed_states(rslt)
plot(ss[,1]+ss[,2], type='l', col='blue', ylab='trends')
lines(ss[, 2], col='red')

