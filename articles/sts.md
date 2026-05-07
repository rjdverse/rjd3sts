# Sructural Time Series using JDemetra+

### Introduction

### Data

We illustrate the various methods using two sets of time series:

- The **retail** data set contains monthly figures over US retail
  activity of various categories of goods and services from 1992 to
  2010.
- The **ABS** data set contains long monthly series over Australian
  retail trade.

### Overview of the supported state space forms

The `general linear gaussian` state-space model can be written in many
different ways. The form considered in JD+ 3.0 is presented below.

``` math
 y_t = Z_t \alpha_t + \epsilon_t,\quad \epsilon_t \sim N\left(0, \sigma^2 H_t\right),\quad t \gt 0 
```

``` math
 \alpha_{t+1} = T_t \alpha_t + \mu_t, \quad \mu_t \sim N \left(0, \sigma^2 V_t \right),\quad t \ge 0 
```

$`y_t`$ is the observation at period t, $`\alpha_t`$ is the state
vector. $`\epsilon_t, \mu_t`$ are assumed to be serially independent at
all leads and lags and independent from each other.  
In the case of multi-variate models, $`y_t`$ is a vector of
observations. However, in most cases, we will use the univariate
approach by considering the observations one by one (univariate handling
of multi-variate models).

The innovations of the state equation will be modelled as

``` math
 \mu_t = S_t \xi_t, \quad \xi_t \sim N\left( 0, \sigma^2 I\right) 
```

In other words, $`V_t=S_t S_t'`$

The initial ($`\equiv t=0`$) conditions of the filter are defined as
follows:

``` math
 \alpha_{0} = a_{0} + B\delta + \mu_{0}, \quad \delta \sim N\left(0, \kappa I \right),\: \mu_{0} \sim N\left(0, P_*\right)
```

where $`\kappa`$ is arbitrary large. $`P_*`$ is the variance of the
stationary part of the initial state vector and $`B`$ models the diffuse
part. We write $`BB'=P_\infty`$.

The definition used in JD+ is quasi-identical to that of Durbin and
Koopman\[1\].

In summary, the model is completely defined by the following quantities
(possible default values are indicated in brackets):

``` math
 \mathbf{Z_t}, \mathbf{H_t} [=0] 
```

``` math
 \mathbf{T_t}, \mathbf{V_t} [=S_t S_t'], \mathbf{S_t} [=Cholesky(V)] 
```

``` math
 \mathbf{a_{0}}[=0], \mathbf{P_*} [=0], \mathbf{B} [=0], \mathbf{P_\infty} [=BB'] 
```

### Description of the state blocks

#### ar

##### Introduction

The auto-regressive block is defined by

``` math
\Phi\left(B\right)y_t=\epsilon_t 
```

where:

``` math
\Phi\left(B\right)=1+\varphi_1 B + \cdots + \varphi_p B^p 
```

is an auto-regressive polynomial.

Let $`\gamma_i`$ be the autocovariances of the process

Using those notations, the state-space block can be written as follows :

#### State block:

``` math
 \alpha_t= \begin{pmatrix} y_t \\ y_{t-1} \\ \vdots \\ y_{t-p+1} \end{pmatrix}
```
  
The state block can be extended with additional lags. That can be useful
in complex (multi-variate) models

#### Dynamics

``` math
 T_t = \begin{pmatrix}-\varphi_1 & \cdots & \cdots  & -\varphi_p  \\
                        1          & \cdots &  \cdots &  0          \\ 
                        \vdots     & \ddots &  \ddots & \vdots\\ 
                        0          &   0    &  1      & 0  \end{pmatrix}
```

``` math
 S_t = \sigma_{ar} \begin{pmatrix} 1 \\ 0 \\ \vdots\\ 0 \end{pmatrix} 
```

``` math
 V_t = S S' 
```

#### Default loading

``` math
 Z_t = \begin{pmatrix} 1 & 0 & \cdots & 0\end{pmatrix}
```

#### Initialization

``` math
 \alpha_{-1} = \begin{pmatrix}0 \\ 0 \\ \vdots\\ 0 \end{pmatrix} 
```

``` math
 P_{*} = \Omega 
```
$`\Omega`$ is the unconditional covariance of the state array; it is
computed by means of the auto-covariance function of the model

``` math
 \Omega_t = \begin{pmatrix}\gamma_0 & \gamma_1 & \cdots  & \gamma_p  \\
                        \gamma_1 & \gamma_0 & \gamma_1 &  \cdots &     \\ 
                        \vdots     &  \ddots & \ddots & \vdots\\ 
                        \gamma_p & \cdots & \gamma_1  & \gamma_0  \end{pmatrix}
```

#### R code

The “ar” block is defined by specifying the coefficients $`\phi_i`$ of
the ar polynomial and the innovation variance. More exactly, they
correspond to the equation

``` math
 y_t = \phi_1 y_{t-1} +  \phi_2 y_{t-2} + \dots + \phi_p y_{t-p} + \epsilon_t
```

The coefficients and/or the variance can be fixed

``` r

b_ar<-ar("ar", c(.7,-.4, .2), nlags=5, variance=1)

cat("T\n")
#> T
knit_print(block_t(b_ar))
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]  0.7 -0.4  0.2    0    0
#> [2,]  1.0  0.0  0.0    0    0
#> [3,]  0.0  1.0  0.0    0    0
#> [4,]  0.0  0.0  1.0    0    0
#> [5,]  0.0  0.0  0.0    1    0
cat("\nP0\n")
#> 
#> P0
knit_print(block_p0(b_ar))
#>            [,1]       [,2]       [,3]       [,4]       [,5]
#> [1,] 1.51552795 0.77018634 0.08695652 0.05590062 0.15838509
#> [2,] 0.77018634 1.51552795 0.77018634 0.08695652 0.05590062
#> [3,] 0.08695652 0.77018634 1.51552795 0.77018634 0.08695652
#> [4,] 0.05590062 0.08695652 0.77018634 1.51552795 0.77018634
#> [5,] 0.15838509 0.05590062 0.08695652 0.77018634 1.51552795
```

### ar2

#### Introduction

An alternative representation of the auto-regressive block will be very
useful for the purposes of reflecting expectations. The process is
defined as above:

``` math
\Phi\left(B\right)y_t=\epsilon_t 
```

where:

``` math
\Phi\left(B\right)=1+\varphi_1 B + \cdots + \varphi_p B^p 
```

is an auto-regressive polynomial. However, modeling data that refers to
expectations may require including conditional expectations in the state
vector. Thus, the same type of representation that is used for the ARMA
model will be considered here.

Let $`\gamma_i`$ be the autocovariances of the model. We also define the
size of our state vector as $`r0=max(p,h+1)`$, where $`h`$ is the
forecast horizon desired by the user. If the user needs to use $`nlags`$
lagged values, whose default value is zero. Then the size of the state
vector will be $`r=r0+nlags`$

Using those notations, the state-space model can be written as follows :

#### State block:

``` math
 \alpha_t= \begin{pmatrix} 
y_{t-nlags} \\ 
\vdots  \\ 
y_{t-1}  \\ 
\hline
y_{t}  \\ 
y_{t+1|t} \\ 
\vdots \\ 
y_{t+h|t} 
\end{pmatrix}
```

where $`y_{t+i|t}`$ is the orthogonal projection of $`y_{t+i}`$ on the
subspace generated by $`{y\left(s\right):s \leq t}`$. Thus, it is the
forecast function with respect to the semi-infinite sample. We also have
that $`y_{t+i|t} = \sum_{j=i}^\infty {\psi_j \epsilon_{t+i-j}}`$

#### Dynamics

``` math
 T_t = \begin{pmatrix} 0 &1 & 0 & \cdots & 0  \\0& 0 & 1 & \cdots & 0\\ \vdots & \vdots & \vdots & \ddots & \vdots\\ 0 & 0 & 0 & \cdots & 1\\
-\varphi_r & \cdots  & \cdots & \cdots &-\varphi_1 \end{pmatrix}
```

with $`\varphi_{j}=0`$ for $`j>p`$

``` math
 S_t = \sigma_{ar} \begin{pmatrix} 0 \\  
\vdots \\
0\\
\hline
1 \\ \psi_1 \\ \vdots\\ \psi_s \end{pmatrix} 
```

``` math
 V_t = S S' 
```

#### Default loading

``` math
 Z_t = \begin{pmatrix} 0 & \cdots  &0  & | & 1 & 0 & \cdots & 0\end{pmatrix}
```

#### Initialization

``` math
 \alpha_{-1}  = \begin{pmatrix} 0 \\  
\vdots \\
0\\
\hline
0 \\ 0 \\ \vdots\\ 0 \end{pmatrix} 
```

``` math
 P_{*} = \Omega 
```

$`\Omega`$ is the unconditional covariance of the state array; it can be
easily derived using the MA representation. We have:

``` math
 \Omega\left(i,0\right) = \gamma_i 
```

``` math
 \Omega\left(i,j\right) = \Omega\left(i-1,j-1\right)-\psi_i \psi_j 
```

``` r

b_ar2<-ar2("ar2", c(-.2, .4, -.1), nlags=3, nfcasts=2)
knit_print(block_t(b_ar2))
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    0    1    0  0.0  0.0  0.0
#> [2,]    0    0    1  0.0  0.0  0.0
#> [3,]    0    0    0  1.0  0.0  0.0
#> [4,]    0    0    0  0.0  1.0  0.0
#> [5,]    0    0    0  0.0  0.0  1.0
#> [6,]    0    0    0 -0.1  0.4 -0.2
```

### arma

#### Introduction

The arma block is defined by

``` math
\Phi\left(B\right)y_t=\Theta\left(B\right)\epsilon_t 
```

where:

``` math
\Phi\left(B\right)=1+\varphi_1 B + \cdots + \varphi_p B^p 
```

``` math
\Theta\left(B\right)=1+\theta_1 B + \cdots + \theta_q B^q 
```

are the auto-regressive and the moving average polynomials.

The MA representation of the process is
$`y_t=\sum_{i=0}^\infty {\psi_i \epsilon_{t-i}}`$. Let $`\gamma_i`$ be
the autocovariances of the model. We also define:
$`r=\max\left(p, q+1\right), \quad s=r-1`$.

Using those notations, the state-space block can be written as follows :

#### State block:

``` math
 \alpha_t= \begin{pmatrix} y_t \\ y_{t+1|t} \\ \vdots \\ y_{t+s|t} \end{pmatrix}
```

where $`y_{t+i|t}`$ is the orthogonal projection of $`y_{t+i}`$ on the
subspace generated by $`{y\left(s\right):s \leq t}`$.Thus, it is the
forecast function with respect to the semi-infinite sample. We also have
that $`y_{t+i|t} = \sum_{j=i}^\infty {\psi_j \epsilon_{t+i-j}}`$

#### Dynamics

``` math
 T_t = \begin{pmatrix}0 &1 & 0 & \cdots & 0  \\0& 0 & 1 & \cdots & 0\\ \vdots & \vdots & \vdots & \ddots & \vdots\\ 0 & 0 & 0 & \cdots & 1\\
-\varphi_r & \cdots  & \cdots & \cdots &-\varphi_1 \end{pmatrix}
```

with $`\varphi_{j}=0`$ for $`j>p`$

``` math
 S_t = \begin{pmatrix}1 \\ \psi_1 \\ \vdots\\ \psi_s \end{pmatrix} 
```

``` math
 V_t = S S' 
```

#### Default loading

``` math
 Z_t = \begin{pmatrix} 1 & 0 & \cdots & 0\end{pmatrix}
```

#### Initialization

``` math
 \alpha_{-1} = \begin{pmatrix}0 \\ 0 \\ \vdots\\ 0 \end{pmatrix} 
```

``` math
 P_{*} = \Omega 
```

$`\Omega`$ is the unconditional covariance of the state array; it can be
easily derived using the MA representation. We have:

``` math
 \Omega\left(i,0\right) = \gamma_i 
```

``` math
 \Omega\left(i,j\right) = \Omega\left(i-1,j-1\right)-\psi_i \psi_j 
```

``` r

b_arma<-arma("arma", ar=c(-.2, .4, -.1), ma=c(.3, .6))
knit_print(block_t(b_arma))
#>      [,1] [,2] [,3]
#> [1,]  0.0  1.0  0.0
#> [2,]  0.0  0.0  1.0
#> [3,]  0.1 -0.4  0.2
knit_print(block_p0(b_arma))
#>           [,1]      [,2]      [,3]
#> [1,] 1.3501359 0.6394319 0.2517752
#> [2,] 0.6394319 0.3501359 0.1394319
#> [3,] 0.2517752 0.1394319 0.1001359
```

### arima

## Examples

### [Basic structural models](https://rjdverse.github.io/rjd3sts/articles/bsm.md)

### [Time varying trading days](https://rjdverse.github.io/rjd3sts/articles/tdvar.md)

### [Regular period cubic splines](https://rjdverse.github.io/rjd3sts/articles/regsplines.md)

### [Modeling Nile riverflow](https://rjdverse.github.io/rjd3sts/articles/regcmp_4.md)

### [Time series with a sampling error](https://rjdverse.github.io/rjd3sts/articles/regcmp_6.md)
