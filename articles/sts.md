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

$$y_{t} = Z_{t}\alpha_{t} + \epsilon_{t},\quad\epsilon_{t} \sim N\left( 0,\sigma^{2}H_{t} \right),\quad t > 0$$

$$\alpha_{t + 1} = T_{t}\alpha_{t} + \mu_{t},\quad\mu_{t} \sim N\left( 0,\sigma^{2}V_{t} \right),\quad t \geq 0$$

$y_{t}$ is the observation at period t, $\alpha_{t}$ is the state
vector. $\epsilon_{t},\mu_{t}$ are assumed to be serially independent at
all leads and lags and independent from each other.  
In the case of multi-variate models, $y_{t}$ is a vector of
observations. However, in most cases, we will use the univariate
approach by considering the observations one by one (univariate handling
of multi-variate models).

The innovations of the state equation will be modelled as

$$\mu_{t} = S_{t}\xi_{t},\quad\xi_{t} \sim N\left( 0,\sigma^{2}I \right)$$

In other words, $V_{t} = S_{t}S_{t}\prime$

The initial ($\equiv t = 0$) conditions of the filter are defined as
follows:

$$\alpha_{0} = a_{0} + B\delta + \mu_{0},\quad\delta \sim N(0,\kappa I),\ \mu_{0} \sim N\left( 0,P_{*} \right)$$

where $\kappa$ is arbitrary large. $P_{*}$ is the variance of the
stationary part of the initial state vector and $B$ models the diffuse
part. We write $BB\prime = P_{\infty}$.

The definition used in JD+ is quasi-identical to that of Durbin and
Koopman\[1\].

In summary, the model is completely defined by the following quantities
(possible default values are indicated in brackets):

$$\mathbf{Z}_{\mathbf{t}},\mathbf{H}_{\mathbf{t}}\lbrack = 0\rbrack$$

$$\mathbf{T}_{\mathbf{t}},\mathbf{V}_{\mathbf{t}}\left\lbrack = S_{t}S_{t}\prime \right\rbrack,\mathbf{S}_{\mathbf{t}}\left\lbrack = Cholesky(V) \right\rbrack$$

$$\mathbf{a}_{\mathbf{0}}\lbrack = 0\rbrack,\mathbf{P}_{*}\lbrack = 0\rbrack,\mathbf{B}\lbrack = 0\rbrack,\mathbf{P}_{\mathbf{\infty}}\lbrack = BB\prime\rbrack$$

### Description of the state blocks

#### ar

##### Introduction

The auto-regressive block is defined by

$$\Phi(B)y_{t} = \epsilon_{t}$$

where:

$$\Phi(B) = 1 + \varphi_{1}B + \cdots + \varphi_{p}B^{p}$$

is an auto-regressive polynomial.

Let $\gamma_{i}$ be the autocovariances of the process

Using those notations, the state-space block can be written as follows :

#### State block:

$$\alpha_{t} = \begin{pmatrix}
y_{t} \\
y_{t - 1} \\
\vdots \\
y_{t - p + 1}
\end{pmatrix}$$  
The state block can be extended with additional lags. That can be useful
in complex (multi-variate) models

#### Dynamics

$$T_{t} = \begin{pmatrix}
{- \varphi_{1}} & \cdots & \cdots & {- \varphi_{p}} \\
1 & \cdots & \cdots & 0 \\
\vdots & \ddots & \ddots & \vdots \\
0 & 0 & 1 & 0
\end{pmatrix}$$

$$S_{t} = \sigma_{ar}\begin{pmatrix}
1 \\
0 \\
\vdots \\
0
\end{pmatrix}$$

$$V_{t} = SS\prime$$

#### Default loading

$$Z_{t} = \begin{pmatrix}
1 & 0 & \cdots & 0
\end{pmatrix}$$

#### Initialization

$$\alpha_{- 1} = \begin{pmatrix}
0 \\
0 \\
\vdots \\
0
\end{pmatrix}$$

$$P_{*} = \Omega$$$\Omega$ is the unconditional covariance of the state
array; it is computed by means of the auto-covariance function of the
model

$$\Omega_{t} = \begin{pmatrix}
\gamma_{0} & \gamma_{1} & \cdots & \gamma_{p} & \\
\gamma_{1} & \gamma_{0} & \gamma_{1} & \cdots & \\
\vdots & \ddots & \ddots & \vdots & \\
\gamma_{p} & \cdots & \gamma_{1} & \gamma_{0} & 
\end{pmatrix}$$

#### R code

The “ar” block is defined by specifying the coefficients $\phi_{i}$ of
the ar polynomial and the innovation variance. More exactly, they
correspond to the equation

$$y_{t} = \phi_{1}y_{t - 1} + \phi_{2}y_{t - 2} + \ldots + \phi_{p}y_{t - p} + \epsilon_{t}$$

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

$$\Phi(B)y_{t} = \epsilon_{t}$$

where:

$$\Phi(B) = 1 + \varphi_{1}B + \cdots + \varphi_{p}B^{p}$$

is an auto-regressive polynomial. However, modeling data that refers to
expectations may require including conditional expectations in the state
vector. Thus, the same type of representation that is used for the ARMA
model will be considered here.

Let $\gamma_{i}$ be the autocovariances of the model. We also define the
size of our state vector as $r0 = max(p,h + 1)$, where $h$ is the
forecast horizon desired by the user. If the user needs to use $nlags$
lagged values, whose default value is zero. Then the size of the state
vector will be $r = r0 + nlags$

Using those notations, the state-space model can be written as follows :

#### State block:

$$\alpha_{t} = \begin{pmatrix}
y_{t - nlags} \\
\vdots \\
y_{t - 1} \\
y_{t} \\
y_{t + 1|t} \\
\vdots \\
y_{t + h|t}
\end{pmatrix}$$

where $y_{t + i|t}$ is the orthogonal projection of $y_{t + i}$ on the
subspace generated by $y(s):s \leq t$. Thus, it is the forecast function
with respect to the semi-infinite sample. We also have that
$y_{t + i|t} = \sum_{j = i}^{\infty}{\psi_{j}\epsilon_{t + i - j}}$

#### Dynamics

$$T_{t} = \begin{pmatrix}
0 & 1 & 0 & \cdots & 0 \\
0 & 0 & 1 & \cdots & 0 \\
\vdots & \vdots & \vdots & \ddots & \vdots \\
0 & 0 & 0 & \cdots & 1 \\
{- \varphi_{r}} & \cdots & \cdots & \cdots & {- \varphi_{1}}
\end{pmatrix}$$

with $\varphi_{j} = 0$ for $j > p$

$$S_{t} = \sigma_{ar}\begin{pmatrix}
0 \\
\vdots \\
0 \\
1 \\
\psi_{1} \\
\vdots \\
\psi_{s}
\end{pmatrix}$$

$$V_{t} = SS\prime$$

#### Default loading

$$Z_{t} = \begin{pmatrix}
0 & \cdots & 0 & | & 1 & 0 & \cdots & 0
\end{pmatrix}$$

#### Initialization

$$\alpha_{- 1} = \begin{pmatrix}
0 \\
\vdots \\
0 \\
0 \\
0 \\
\vdots \\
0
\end{pmatrix}$$

$$P_{*} = \Omega$$

$\Omega$ is the unconditional covariance of the state array; it can be
easily derived using the MA representation. We have:

$$\Omega(i,0) = \gamma_{i}$$

$$\Omega(i,j) = \Omega(i - 1,j - 1) - \psi_{i}\psi_{j}$$

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

$$\Phi(B)y_{t} = \Theta(B)\epsilon_{t}$$

where:

$$\Phi(B) = 1 + \varphi_{1}B + \cdots + \varphi_{p}B^{p}$$

$$\Theta(B) = 1 + \theta_{1}B + \cdots + \theta_{q}B^{q}$$

are the auto-regressive and the moving average polynomials.

The MA representation of the process is
$y_{t} = \sum_{i = 0}^{\infty}{\psi_{i}\epsilon_{t - i}}$. Let
$\gamma_{i}$ be the autocovariances of the model. We also define:
$r = \max(p,q + 1),\quad s = r - 1$.

Using those notations, the state-space block can be written as follows :

#### State block:

$$\alpha_{t} = \begin{pmatrix}
y_{t} \\
y_{t + 1|t} \\
\vdots \\
y_{t + s|t}
\end{pmatrix}$$

where $y_{t + i|t}$ is the orthogonal projection of $y_{t + i}$ on the
subspace generated by $y(s):s \leq t$.Thus, it is the forecast function
with respect to the semi-infinite sample. We also have that
$y_{t + i|t} = \sum_{j = i}^{\infty}{\psi_{j}\epsilon_{t + i - j}}$

#### Dynamics

$$T_{t} = \begin{pmatrix}
0 & 1 & 0 & \cdots & 0 \\
0 & 0 & 1 & \cdots & 0 \\
\vdots & \vdots & \vdots & \ddots & \vdots \\
0 & 0 & 0 & \cdots & 1 \\
{- \varphi_{r}} & \cdots & \cdots & \cdots & {- \varphi_{1}}
\end{pmatrix}$$

with $\varphi_{j} = 0$ for $j > p$

$$S_{t} = \begin{pmatrix}
1 \\
\psi_{1} \\
\vdots \\
\psi_{s}
\end{pmatrix}$$

$$V_{t} = SS\prime$$

#### Default loading

$$Z_{t} = \begin{pmatrix}
1 & 0 & \cdots & 0
\end{pmatrix}$$

#### Initialization

$$\alpha_{- 1} = \begin{pmatrix}
0 \\
0 \\
\vdots \\
0
\end{pmatrix}$$

$$P_{*} = \Omega$$

$\Omega$ is the unconditional covariance of the state array; it can be
easily derived using the MA representation. We have:

$$\Omega(i,0) = \gamma_{i}$$

$$\Omega(i,j) = \Omega(i - 1,j - 1) - \psi_{i}\psi_{j}$$

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
