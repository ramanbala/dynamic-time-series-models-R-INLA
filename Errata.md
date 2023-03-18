Errata
================
2023-03-18

<!-- [//]: # ({% raw %}) -->
<!-- [//]: # ({% endraw %}) -->
<!-- ## Corrections {-} -->

### Chapter-6, Section 6.2.2

<!-- [//]: # ({% raw %}) -->
<!-- [//]: # ({% endraw %}) -->

The correct model output for *model.panelts.2* is

<!-- [//]: # ({% raw %}) -->

``` r
format.inla.out(summary.model.panelts.2$fixed[,c(1,2,4)])
##    name         mean   sd    0.5q  
## 1  fact.alpha1   0.470 0.070  0.470
## 2  fact.alpha2  -0.511 0.072 -0.511
## 3  fact.alpha3   0.162 0.071  0.162
## 4  fact.alpha4   0.728 0.069  0.728
## 5  fact.alpha5   1.395 0.070  1.395
## 6  fact.alpha6  -1.311 0.070 -1.311
## 7  fact.alpha7  -0.551 0.072 -0.551
## 8  fact.alpha8   0.998 0.070  0.998
## 9  fact.alpha9  -1.064 0.070 -1.064
## 10 fact.alpha10 -0.352 0.072 -0.352
format.inla.out(summary.model.panelts.2$hyperpar[,c(1:2)])
##   name                       mean  sd   
## 1 Precision for Gaussian obs 1.065 0.042
## 2 Precision for id.beta1     1.230 0.103
## 3 Rho for id.beta1           0.760 0.026
```

<!-- [//]: # ({% endraw %}) -->

### Chapter-11, Section 11.4.2

In the code chunk following Figure 11.1 in the online version, replace
*tnc.type3* with *tnc.kh3*, i.e. replace

``` r
post.sampletype3 <-
  inla.posterior.sample(n = 500, tnc.type3, seed = 1234)
```

with

``` r
post.sampletype3 <-
  inla.posterior.sample(n = 500, tnc.kh3, seed = 1234)
```
