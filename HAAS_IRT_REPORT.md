---
title: "HAAS_IRT_REPORT"
output: 
  html_document:
    keep_md: true
---





``` r
HAAS <- readRDS("HAAS.rds")
HAAS <- as.data.frame(HAAS)
df <- HAAS
head(df)
```

```
##   HAAS1 HAAS2 HAAS3 HAAS4
## 1     5     2     2     3
## 2     2     1     2     3
## 3     4     1     2     1
## 4     4     0     2     3
## 5     4     1     2     1
## 6     4     2     2     1
```

# Dimensionality


``` r
aisp1 <- aisp(df, search = 'normal', lowerbound=seq(.3,.8,by=.05))
aisp1
```

```
##       0.3 0.35 0.4 0.45 0.5 0.55 0.6
## HAAS1   1    1   1    1   1    1   1
## HAAS2   1    1   1    1   1    1   1
## HAAS3   1    1   1    0   0    0   0
## HAAS4   1    1   0    0   0    0   0
```

``` r
aisp2 <- aisp(df, search = 'ga', lowerbound=seq(.3,.8,by=.05))
aisp2
```

```
##       0.3 0.35 0.4 0.45 0.5 0.55 0.6
## HAAS1   1    1   1    1   1    1   1
## HAAS2   1    1   1    1   1    1   1
## HAAS3   1    1   1    0   0    0   0
## HAAS4   1    1   0    0   0    0   0
```

# Scalability


``` r
H <- coefH(df)
```

```
## $Hij
##       HAAS1   se      HAAS2   se      HAAS3   se      HAAS4   se     
## HAAS1                  0.645  (0.077)  0.375  (0.115)  0.326  (0.120)
## HAAS2  0.645  (0.077)                  0.447  (0.114)  0.416  (0.097)
## HAAS3  0.375  (0.115)  0.447  (0.114)                  0.313  (0.107)
## HAAS4  0.326  (0.120)  0.416  (0.097)  0.313  (0.107)                
## 
## $Hi
##       Item H  se     
## HAAS1   0.446 (0.077)
## HAAS2   0.509 (0.060)
## HAAS3   0.370 (0.088)
## HAAS4   0.356 (0.085)
## 
## $H
## Scale H      se 
##   0.422 (0.066)
```

# Minsize

The parameter below will determine the minimum size of the "rest groups" for tests of monotonicity, order invariance, local independence, etc.

If it is too large, it will not allow the construction of at least 2 groups, compromising the methods. If it is too small, it generates less reliable statistics.

From attempts we arrived at the value of 40.

``` r
minsize <- 40
```

# Monotonicity


``` r
monotonicity_test <- check.monotonicity(df, minsize = minsize)
summary(monotonicity_test)
```

```
##       ItemH #ac #vi #vi/#ac maxvi sum sum/#ac zmax #zsig crit
## HAAS1  0.45   3   0       0     0   0       0    0     0    0
## HAAS2  0.51   4   0       0     0   0       0    0     0    0
## HAAS3  0.37   3   0       0     0   0       0    0     0    0
## HAAS4  0.36   6   0       0     0   0       0    0     0    0
```


``` r
plot(monotonicity_test)
```

![](HAAS_IRT_REPORT_files/figure-html/unnamed-chunk-6-1.png)<!-- -->![](HAAS_IRT_REPORT_files/figure-html/unnamed-chunk-6-2.png)<!-- -->![](HAAS_IRT_REPORT_files/figure-html/unnamed-chunk-6-3.png)<!-- -->![](HAAS_IRT_REPORT_files/figure-html/unnamed-chunk-6-4.png)<!-- -->

# IIO


``` r
check.iio.test <- check.iio(
  df, 
  method = "MIIO", 
  minsize = minsize, 
  verbose = FALSE, 
  item.selection = FALSE,
)
summary(check.iio.test)
```

```
## $method
## [1] "MIIO"
## 
## $item.summary
##       ItemH #ac #vi #vi/#ac maxvi sum sum/#ac tmax #tsig crit
## HAAS1  0.45   3   0       0     0   0       0    0     0    0
## HAAS4  0.36   3   0       0     0   0       0    0     0    0
## HAAS3  0.37   3   0       0     0   0       0    0     0    0
## HAAS2  0.51   3   0       0     0   0       0    0     0    0
## 
## $backward.selection
##       step 1
## HAAS1      0
## HAAS4      0
## HAAS3      0
## HAAS2      0
## 
## $HT
## [1] 0.654295
```


``` r
par(mfrow=c(2,3))
plot(check.iio.test)
```

![](HAAS_IRT_REPORT_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

# Local Independence


``` r
IL <- check.ca(df, Windex=T, MINSIZE=minsize)
IL$InScale
```

```
## [[1]]
## [1] TRUE TRUE TRUE TRUE
```

``` r
summary(IL)
```

```
##         Length Class  Mode
## InScale 1      -none- list
## Index   1      -none- list
## Flagged 1      -none- list
```

## Reliability


``` r
check.reliability(
  df,
  MS = TRUE,
  alpha = TRUE,
  lambda.2 = TRUE,
  LCRC = TRUE
)
```

```
## $MS
## [1] 0.7076666
## 
## $alpha
## [1] 0.6630872
## 
## $lambda.2
## [1] 0.6800608
## 
## $LCRC
## [1] 0.6634715
```

# Parametric Model


``` r
fitGraded <- mirt(df, 1, itemtype = "graded", SE = FALSE, verbose = TRUE)
```

```
## Iteration: 1, Log-Lik: -518.931, Max-Change: 0.82902Iteration: 2, Log-Lik: -509.544, Max-Change: 0.47411Iteration: 3, Log-Lik: -505.608, Max-Change: 0.37382Iteration: 4, Log-Lik: -503.146, Max-Change: 0.16577Iteration: 5, Log-Lik: -502.848, Max-Change: 0.12144Iteration: 6, Log-Lik: -502.716, Max-Change: 0.09130Iteration: 7, Log-Lik: -502.608, Max-Change: 0.01821Iteration: 8, Log-Lik: -502.604, Max-Change: 0.05080Iteration: 9, Log-Lik: -502.599, Max-Change: 0.03180Iteration: 10, Log-Lik: -502.594, Max-Change: 0.01632Iteration: 11, Log-Lik: -502.593, Max-Change: 0.00386Iteration: 12, Log-Lik: -502.593, Max-Change: 0.01339Iteration: 13, Log-Lik: -502.592, Max-Change: 0.01108Iteration: 14, Log-Lik: -502.592, Max-Change: 0.00604Iteration: 15, Log-Lik: -502.591, Max-Change: 0.00068Iteration: 16, Log-Lik: -502.591, Max-Change: 0.01602Iteration: 17, Log-Lik: -502.591, Max-Change: 0.00359Iteration: 18, Log-Lik: -502.591, Max-Change: 0.00016Iteration: 19, Log-Lik: -502.591, Max-Change: 0.00016Iteration: 20, Log-Lik: -502.591, Max-Change: 0.00017Iteration: 21, Log-Lik: -502.591, Max-Change: 0.00005
```

``` r
summary(fitGraded)
```

```
##          F1    h2
## HAAS1 0.810 0.657
## HAAS2 0.843 0.710
## HAAS3 0.618 0.382
## HAAS4 0.532 0.283
## 
## SS loadings:  2.032 
## Proportion Var:  0.508 
## 
## Factor correlations: 
## 
##    F1
## F1  1
```


``` r
theta <- fscores(fitGraded, full.scores.SE = TRUE)
empirical_rxx(theta)
```

```
##        F1 
## 0.7824792
```

## M2


``` r
M2(fitGraded, type = "C2", calcNULL = FALSE)
```

```
##             M2 df         p RMSEA RMSEA_5  RMSEA_95      SRMSR      TLI CFI
## stats 1.157516  2 0.5605941     0       0 0.1671818 0.04565509 1.030963   1
```

## Itemfit


``` r
itemfit(fitGraded, p.adjust = 'fdr')
```

```
##    item   S_X2 df.S_X2 RMSEA.S_X2 p.S_X2
## 1 HAAS1  6.832       7      0.000  0.568
## 2 HAAS2 13.090      10      0.055  0.568
## 3 HAAS3  8.209       8      0.016  0.568
## 4 HAAS4  9.584      11      0.000  0.568
```

## Item Parameters


``` r
params <- coef(fitGraded, IRTpars = TRUE, simplify = TRUE)
round(params$items, 2)
```

```
##          a    b1    b2    b3    b4   b5   b6
## HAAS1 2.35 -2.62 -1.82 -0.78 -0.75 0.29   NA
## HAAS2 2.66 -0.79  0.04  1.21  2.51   NA   NA
## HAAS3 1.34 -4.10 -1.24  1.71    NA   NA   NA
## HAAS4 1.07 -3.43 -1.45 -0.58  1.41 1.85 3.22
```

## Empirical CCI


``` r
CCI_empirica <- lapply(
  1:ncol(df), 
  function(x) itemfit(fitGraded, empirical.plot = x)
)
CCI_empirica
```

```
## [[1]]
```

![](HAAS_IRT_REPORT_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```
## 
## [[2]]
```

![](HAAS_IRT_REPORT_files/figure-html/unnamed-chunk-16-2.png)<!-- -->

```
## 
## [[3]]
```

![](HAAS_IRT_REPORT_files/figure-html/unnamed-chunk-16-3.png)<!-- -->

```
## 
## [[4]]
```

![](HAAS_IRT_REPORT_files/figure-html/unnamed-chunk-16-4.png)<!-- -->

## Trace Plot


``` r
plot(fitGraded, type = 'trace')
```

![](HAAS_IRT_REPORT_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

## Info Plot


``` r
plot(fitGraded, type = "infoSE")
```

![](HAAS_IRT_REPORT_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

``` r
plot(fitGraded, type = 'infotrace')
```

![](HAAS_IRT_REPORT_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

# Citation


``` r
citation('mokken')
```

```
## To cite mokken in publications use:
## 
##   Van der Ark LA (2007). "Mokken Scale Analysis in R." _Journal of
##   Statistical Software_, *20*(11), 1-19. doi:10.18637/jss.v020.i11
##   <https://doi.org/10.18637/jss.v020.i11>.
## 
##   Van der Ark LA (2012). "New Developments in Mokken Scale Analysis in
##   R." _Journal of Statistical Software_, *48*(5), 1-27.
##   doi:10.18637/jss.v048.i05 <https://doi.org/10.18637/jss.v048.i05>.
## 
## To see these entries in BibTeX format, use 'print(<citation>,
## bibtex=TRUE)', 'toBibtex(.)', or set
## 'options(citation.bibtex.max=999)'.
```

``` r
citation('mirt')
```

```
## To cite mirt in publications use:
## 
##   R. Philip Chalmers (2012). mirt: A Multidimensional Item Response
##   Theory Package for the R Environment. Journal of Statistical
##   Software, 48(6), 1-29. doi:10.18637/jss.v048.i06
## 
## A BibTeX entry for LaTeX users is
## 
##   @Article{,
##     title = {{mirt}: A Multidimensional Item Response Theory Package for the {R} Environment},
##     author = {R. Philip Chalmers},
##     journal = {Journal of Statistical Software},
##     year = {2012},
##     volume = {48},
##     number = {6},
##     pages = {1--29},
##     doi = {10.18637/jss.v048.i06},
##   }
```
