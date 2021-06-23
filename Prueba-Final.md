Examen Final
================

# Importamos tabla de datos

``` r
library(readxl)
datos<- read_excel("C:/Users/user/Desktop/af.xlsx")
```

# Tipificacion o estandarizacion de variables

la tipificacion permite que todas las variables metricas gocen de una
misma unidad de medida estadistica.

``` r
datost<- datos #crear una nueva base de datos o data frame
datost<- scale(datost, center= T, scale= T)
datost<- as.data.frame(datost)
```

# NORMALIDAD multivariante

H0: Normalidad multivariante H1: No normalidad multivariante confianza=
95% Alfa= 5% = 0,05 P value &gt; alfa: no se rechaza la H0 (Normalidad)
P value &lt; alfa: se rechaza la H0 (No normalidad)

``` r
library(MVN)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## sROC 0.1-2 loaded

``` r
mvn(datost[2:7])
```

    ## $multivariateNormality
    ##              Test         Statistic           p value Result
    ## 1 Mardia Skewness  48.2333933698349 0.760228390445751    YES
    ## 2 Mardia Kurtosis -1.48496413209869 0.137553325757602    YES
    ## 3             MVN              <NA>              <NA>    YES
    ## 
    ## $univariateNormality
    ##           Test         Variable Statistic   p value Normality
    ## 1 Shapiro-Wilk       edad          0.8933    0.0058    NO    
    ## 2 Shapiro-Wilk     peso(Kg)        0.9518    0.1887    YES   
    ## 3 Shapiro-Wilk Partidos jugados    0.9541    0.2179    YES   
    ## 4 Shapiro-Wilk     Lesiones        0.9763    0.7218    YES   
    ## 5 Shapiro-Wilk Horas de entreno    0.9122    0.0169    NO    
    ## 6 Shapiro-Wilk   salario(USD)      0.9875    0.9720    YES   
    ## 
    ## $Descriptives
    ##                   n          Mean Std.Dev      Median       Min      Max
    ## edad             30  8.279691e-17       1 -0.19642105 -1.236297 1.710019
    ## peso(Kg)         30 -4.803991e-16       1 -0.11392778 -1.497337 2.246005
    ## Partidos jugados 30  1.050050e-16       1 -0.03097148 -1.773117 1.711174
    ## Lesiones         30  0.000000e+00       1 -0.14007537 -1.820980 2.661432
    ## Horas de entreno 30  8.417926e-17       1 -0.02619286 -1.597764 1.545379
    ## salario(USD)     30 -2.671474e-16       1 -0.06792110 -2.051932 2.023334
    ##                        25th      75th        Skew    Kurtosis
    ## edad             -0.8463436 0.6268142  0.49173391 -1.20996819
    ## peso(Kg)         -0.9683862 0.7812191  0.27891524 -1.02579919
    ## Partidos jugados -0.4665079 0.8401014 -0.13298287 -0.97893080
    ## Lesiones         -0.7003769 0.6303392  0.46833319 -0.01970568
    ## Horas de entreno -0.8119786 0.7595929  0.03723389 -1.03805880
    ## salario(USD)     -0.6309513 0.7095968 -0.11802590 -0.69759403

como el P value &gt; alfa, no se rechaza la H0, por lo tanto, existe
normalidad multivariante.

# MATRIZ DE CORRELACIONES

H0: correlacion = 0 (no hay correlacion) H1: correlacion diferente de 0
(si hay correlacion)

Cuando no se rechaza H0, no se aplica AFE. se rehace H0, si para aplicar
AFE.

``` r
library(psych)
corr.test(datost[,2:7])
```

    ## Call:corr.test(x = datost[, 2:7])
    ## Correlation matrix 
    ##                   edad peso(Kg) Partidos jugados Lesiones Horas de entreno
    ## edad              1.00     0.15             0.07    -0.19             0.29
    ## peso(Kg)          0.15     1.00            -0.03    -0.24             0.18
    ## Partidos jugados  0.07    -0.03             1.00     0.18            -0.35
    ## Lesiones         -0.19    -0.24             0.18     1.00             0.22
    ## Horas de entreno  0.29     0.18            -0.35     0.22             1.00
    ## salario(USD)     -0.18     0.27            -0.39    -0.22             0.00
    ##                  salario(USD)
    ## edad                    -0.18
    ## peso(Kg)                 0.27
    ## Partidos jugados        -0.39
    ## Lesiones                -0.22
    ## Horas de entreno         0.00
    ## salario(USD)             1.00
    ## Sample Size 
    ## [1] 30
    ## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
    ##                  edad peso(Kg) Partidos jugados Lesiones Horas de entreno
    ## edad             0.00     1.00             1.00     1.00             1.00
    ## peso(Kg)         0.44     0.00             1.00     1.00             1.00
    ## Partidos jugados 0.71     0.85             0.00     1.00             0.81
    ## Lesiones         0.31     0.19             0.35     0.00             1.00
    ## Horas de entreno 0.12     0.35             0.06     0.25             0.00
    ## salario(USD)     0.33     0.15             0.03     0.23             1.00
    ##                  salario(USD)
    ## edad                     1.00
    ## peso(Kg)                 1.00
    ## Partidos jugados         0.47
    ## Lesiones                 1.00
    ## Horas de entreno         1.00
    ## salario(USD)             0.00
    ## 
    ##  To see confidence intervals of the correlations, print with the short=FALSE option

``` r
correlaciones<- corr.test(datost[,2:7]) # se crea la matriz de correlaciones
correlaciones$r #matriz de correlaciones
```

    ##                        edad    peso(Kg) Partidos jugados   Lesiones
    ## edad              1.0000000  0.14609451       0.07005840 -0.1908662
    ## peso(Kg)          0.1460945  1.00000000      -0.03493748 -0.2437009
    ## Partidos jugados  0.0700584 -0.03493748       1.00000000  0.1772736
    ## Lesiones         -0.1908662 -0.24370087       0.17727362  1.0000000
    ## Horas de entreno  0.2905315  0.17551766      -0.35015832  0.2163430
    ## salario(USD)     -0.1849272  0.26855643      -0.39388417 -0.2248153
    ##                  Horas de entreno  salario(USD)
    ## edad                 0.2905314863 -0.1849271750
    ## peso(Kg)             0.1755176553  0.2685564305
    ## Partidos jugados    -0.3501583163 -0.3938841721
    ## Lesiones             0.2163429610 -0.2248153169
    ## Horas de entreno     1.0000000000 -0.0003874514
    ## salario(USD)        -0.0003874514  1.0000000000

``` r
r <- as.matrix(correlaciones$r)
```

Alfa= 0,05 P value &gt; alfa: no se rechaza H0 P value &lt; alfa: se
rechaza H0, estamos en esta situacion, por lo tanto, si es aplicable el
analisis factorial Exploratorio

# Indicadores de aplicabilidad del AFE (Bondad del ajuste)

## contraste de esfericidad de Bartlett

H0: las correlaciones teoricas entre cada par de variables es nulo H1:
las correlaciones teoricas entre cada par de variables no es nulo

P value &gt; alfa: no se aplica el AFE (no se rechaza H0) P value &lt;
alfa: si se aplica el AFE (se rechaza H0)

``` r
dim(datost) #tamaño de la muestra= 30 personas
```

    ## [1] 30  7

``` r
cortest.bartlett(r, n= 30)
```

    ## $chisq
    ## [1] 25.69402
    ## 
    ## $p.value
    ## [1] 0.04136455
    ## 
    ## $df
    ## [1] 15

como el P value es menor a alfa, se rechaza la H0, por lo tanto, las
correlaciones teoricas entre cada par de variables es nulo, es decir, si
es aplicable el analisis factorial exploratorio(AFE).

## MEDIDA DE ADECUACION MUESTRAL DE KAISER, MEYER Y OKLIN (KMO)

estudia variable por variable, si son o no aceptadas en el modelo para
hacer AFE.(que variables elimino o mantengo) se mantiene una variable en
el modelo, si el KMO es igual o mayor a 0,7. se elimina una variable del
modelo, si el KMO es menor a 0,7

``` r
KMO(r)
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = r)
    ## Overall MSA =  0.37
    ## MSA for each item = 
    ##             edad         peso(Kg) Partidos jugados         Lesiones 
    ##             0.35             0.41             0.37             0.32 
    ## Horas de entreno     salario(USD) 
    ##             0.30             0.53

KMO= 0,37 el modelo es unacceptable

``` r
fa.parallel(r, fm= "pa", n.obs = 30, ylabel = "E igenvalues")
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

![](Prueba-Final_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  0

con el metodo de los ejes principales se extraeria 0 factor \#\# metodo
de las componentes principales

``` r
fa.parallel(r, fm= "pc", n.obs = 30, ylabel = "E ingevalues")
```

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

![](Prueba-Final_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  0

con el metodo de las componentes principales se recomienda extraer 0
factor

## Metodo de la maxima verosimilitud

``` r
fa.parallel(r, fm= "ml", n.obs = 30, ylabel = "E igenvalues")
```

![](Prueba-Final_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  0

con el metodo de la maxima verosimilitud se recomienda extraer 0 factor

# Metodo de extraccion de factores

## metodo de analisis de los componentes principales(ACP)

acp&lt;- principal(r, nfactors=1, rotate= “none”)

``` r
library(psych)
acp<- principal(r, nfactors=1, rotate= "none")
acp
```

    ## Principal Components Analysis
    ## Call: principal(r = r, nfactors = 1, rotate = "none")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                    PC1    h2   u2 com
    ## edad              0.16 0.024 0.98   1
    ## peso(Kg)          0.58 0.336 0.66   1
    ## Partidos jugados -0.69 0.482 0.52   1
    ## Lesiones         -0.50 0.253 0.75   1
    ## Horas de entreno  0.38 0.147 0.85   1
    ## salario(USD)      0.70 0.491 0.51   1
    ## 
    ##                 PC1
    ## SS loadings    1.73
    ## Proportion Var 0.29
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 component is sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.21 
    ## 
    ## Fit based upon off diagonal values = 0.14

PC1: cargas factoriales de cada variable. h2: comunalidad (varianza
comun explicada) edad es explicado en un 2,4% por el factor extraido.
peso es explicada en un 33,62% por el factor extraido. partidos jugados
es explicado en un 48,18%, lesiones es explicado en un 25,29%, horas de
entreno es explicado en un 14,69% y salario en un 49,06%.

entre mas alta sea h2 es mejor el modelo. 0;1

u2: especificidad(varianza no explicada). la edad no es explicada en un
97,58%, peso pierde un 66,37%, partidos jugados un 51,81%, lesiones un
74,70%, horas de entreno 85,30% y salario un 50,93%.

mientras mas pequeño sea la especificidad es mejor el modelo. 0;1

h2 + u2= 1 comunalidad + especificidad= 1 varianza explicada + varianza
no explicada =1

ss loadings= 1.73 ( varianza explicada en valores absolutos) Proportion
Var 29% (El % que la varianza explicada representa del total).

lo “ideal” es que Proportion Var sea lo mas cercano a 1. RMSR=0.21 (raiz
cuadrada media de los residuos)

## metodo de los ejes principales o componentes principales iteradas (CPI)

``` r
cpi<- fa(r, nfactors= 1, fm= "pa", rotate= "none", n.obs = 30)
cpi
```

    ## Factor Analysis using method =  pa
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "pa")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                    PA1      h2   u2 com
    ## edad              0.01 5.1e-05 1.00   1
    ## peso(Kg)          0.34 1.2e-01 0.88   1
    ## Partidos jugados -0.58 3.4e-01 0.66   1
    ## Lesiones         -0.32 1.0e-01 0.90   1
    ## Horas de entreno  0.20 4.0e-02 0.96   1
    ## salario(USD)      0.65 4.3e-01 0.57   1
    ## 
    ##                 PA1
    ## SS loadings    1.02
    ## Proportion Var 0.17
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  0.98 with Chi Square of  25.69
    ## The degrees of freedom for the model are 9  and the objective function was  0.67 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.16 
    ## The df corrected root mean square of the residuals is  0.21 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  23.44  with prob <  0.0053 
    ## The total number of observations was  30  with Likelihood Chi Square =  17.18  with prob <  0.046 
    ## 
    ## Tucker Lewis Index of factoring reliability =  -0.358
    ## RMSEA index =  0.171  and the 90 % confidence intervals are  0.023 0.303
    ## BIC =  -13.43
    ## Fit based upon off diagonal values = 0.48
    ## Measures of factor score adequacy             
    ##                                                    PA1
    ## Correlation of (regression) scores with factors   0.78
    ## Multiple R square of scores with factors          0.61
    ## Minimum correlation of possible factor scores     0.21

Proportion Var= 17% RSMR= 0.16

## Metodo de maxima verosimilitud(MVE)

``` r
mve<- fa(r, nfactors = 1, fm= "ml", rotate= "none", n.obs= 30)
mve
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "ml")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                    ML1      h2    u2 com
    ## edad             -0.18 3.4e-02 0.966   1
    ## peso(Kg)          0.27 7.2e-02 0.928   1
    ## Partidos jugados -0.39 1.6e-01 0.844   1
    ## Lesiones         -0.23 5.1e-02 0.949   1
    ## Horas de entreno  0.00 2.3e-08 1.000   1
    ## salario(USD)      1.00 1.0e+00 0.005   1
    ## 
    ##                 ML1
    ## SS loadings    1.31
    ## Proportion Var 0.22
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  0.98 with Chi Square of  25.69
    ## The degrees of freedom for the model are 9  and the objective function was  0.65 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.17 
    ## The df corrected root mean square of the residuals is  0.22 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  25.4  with prob <  0.0026 
    ## The total number of observations was  30  with Likelihood Chi Square =  16.62  with prob <  0.055 
    ## 
    ## Tucker Lewis Index of factoring reliability =  -0.266
    ## RMSEA index =  0.165  and the 90 % confidence intervals are  0 0.298
    ## BIC =  -13.99
    ## Fit based upon off diagonal values = 0.44
    ## Measures of factor score adequacy             
    ##                                                    ML1
    ## Correlation of (regression) scores with factors   1.00
    ## Multiple R square of scores with factors          1.00
    ## Minimum correlation of possible factor scores     0.99

Proportion Var= 22% RSMR= 0.17

### Resumen

ACP: var= 29% RSMR= 0.21 CPI: Var= 17% RSMR= 0.16 MVE: Var= 22% RSMR=
0,17

¿con cual nos quedamos? aquel modelo que tenga la proportion var mas
alta y el RSMR mas pequeño.

# REPRESENTACION GRAFICA DE LOS FACTORES EXTRAIDOS

## Metodo de analisis de las componentes pricipales(ACP)

plot(acp, lables= row.names(r), cex=.7, ylim=c(-.8,.8))

## Metodo de las componentes principales iterasas(CPI)

plot(cpi, lables= row.names(r), cex=.7, ylim=c(-.8,.8))

## Metodo de la maxima verosimilitud (MVE)

plot(mve, lables= row.names(r), cex=1, ylim=c(-.8,.8))

## estas graficas solo funcionan cyando hay 2 factores extraidos, en este caso de obtuvo 0 factores

# Obtencion de las puntuaciones factoriales

## Metodo de analisis de las componentes principales iteradas (ACP)

``` r
acp1<- principal(datost[,2:7], nfactors = 1, rotate= "none", scores= T)
acp1$scores
```

    ##               PC1
    ##  [1,] -0.41493092
    ##  [2,] -1.33434722
    ##  [3,] -1.17581779
    ##  [4,]  1.59504205
    ##  [5,]  0.12515715
    ##  [6,] -0.43780857
    ##  [7,] -0.46887893
    ##  [8,]  0.62807279
    ##  [9,] -0.33378746
    ## [10,] -2.27698741
    ## [11,]  1.76692943
    ## [12,]  0.54802994
    ## [13,]  0.63148111
    ## [14,]  0.41343222
    ## [15,] -1.37836884
    ## [16,] -0.99665999
    ## [17,]  1.98039381
    ## [18,]  1.01787118
    ## [19,]  1.50380066
    ## [20,] -1.09110961
    ## [21,] -0.77941087
    ## [22,] -0.12932246
    ## [23,]  0.18691410
    ## [24,]  0.28257310
    ## [25,] -0.09643498
    ## [26,] -0.74698876
    ## [27,]  0.29675440
    ## [28,]  0.53393157
    ## [29,]  0.16711668
    ## [30,] -0.01664639

``` r
puntuacionesfactoriales_acp<-acp1$scores
puntuacionesfactoriales_acp<-as.data.frame(puntuacionesfactoriales_acp)
```

## METODO DE LAS COMPONENTES PRINCIPALES ITERADAS (CPI)

``` r
cpi1<- fa(datost[,2:7], nfactors = 1, fm="pa", rotate= "none", n.obs=30, scores="regression")
cpi1$scores
```

    ##               PA1
    ##  [1,] -0.26266471
    ##  [2,] -1.33615009
    ##  [3,] -0.92066542
    ##  [4,]  1.31022310
    ##  [5,]  0.06042560
    ##  [6,] -0.20559017
    ##  [7,] -0.32308294
    ##  [8,]  0.71021510
    ##  [9,] -0.15923143
    ## [10,] -1.60192731
    ## [11,]  1.36668644
    ## [12,]  0.28587302
    ## [13,]  0.78958630
    ## [14,]  0.19872335
    ## [15,] -0.99083641
    ## [16,] -0.88282377
    ## [17,]  1.35921364
    ## [18,]  0.35978042
    ## [19,]  0.87543745
    ## [20,] -1.10265354
    ## [21,] -0.47038895
    ## [22,] -0.07328279
    ## [23,]  0.41200565
    ## [24,] -0.06023391
    ## [25,] -0.22737300
    ## [26,] -0.56774772
    ## [27,]  0.33325660
    ## [28,]  0.53143438
    ## [29,] -0.06616649
    ## [30,]  0.65795761

``` r
puntfact_cpi<-cpi1$scores
puntfact_cpi<-as.data.frame(puntfact_cpi)
```

## METODO DE LA MAXIMA VEROSIMILITUD

``` r
mve1<- fa(datost[,2:7], nfactors = 1, fm="ml", rotate= "none", n.obs=30, scores="regression")
mve1$scores
```

    ##               ML1
    ##  [1,] -0.44222693
    ##  [2,] -2.04632122
    ##  [3,] -0.55226594
    ##  [4,]  1.16727917
    ##  [5,]  0.62717872
    ##  [6,] -0.12398118
    ##  [7,] -0.76174290
    ##  [8,]  0.63050541
    ##  [9,] -0.22851938
    ## [10,] -1.51713735
    ## [11,]  1.37671903
    ## [12,] -0.33274987
    ## [13,]  1.05558517
    ## [14,]  0.19893004
    ## [15,] -0.55183854
    ## [16,] -1.29631530
    ## [17,]  0.84812344
    ## [18,] -0.75808954
    ## [19,]  0.84219471
    ## [20,] -1.83290556
    ## [21,] -0.65592131
    ## [22,]  0.52004418
    ## [23,]  1.48150168
    ## [24,]  0.40878495
    ## [25,] -0.86627483
    ## [26,] -0.01741188
    ## [27,]  0.73517331
    ## [28,]  0.30794281
    ## [29,] -0.22996489
    ## [30,]  2.01370400

``` r
puntfact_mve<-mve1$scores
puntfact_mve<-as.data.frame(puntfact_mve)
```

# OBTENCION DE LOS FACTORES EXTRAIDOS

``` r
factor.scores(r, acp, method = "Thurstone")
```

    ## $scores
    ## NULL
    ## 
    ## $weights
    ##                          PC1
    ## edad              0.08971433
    ## peso(Kg)          0.33464119
    ## Partidos jugados -0.40057861
    ## Lesiones         -0.29026241
    ## Horas de entreno  0.22122145
    ## salario(USD)      0.40423930
    ## 
    ## $r.scores
    ##     PC1
    ## PC1   1
    ## 
    ## $R2
    ## [1] 1

Z1=0,089edad+0,33 peso-0,40 partidos jugados-0,29 lesiones+0,22 horas de
entreno+ 0,40 salario

# AGREGAR FACTOR EXTRAIDO(PUNTUACIONES FACTORIALES)EN EL DATA FRAME ORIGINAL

``` r
datos_puntuaciones<- c(datos, puntuacionesfactoriales_acp)
datos_puntuaciones<- as.data.frame(datos_puntuaciones)
```

# GUARDAR EL DATA FRAME" DATOS\_PUNTUACIONES"

``` r
setwd("C:/Users/user/Desktop") #define donde guardaras tu archivo excel cvs
write.table(datos_puntuaciones, file="encuesta.cvs", sep= ";", row.names=F, dec=",")
```
