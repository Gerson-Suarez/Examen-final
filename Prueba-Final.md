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

\#NORMALIDAD multivariante H0: Normalidad multivariante H1: No
normalidad multivariante confianza= 95% Alfa= 5% = 0,05 P value &gt;
alfa: no se rechaza la H0 (Normalidad) P value &lt; alfa: se rechaza la
H0 (No normalidad)

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

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
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
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

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

## Metodo paralelo con interacciones
