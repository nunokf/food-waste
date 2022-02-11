Food Waste
================
Nuno Fernandes

## Preprocessing

### read.csv

``` r
setwd("~/2021/food waste")
food<-read.csv("Comportamento_Alimentar.csv")
```

### packages

``` r
packages <- c("glmm", "lmerTest", "lme4", "Hmisc", "lattice", "ggplot2", "sjPlot")

installed_packages <- packages %in% row.names(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed.packages])
}

lapply(packages, library, character.only = TRUE)
```

### coding dimension

``` r
food$Dimension[food$Dimension==1]<-"aStatus"
food$Dimension[food$Dimension==2]<-"Environment"
food$Dimension[food$Dimension==3]<-"Economic"
food$Dimension[food$Dimension==4]<-"Social"
```

### coding temporal framing

``` r
food$Tem_Val[food$Tem_Val==0]<-"P+F-"
food$Tem_Val[food$Tem_Val==1]<-"P-F+"
```

### coding dv:food choices

``` r
food$escolha[food$escolha==0]<-"excess"
food$escolha[food$escolha==1]<-"enough"
```

### as.factor

``` r
food$id<-food$ï..id 
food$id<-as.factor(food$id)
food$Scenario<-as.factor(food$Scenario)
food$Dimension<-as.factor(food$Dimension)
food$Tem_Val<-as.factor(food$Tem_Val)
food$escolha<-as.factor(food$escolha)
```

## Statistical Analysis

``` r
m1<- glmer(escolha ~ Dimension*Tem_Val + Q_comida_fora + (1|id/Scenario),
           data = food, family = binomial, control = glmerControl(optimizer = "bobyqa"))

m2<- glmer(escolha ~ Dimension*Q_consumo_sustentavel +  Q_comida_fora + (1|id/Scenario),
           data = food, family = binomial, control = glmerControl(optimizer = "bobyqa"))
```

### summary m1

``` r
summary(m1)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: escolha ~ Dimension * Tem_Val + Q_comida_fora + (1 | id/Scenario)
    ##    Data: food
    ## Control: glmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1646.9   1708.3   -812.4   1624.9     1957 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2686  0.1074  0.1312  0.4484  1.8215 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance  Std.Dev. 
    ##  Scenario:id (Intercept) 2.723e-10 0.0000165
    ##  id          (Intercept) 4.986e+00 2.2328648
    ## Number of obs: 1968, groups:  Scenario:id, 1968; id, 82
    ## 
    ## Fixed effects:
    ##                                  Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)                        0.8627     1.0101   0.854   0.3931   
    ## DimensionEconomic                  0.5980     0.2547   2.348   0.0189 * 
    ## DimensionEnvironment               0.7175     0.2382   3.012   0.0026 **
    ## DimensionSocial                    0.5099     0.2583   1.974   0.0484 * 
    ## Tem_ValP+F-                        0.3736     0.2542   1.470   0.1417   
    ## Q_comida_fora                      0.3295     0.3668   0.898   0.3690   
    ## DimensionEconomic:Tem_ValP+F-     -0.4398     0.3642  -1.208   0.2272   
    ## DimensionEnvironment:Tem_ValP+F-  -0.5740     0.3449  -1.664   0.0961 . 
    ## DimensionSocial:Tem_ValP+F-       -0.1486     0.3796  -0.391   0.6955   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                 (Intr) DmnsnEc DmnsnEn DmnsnS T_VP+F Q_cmd_ DmnsnEc:T_VP+F-
    ## DimnsnEcnmc     -0.113                                                     
    ## DmnsnEnvrnm     -0.115  0.475                                              
    ## DimensinScl     -0.108  0.431   0.465                                      
    ## Tem_ValP+F-     -0.106  0.427   0.460   0.418                              
    ## Q_comida_fr     -0.950  0.006   0.004   0.003 -0.001                       
    ## DmnsnEc:T_VP+F-  0.075 -0.685  -0.318  -0.294 -0.691 -0.001                
    ## DmnsnEn:T_VP+F-  0.078 -0.315  -0.671  -0.312 -0.731  0.000  0.508         
    ## DmS:T_VP+F-      0.075 -0.287  -0.309  -0.672 -0.662 -0.003  0.463         
    ##                 DmnsnEn:T_VP+F-
    ## DimnsnEcnmc                    
    ## DmnsnEnvrnm                    
    ## DimensinScl                    
    ## Tem_ValP+F-                    
    ## Q_comida_fr                    
    ## DmnsnEc:T_VP+F-                
    ## DmnsnEn:T_VP+F-                
    ## DmS:T_VP+F-      0.489         
    ## optimizer (bobyqa) convergence code: 0 (OK)
    ## boundary (singular) fit: see ?isSingular

``` r
anova(m1)
```

    ## Analysis of Variance Table
    ##                   npar Sum Sq Mean Sq F value
    ## Dimension            3 8.4882 2.82940  2.8294
    ## Tem_Val              1 0.2316 0.23164  0.2316
    ## Q_comida_fora        1 0.7942 0.79421  0.7942
    ## Dimension:Tem_Val    3 3.2658 1.08861  1.0886

### Confidence Intervals m1

``` r
confint(m1, method = "Wald")
```

    ##                                        2.5 %    97.5 %
    ## .sig01                                    NA        NA
    ## .sig02                                    NA        NA
    ## (Intercept)                      -1.11709453 2.8424209
    ## DimensionEconomic                 0.09881194 1.0972836
    ## DimensionEnvironment              0.25055107 1.1844136
    ## DimensionSocial                   0.00362904 1.0160901
    ## Tem_ValP+F-                      -0.12465513 0.8718544
    ## Q_comida_fora                    -0.38940451 1.0484174
    ## DimensionEconomic:Tem_ValP+F-    -1.15374751 0.2740817
    ## DimensionEnvironment:Tem_ValP+F- -1.24999460 0.1020506
    ## DimensionSocial:Tem_ValP+F-      -0.89269164 0.5954690

### table m1 - escolha \~ Dimension \* Tem\_Val + Q\_comida\_fora + (1 \| id/Scenario)

``` r
tab_model(m1)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
escolha
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.37
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.33 – 17.16
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.393
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Dimension \[Economic\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.82
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.10 – 3.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.019</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Dimension \[Environment\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.28 – 3.27
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.003</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Dimension \[Social\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.67
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.00 – 2.76
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.048</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Tem\_Val \[P+F-\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.45
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.88 – 2.39
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.142
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Q\_comida\_fora
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.39
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.68 – 2.85
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.369
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Dimension \[Economic\] \*<br>Tem\_Val \[P+F-\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.64
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.32 – 1.32
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.227
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Dimension \[Environment\] \*<br>Tem\_Val \[P+F-\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.56
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.29 – 1.11
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.096
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Dimension \[Social\] \*<br>Tem\_Val \[P+F-\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.86
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.41 – 1.81
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.695
</td>
</tr>
<tr>
<td colspan="4" style="font-weight:bold; text-align:left; padding-top:.8em;">
Random Effects
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
σ<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
3.29
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>Scenario:id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.00
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
4.99
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
ICC
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.60
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>Scenario</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
24
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
82
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
1968
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
Marginal R<sup>2</sup> / Conditional R<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.013 / 0.608
</td>
</tr>
</table>

### summary m2

``` r
summary(m2)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: escolha ~ Dimension * Q_consumo_sustentavel + Q_comida_fora +  
    ##     (1 | id/Scenario)
    ##    Data: food
    ## Control: glmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1632.8   1693.4   -805.4   1610.8     1813 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8335  0.1154  0.1653  0.4891  1.7284 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Scenario:id (Intercept) 0.000    0.000   
    ##  id          (Intercept) 4.173    2.043   
    ## Number of obs: 1824, groups:  Scenario:id, 1824; id, 76
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                                -0.24793    2.08073  -0.119    0.905
    ## DimensionEconomic                           0.79408    1.03805   0.765    0.444
    ## DimensionEnvironment                        0.22649    0.98811   0.229    0.819
    ## DimensionSocial                             0.18549    1.06175   0.175    0.861
    ## Q_consumo_sustentavel                       0.12989    0.29782   0.436    0.663
    ## Q_comida_fora                               0.44336    0.36640   1.210    0.226
    ## DimensionEconomic:Q_consumo_sustentavel    -0.07494    0.19284  -0.389    0.698
    ## DimensionEnvironment:Q_consumo_sustentavel  0.04270    0.18367   0.232    0.816
    ## DimensionSocial:Q_consumo_sustentavel       0.05148    0.19977   0.258    0.797
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) DmnsnEc DmnsnEn DmnsnS Q_cns_ Q_cmd_ DmnsnEc:Q__ DmnsnEn:Q__
    ## DimnsnEcnmc -0.225                                                             
    ## DmnsnEnvrnm -0.233  0.483                                                      
    ## DimensinScl -0.222  0.448   0.481                                              
    ## Q_cnsm_sstn -0.887  0.285   0.299   0.283                                      
    ## Q_comida_fr -0.681  0.004  -0.004   0.001  0.291                               
    ## DmnsnEc:Q__  0.220 -0.984  -0.474  -0.440 -0.289 -0.003                        
    ## DmnsnEn:Q__  0.227 -0.474  -0.984  -0.470 -0.303  0.006  0.481                 
    ## DmnsnSc:Q__  0.215 -0.435  -0.465  -0.984 -0.284  0.000  0.442       0.470     
    ## optimizer (bobyqa) convergence code: 0 (OK)
    ## boundary (singular) fit: see ?isSingular

``` r
anova(m2)
```

    ## Analysis of Variance Table
    ##                                 npar Sum Sq Mean Sq F value
    ## Dimension                          3 8.4155 2.80517  2.8052
    ## Q_consumo_sustentavel              1 0.0140 0.01403  0.0140
    ## Q_comida_fora                      1 1.4356 1.43558  1.4356
    ## Dimension:Q_consumo_sustentavel    3 0.4859 0.16196  0.1620

### Confidence Intervals m2

``` r
confint(m2, method = "Wald")
```

    ##                                                 2.5 %    97.5 %
    ## .sig01                                             NA        NA
    ## .sig02                                             NA        NA
    ## (Intercept)                                -4.3260884 3.8302251
    ## DimensionEconomic                          -1.2404621 2.8286153
    ## DimensionEnvironment                       -1.7101639 2.1631492
    ## DimensionSocial                            -1.8954935 2.2664830
    ## Q_consumo_sustentavel                      -0.4538305 0.7136006
    ## Q_comida_fora                              -0.2747797 1.1614997
    ## DimensionEconomic:Q_consumo_sustentavel    -0.4529023 0.3030244
    ## DimensionEnvironment:Q_consumo_sustentavel -0.3172890 0.4026832
    ## DimensionSocial:Q_consumo_sustentavel      -0.3400611 0.4430140

### table m2 - escolha \~ Dimension\*Q\_consumo\_sustentavel + Q\_comida\_fora + (1\|id/Scenario)

``` r
tab_model(m2)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
escolha
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.78
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.01 – 46.07
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.905
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Dimension \[Economic\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.21
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.29 – 16.92
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.444
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Dimension \[Environment\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.25
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.18 – 8.70
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.819
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Dimension \[Social\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.20
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.15 – 9.65
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.861
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Q\_consumo\_sustentavel
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.14
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.64 – 2.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.663
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Q\_comida\_fora
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.56
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.76 – 3.19
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.226
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Dimension \[Economic\] \*<br>Q\_consumo\_sustentavel
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.93
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.64 – 1.35
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.698
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Dimension \[Environment\] \*<br>Q\_consumo\_sustentavel
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.73 – 1.50
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.816
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Dimension \[Social\] \*<br>Q\_consumo\_sustentavel
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.71 – 1.56
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.797
</td>
</tr>
<tr>
<td colspan="4" style="font-weight:bold; text-align:left; padding-top:.8em;">
Random Effects
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
σ<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
3.29
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>Scenario:id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.00
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
4.17
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>Scenario</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
24
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
76
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
1824
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
Marginal R<sup>2</sup> / Conditional R<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.038 / NA
</td>
</tr>
</table>

## Data Visualization

### Probability of cooking just enough food (violin plot)

``` r
food<-read.csv("Comportamento_Alimentar.csv")
food$Dimension[food$Dimension==1]<-"Status"
food$Dimension[food$Dimension==2]<-"Environmental"
food$Dimension[food$Dimension==3]<-"Economic"
food$Dimension[food$Dimension==4]<-"Social"

ggplot(data = food, aes(x = Dimension, y = PredictedProbability_01)) +
  ylab("Probability of choosing to cook just enough food") + xlab("Dimension") +
  theme(axis.title.x = element_text(color="black", size = 15),
        axis.title.y = element_text(color="black", size = 15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13)) +
  scale_x_discrete(name ="Dimension", 
                   limits=c("Status","Environmental","Economic","Social")) +
  ylim(0.00, 1.00) +
  geom_violin(scale = "area", adjust = .8) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = .5), 
               geom="errorbar", color="red", width = .1) +
  stat_summary(fun.y=mean, geom="point", color="red") +
  geom_smooth(method = "glmer") 
```

![](index_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Probability of cooking just enough food (jitter) - <https://doi.org/10.1002/bdm.2274>

``` r
apatheme <- theme_bw(base_size = 12)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

fixd.plot <- ggplot(food, aes(x = Dimension, y = PredictedProbability_01))  +
  geom_point(position = position_jitter(width = .1), alpha = .2) +
  # geom_point(size = 1, colour = "grey60", alpha = .5, position = position_jitter(width = .3, height = 0)) +
  #geom_errorbar(data = food, aes(x = diff, y = m, ymin = m -sd, ymax = m + sd), 
                #position = position_nudge(x = -.2), width = .2) +
scale_x_discrete(name ="Dimension", 
                   limits=c("Status","Environmental","Economic","Social")) +
  
  #confirm if this is to remove or not
  #geom_point(data = food, aes(y = PredictedProbability_01), size = 4, position = position_nudge(x = -.2))  + 
  #error bar
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = .5), 
               geom="errorbar", color="red", width = .1) + 
  stat_summary(fun=mean, geom="point", color="red") +
  
    ylab("Probability of cooking just enough food") + xlab("Dimension") + apatheme
ggsave("figure4.pdf", width=4, height=4, unit='in', dpi=300)

fixd.plot
```

![](index_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
