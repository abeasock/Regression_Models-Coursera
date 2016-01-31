# Regression Models Course Project
Amber Beasock  
31 January 2016  

----------------------------------------------------------------------------------

### Effects of manual and automatic transmission on MPG
  
### Executive Summary

In this project, we analyze the `mtcars` (Motor Trend Car Road Tests) data set and explore the relationship between a set of variables and miles per gallon (MPG). The data was extracted from the 1974 *Motor Trend* US Magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973-74 models). The main focus of the project is to answer the following questions:

- Is an automatic or manual transmission better for MPG
- Quantify the MPG difference between automatic and manual transmissions

In our analysis, we use exploratory data analysis and regression models to explore the impact of manual transmissons (am=1) and automatic transmissions(am=0) on MPG (Miles/(US) gallon). 

Our analysis concludes that manual transmission is better for MPG. The mean for manual is about 7 mpg more than the mean for automatic transmission. Linear regression analysis shows that manual transmission cars get an average of 2.94 mpg more than automatics when weight and 1/4 mile time are held constant. 

### Data Processing

```r
library(corrplot)
```

Load the data set `mtcars`. 
Use `?mtcars` to view the R documentation on the data set to find out more information. From this we learn the mapping of the variable `am`, 0=automatic, 1=manual. 
Transform some variables from numeric to factor.

```r
data(mtcars)
head(mtcars,3) #Preview of the dataset
```

```
##                mpg cyl disp  hp drat    wt  qsec vs am gear carb
## Mazda RX4     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
## Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
## Datsun 710    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
```

```r
dim(mtcars)
```

```
## [1] 32 11
```

```r
mtcars_num <- mtcars #Need the variables as numeric to check correlation
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- factor(mtcars$am, labels=c("automatic","manual"))
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)
```

### Exploratory Data Analysis

The plots created in the exploratory data analysis can be found in the **Appendix: Figures** section at the end. According to the box plot `Figure 1`, manual transmission yields a higher MPG. In `Figure 2`, a pair graph is used to generate scatterplots to show the relationship between variables. A correlation matrix is also used to further examine the correlation between variables. `Figure 3` shows the strength of correlation between all variables.

```r
x <- mtcars_num[1]
y <- mtcars_num[2:11] 
cor(x, y)
```

```
##           cyl       disp         hp      drat         wt     qsec
## mpg -0.852162 -0.8475514 -0.7761684 0.6811719 -0.8676594 0.418684
##            vs        am      gear       carb
## mpg 0.6640389 0.5998324 0.4802848 -0.5509251
```
There are stronger correlations between variables: "cyl", "disp", "hp", and "wt". 

### Statistical Inference

A two sample T-test is performed on the two types of transmissions (manual and automatic) to test the null hypothesis, which is the MPG of the automatic and manual transmissions are from the same distrubution. 

```r
t_test <- t.test(mpg ~ am, data=mtcars)
t_test$p.value 
```

```
## [1] 0.001373638
```

```r
t_test$estimate 
```

```
## mean in group automatic    mean in group manual 
##                17.14737                24.39231
```

The p-value is 0.0014, so we reject the null hypothesis. The mean of manual transmission is about 7 MPG more than that of an automatic transmission.

### Regression Analysis

In the first model, all variables are included as predictors of MPG.  

```r
initModel <- lm(mpg ~ ., data=mtcars)
summary(initModel) 
```

```
## 
## Call:
## lm(formula = mpg ~ ., data = mtcars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.5087 -1.3584 -0.0948  0.7745  4.6251 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 23.87913   20.06582   1.190   0.2525  
## cyl6        -2.64870    3.04089  -0.871   0.3975  
## cyl8        -0.33616    7.15954  -0.047   0.9632  
## disp         0.03555    0.03190   1.114   0.2827  
## hp          -0.07051    0.03943  -1.788   0.0939 .
## drat         1.18283    2.48348   0.476   0.6407  
## wt          -4.52978    2.53875  -1.784   0.0946 .
## qsec         0.36784    0.93540   0.393   0.6997  
## vs1          1.93085    2.87126   0.672   0.5115  
## ammanual     1.21212    3.21355   0.377   0.7113  
## gear4        1.11435    3.79952   0.293   0.7733  
## gear5        2.52840    3.73636   0.677   0.5089  
## carb2       -0.97935    2.31797  -0.423   0.6787  
## carb3        2.99964    4.29355   0.699   0.4955  
## carb4        1.09142    4.44962   0.245   0.8096  
## carb6        4.47757    6.38406   0.701   0.4938  
## carb8        7.25041    8.36057   0.867   0.3995  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.833 on 15 degrees of freedom
## Multiple R-squared:  0.8931,	Adjusted R-squared:  0.779 
## F-statistic:  7.83 on 16 and 15 DF,  p-value: 0.000124
```
The residual standard error is 2.833 on 15 degrees of freedom. The Adjusted R-Squared value is 0.779, which is interpreted to mean this model can explain about 77.9% of the variance of the MPG. All of the coefficients are above the 0.05 significant level, so we can conclude none of the coefficients are significant. 

Backward elimination can be used to used to find the most statistically significant predictor variables. The `step()` function starts with all the predictors in the model and then drops the one with the largest p-value (least significant). Then the model is re-fitted and this is repeated until only variables that are statistically significant remain.

```r
bestModel <- step(initModel, k=log(nrow(mtcars))) 
```

```
## Start:  AIC=101.32
## mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb
## 
##        Df Sum of Sq    RSS     AIC
## - carb  5   13.5989 134.00  87.417
## - gear  2    3.9729 124.38  95.428
## - cyl   2   10.9314 131.33  97.170
## - am    1    1.1420 121.55  98.157
## - qsec  1    1.2413 121.64  98.183
## - drat  1    1.8208 122.22  98.335
## - vs    1    3.6299 124.03  98.806
## - disp  1    9.9672 130.37 100.400
## <none>              120.40 101.321
## - wt    1   25.5541 145.96 104.014
## - hp    1   25.6715 146.07 104.040
## 
## Step:  AIC=87.42
## mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear
## 
##        Df Sum of Sq    RSS    AIC
## - gear  2    5.0215 139.02 81.662
## - cyl   2   12.5642 146.57 83.353
## - disp  1    0.9934 135.00 84.187
## - drat  1    1.1854 135.19 84.233
## - vs    1    3.6763 137.68 84.817
## - qsec  1    5.2634 139.26 85.184
## - am    1   11.9255 145.93 86.679
## <none>              134.00 87.417
## - wt    1   19.7963 153.80 88.360
## - hp    1   22.7935 156.79 88.978
## 
## Step:  AIC=81.66
## mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am
## 
##        Df Sum of Sq    RSS    AIC
## - cyl   2   10.4247 149.45 77.045
## - drat  1    0.9672 139.99 78.418
## - disp  1    1.5483 140.57 78.551
## - vs    1    2.1829 141.21 78.695
## - qsec  1    3.6324 142.66 79.022
## <none>              139.02 81.662
## - am    1   16.5665 155.59 81.799
## - hp    1   18.1768 157.20 82.129
## - wt    1   31.1896 170.21 84.674
## 
## Step:  AIC=77.04
## mpg ~ disp + hp + drat + wt + qsec + vs + am
## 
##        Df Sum of Sq    RSS    AIC
## - vs    1     0.645 150.09 73.717
## - drat  1     2.869 152.32 74.187
## - disp  1     9.111 158.56 75.473
## - qsec  1    12.573 162.02 76.164
## - hp    1    13.929 163.38 76.431
## <none>              149.45 77.045
## - am    1    20.457 169.91 77.684
## - wt    1    60.936 210.38 84.523
## 
## Step:  AIC=73.72
## mpg ~ disp + hp + drat + wt + qsec + am
## 
##        Df Sum of Sq    RSS    AIC
## - drat  1     3.345 153.44 70.956
## - disp  1     8.545 158.64 72.023
## - hp    1    13.285 163.38 72.965
## <none>              150.09 73.717
## - am    1    20.036 170.13 74.261
## - qsec  1    25.574 175.67 75.286
## - wt    1    67.572 217.66 82.146
## 
## Step:  AIC=70.96
## mpg ~ disp + hp + wt + qsec + am
## 
##        Df Sum of Sq    RSS    AIC
## - disp  1     6.629 160.07 68.844
## - hp    1    12.572 166.01 70.011
## <none>              153.44 70.956
## - qsec  1    26.470 179.91 72.583
## - am    1    32.198 185.63 73.586
## - wt    1    69.043 222.48 79.380
## 
## Step:  AIC=68.84
## mpg ~ hp + wt + qsec + am
## 
##        Df Sum of Sq    RSS    AIC
## - hp    1     9.219 169.29 67.170
## <none>              160.07 68.844
## - qsec  1    20.225 180.29 69.186
## - am    1    25.993 186.06 70.193
## - wt    1    78.494 238.56 78.147
## 
## Step:  AIC=67.17
## mpg ~ wt + qsec + am
## 
##        Df Sum of Sq    RSS    AIC
## <none>              169.29 67.170
## - am    1    26.178 195.46 68.306
## - qsec  1   109.034 278.32 79.614
## - wt    1   183.347 352.63 87.187
```

```r
summary(bestModel)  
```

```
## 
## Call:
## lm(formula = mpg ~ wt + qsec + am, data = mtcars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.4811 -1.5555 -0.7257  1.4110  4.6610 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   9.6178     6.9596   1.382 0.177915    
## wt           -3.9165     0.7112  -5.507 6.95e-06 ***
## qsec          1.2259     0.2887   4.247 0.000216 ***
## ammanual      2.9358     1.4109   2.081 0.046716 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.459 on 28 degrees of freedom
## Multiple R-squared:  0.8497,	Adjusted R-squared:  0.8336 
## F-statistic: 52.75 on 3 and 28 DF,  p-value: 1.21e-11
```
The best model shows that "wt", "qsec", and "am" are statistically significant variables. The Adjusted R-Squared value is 0.8336, which is interpreted to mean this model can explain about 83.36% of the variance of the MPG. 

Since the focus of this project is on the relationship between transmission and MPG, the next model will fit transmission as a predictor of MPG.

```r
mpg_amModel <- lm(mpg ~ am, data=mtcars)
summary(mpg_amModel) 
```

```
## 
## Call:
## lm(formula = mpg ~ am, data = mtcars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.3923 -3.0923 -0.2974  3.2439  9.5077 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   17.147      1.125  15.247 1.13e-15 ***
## ammanual       7.245      1.764   4.106 0.000285 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.902 on 30 degrees of freedom
## Multiple R-squared:  0.3598,	Adjusted R-squared:  0.3385 
## F-statistic: 16.86 on 1 and 30 DF,  p-value: 0.000285
```
This model shows that a car with an automatic transmission has an average of 17.147 mpg, and manual transmission increases mpg by 7.245. However, this model has an Adjusted R-Squared of 0.3385, which means the model can only explain about 33.85% of the variance of the MPG. Other variables should be added in to get a higher Adjusted R-Squared value. 

Lastly, let's compare the model with only "am" as the predictor variable with the best model obtained through backwards elimination.

```r
anova <- anova(mpg_amModel, bestModel)
```
This results in a p-value of $1.5504951\times 10^{-9}$, which is highly significant. Therefore, we reject the null hypothesis that variables "wt", "qsec", and "am" do not contribute to the prediction of mpg. 

The best fitted model is `lm(formula = mpg ~ wt + qsec + am, data = mtcars)`

```r
confint(bestModel)
```

```
##                   2.5 %    97.5 %
## (Intercept) -4.63829946 23.873860
## wt          -5.37333423 -2.459673
## qsec         0.63457320  1.817199
## ammanual     0.04573031  5.825944
```

```r
summary(bestModel)$coef 
```

```
##              Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)  9.617781  6.9595930  1.381946 1.779152e-01
## wt          -3.916504  0.7112016 -5.506882 6.952711e-06
## qsec         1.225886  0.2886696  4.246676 2.161737e-04
## ammanual     2.935837  1.4109045  2.080819 4.671551e-02
```
This model shows that when "wt" and "qsec" remain constant, manual transmission cars get an average of 2.94 more MPG than those with automatic transmission.

### Residual Plots & Diagnostics

Refer to `Figure 4` in the **Appendix: Figures** section for the residual plots. Interpretation of the residual plots:

- The Residuals vs Fitted plot - the points are randomly scattered on the plot confirming the independence condition.
- The Normal Q-Q plot - the majority of points fall on the line indicating normal distribution of the residuals.
- The Scale-Location plot - there is a random band around the line with no clear pattern indicating constant variance.
- The Residual vs Leverage plot - there are some outliers (leverage points) that may indicate values of increased leverage of outliers.

Next, regression diagnostics can be used to further investigate our model. 

The hatvalues() function is used to find values far from the average (usually 2-3x the average) because these may have substantial influence on the regression parameters.

```r
hv_mean<- mean(hatvalues(bestModel))
hv <- hatvalues(bestModel)
tail(sort(hv),3)
```

```
##   Chrysler Imperial Lincoln Continental            Merc 230 
##           0.2296338           0.2642151           0.2970422
```
The average hatvalue is 0.125, and from the above step we get the cars that are about 2 times the average or greater. These are the same cars that we see mentioned on the residual plots.

### Conclusion

Cars with manual transmission get better miles per gallon compared to those with automatic transmission. 

- The t-test shows that manual transmission gets an average of 7 MPG more than cars with automatic transmission.
- Several linear regression models were fitted to evaluate different aspects that could impact MPG. The best fitted model `lm(formula = mpg ~ wt + qsec + am, data = mtcars)` showed that when "wt" (Weigth (lb/1000)) and "qsec" (1/4 mile time) remain constant, manual transmission cars get an average of 2.94 more MPG than those with automatic transmission.

### Appendix: Figures

**Figure 1 - Boxplot of MPG vs Transmission**

```r
boxplot(mtcars$mpg~mtcars$am, xlab="Transmission", ylab="MPG", main="Figure 1: Boxplot", col=c("blue","green"))
```

![](./regression_model_files/figure-html/unnamed-chunk-11-1.png) 

**Figure 2 - Pair Graph of Motor Trend Car Road Tests**

```r
pairs(mpg ~ ., mtcars, panel=panel.smooth, main="Figure 2: Pair Graph")
```

![](./regression_model_files/figure-html/unnamed-chunk-12-1.png) 

**Figure 3 - Correlation Plot**

```r
cor2 <- cor(mtcars_num)
corrplot(cor2, method="color")
```

![](./regression_model_files/figure-html/unnamed-chunk-13-1.png) 

**Figure 4 - Residual Plots**

```r
par(mfrow=c(2,2))
plot(bestModel)
```

![](./regression_model_files/figure-html/unnamed-chunk-14-1.png) 
