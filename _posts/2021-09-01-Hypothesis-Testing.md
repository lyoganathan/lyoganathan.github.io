---
layout: single
title: "Hypothesis Testing With Regression"
excerpt: "Comparing regression and t-test when you have a continous Y and a categorical X"
date: "2021-09-01"
tags: [regression, categorical, hypothesis, testing, multiple, categories]
values:
  show_date: true
toc: yes
---



## Intro

There's many different tests avaliable to do hypothesis testing a.k.a A/B testing. Permutation test, t-test, chi-square test, [bayesian estimation](https://jkkweb.sitehost.iu.edu/articles/Kruschke2013JEPG.pdf), [GEE](https://quoradata.quora.com/A-Robust-Statistical-Test-for-Ratio-Metrics)...

This posts goes through regression with groups and shows how to get the same result as a t-test, and why regression works for hypothesis testing. I will also try to link this to the p-values and t-values of the regression coefficients.


## Palmer penguins dataset

I've been watching a lot of Atypical recently and found out there's a dataset on penguins in Antarctica available in the _palmerpenguins_ library:


{% highlight r %}
library(palmerpenguins)
library(ggplot2)
library(scatterplot3d)
library(knitr)

kable(head(penguins,10))
{% endhighlight %}



|species |island    | bill_length_mm| bill_depth_mm| flipper_length_mm| body_mass_g|sex    | year|
|:-------|:---------|--------------:|-------------:|-----------------:|-----------:|:------|----:|
|Adelie  |Torgersen |           39.1|          18.7|               181|        3750|male   | 2007|
|Adelie  |Torgersen |           39.5|          17.4|               186|        3800|female | 2007|
|Adelie  |Torgersen |           40.3|          18.0|               195|        3250|female | 2007|
|Adelie  |Torgersen |             NA|            NA|                NA|          NA|NA     | 2007|
|Adelie  |Torgersen |           36.7|          19.3|               193|        3450|female | 2007|
|Adelie  |Torgersen |           39.3|          20.6|               190|        3650|male   | 2007|
|Adelie  |Torgersen |           38.9|          17.8|               181|        3625|female | 2007|
|Adelie  |Torgersen |           39.2|          19.6|               195|        4675|male   | 2007|
|Adelie  |Torgersen |           34.1|          18.1|               193|        3475|NA     | 2007|
|Adelie  |Torgersen |           42.0|          20.2|               190|        4250|NA     | 2007|

#### Visualize body mass of species

{% highlight r %}
# Remove NAs
penguins_df = na.omit(penguins)

# Histogram, position='identity' makes sure its not stacked
#ggplot(penguins_df, aes(x=body_mass_g, fill=species, color=species)) + geom_histogram(alpha=0.4, position='identity')

# Density plot:
ggplot(penguins_df, aes(x=body_mass_g, fill=species)) + geom_density(alpha=0.8) + theme_minimal()
{% endhighlight %}

![plot of chunk unnamed-chunk-3](/assets/images/regression_hypothesis_testing/unnamed-chunk-3-1.png)

#### Summary Statistics

{% highlight r %}
# Mean body mass for each species
aggregate(body_mass_g ~ species, penguins_df, mean)
{% endhighlight %}



{% highlight text %}
##     species body_mass_g
## 1    Adelie        3706
## 2 Chinstrap        3733
## 3    Gentoo        5092
{% endhighlight %}



{% highlight r %}
# Number of data points for each species
aggregate(body_mass_g ~ species, penguins_df, length)
{% endhighlight %}



{% highlight text %}
##     species body_mass_g
## 1    Adelie         146
## 2 Chinstrap          68
## 3    Gentoo         119
{% endhighlight %}


#### Adelie vs Chinstrap

Let's test the difference between body mass of Adelie and Chinstrap using regression and compare the result to a t-test. First let's look at the absolute difference:


{% highlight r %}
# Here we keep rows that contain Adelie or Chinstrap and only keep species and body_mass_g column
test_df = penguins_df[penguins_df$species %in% c('Adelie','Chinstrap'), c("species","body_mass_g")]

# Create factors again to remove unused factor level:
test_df$species = factor(test_df$species)

# Difference between means:
diff(by(test_df$body_mass_g, test_df$species, mean))
{% endhighlight %}



{% highlight text %}
## [1] 26.92
{% endhighlight %}

## Regression with categorical variables

Our x variable (species) is a category/factor. In python, you might have to use one hot encoding on your categorical variables before you pass your data into a linear model. The nice thing about R is that it does the one hot encoding/dummy coding on factors automagically for us:


{% highlight r %}
two_lvl_model = lm(body_mass_g ~ species, data = test_df)
summary(two_lvl_model)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## lm(formula = body_mass_g ~ species, data = test_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1033.1  -306.2   -20.6   268.4  1068.8 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        3706.2       36.1  102.59   <2e-16 ***
## speciesChinstrap     26.9       64.1    0.42     0.67    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 437 on 212 degrees of freedom
## Multiple R-squared:  0.000832,	Adjusted R-squared:  -0.00388 
## F-statistic: 0.176 on 1 and 212 DF,  p-value: 0.675
{% endhighlight %}

#### Dummy Coding / One-Hot Encoding

Let's manually do the dummy coding and compare results. If you think about a linear model $y=mx+b$ where $x$ is species and $y$ is body mass, we need $x$ to be $0$ for one category and $1$ for the other. In this case let's make $Chinstrap$ $1$ and $Adelie$ $0$.


{% highlight r %}
# Manually create dummy variables:
test_df$x = ifelse(test_df$species == 'Chinstrap',1,0)
{% endhighlight %}

Now we do regression with our new variable $x$.

{% highlight r %}
two_lvl_dummy = lm(body_mass_g ~ x, data = test_df)
summary(two_lvl_dummy)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## lm(formula = body_mass_g ~ x, data = test_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1033.1  -306.2   -20.6   268.4  1068.8 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   3706.2       36.1  102.59   <2e-16 ***
## x               26.9       64.1    0.42     0.67    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 437 on 212 degrees of freedom
## Multiple R-squared:  0.000832,	Adjusted R-squared:  -0.00388 
## F-statistic: 0.176 on 1 and 212 DF,  p-value: 0.675
{% endhighlight %}

We see that we got the exact same result as before.

#### Intrepreting Regression Coefficients


{% highlight r %}
# Intercept:
two_lvl_model$coefficients[1]
{% endhighlight %}



{% highlight text %}
## (Intercept) 
##        3706
{% endhighlight %}



{% highlight r %}
# Slope
two_lvl_model$coefficients[2]
{% endhighlight %}



{% highlight text %}
## speciesChinstrap 
##            26.92
{% endhighlight %}

Do these numbers look familar? The slope is $26.92$ which is actually the group difference. The 
intercept is $3700.62$ which is mean of Adelie group. Why is this the case?

Let's go back to the equation for this linear model, keeping in mind the dummy coding of our categorical variable. Adelie is represented by $x = 0$ and Chinstrap is $x = 1$.

If we sub in $x=0$, we will get $y=3700.662$ which is the mean of Adelie body mass:

$$ y = 32.42598(0) + 3700.662 = 3700.662 $$

When $x = 1$, $y=3733.088$ which is the mean of Chinstrap body mass:

$$ y = 32.42598(1) + 3700.66 = 3733.088 $$

I guess I didn't answer the real questions: Why is the slope the mean difference? Why is it that the line passes through the mean of both groups? 

Let's plot the situation:


{% highlight r %}
plot.default(x=test_df$species, y=test_df$body_mass_g)
lines(test_df$species, predict(two_lvl_model),col='blue')
{% endhighlight %}

![plot of chunk unnamed-chunk-10](/assets/images/regression_hypothesis_testing/unnamed-chunk-10-1.png)

The way the slope and intercept are calculated is called Oridnary Least Squares. It minimizes the sum of squared deviations. For every x variable our model predicts a value $\hat{y_i}$. Our $x$ variable is categorical with 2 levels: $Adelie(0)$ or $Chinstrap(1)$. 

$$\sum_{i=1}^{n} (y_i - \hat{y_i})^{2}$$

The least squares formula works by minimizing the sum of squared deviations. In this case the mean of both groups is what minimizes the sum of squared errors.

Let's take a simple example with 3 data points in one dimension. Let's say each data point is $y$. What value of $\hat{y}$ would minimize the sum of squared errors (the formula above)?.

![Residuals in one dimension](/assets/images/regression_hypothesis_testing/sse_1d.png)

The mean is $0.6$. Imagine a line from each point to the mean. The length of the line would be the difference between each point and the mean. The sum of the squares of those lines would be the squared errors. Try with any other point (for example the median 0.8). The mean is the one that minimizes sum of squared errors.

The line of best fit passes through both group means. The group means minimize the squared errors.

#### Interpreting t-value & p-value of the slope

What does the t-value represent here? Why does regression have a t-value at all?
Where is null t distribution and the area under the curve?

It is essentially a one sample t-test. The t-value and p-value are from a one sample t-test on the regression coefficient. And as we saw above, in this case the slope *_is_* the mean difference. So we are in a sense testing if the difference in means is different from $0$. If we compare the p-value of the slope here to a t-test or permutation test, we should see similar results. In fact, it should be identical to the t-test that uses pooled variance.

The statistical test which is conducted for the statistical significance of the coefficient is a one sample t-test. This is confusing since we do not have a "sample" of multiple coefficients for X4, but we have an estimate of the distributional properties of such a sample using the central limit theorem. The mean and standard error describe the location and shape of such a limiting distribution. If you take the column "Est" and divide by "SE" that gives you the t-value. If you use the t-value and look it up on a t-distribution with the given degrees of freedom, this gives you the p-values.

We can also compare this result to what we would get from a t-test:

## Regression vs t-test


{% highlight r %}
# If you set var.equal = T it will give the same t-value & p-value as the regression
t.test(test_df$body_mass_g ~ test_df$species, var.equal = T)
{% endhighlight %}



{% highlight text %}
## 
## 	Two Sample t-test
## 
## data:  test_df$body_mass_g by test_df$species
## t = -0.42, df = 212, p-value = 0.7
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -153.25   99.41
## sample estimates:
##    mean in group Adelie mean in group Chinstrap 
##                    3706                    3733
{% endhighlight %}

#### Why is the p-value for t-test and regression exactly the same?

T-test forumla:

$$ t = \frac{\bar{x_1} - \bar{x_2}}{ \sqrt{s_p^2 (\frac{1}{n_1} + \frac{1}{n_2}) }} $$

The denominator is the pooled standard error and $s_p^2$ is the pooled variance which is:

$$ s_p^2 = \frac{\sum_{i=1}^{n_1}(x_i - \bar{x_1}) + \sum_{j=1}^{n_2}(x_j - \bar{x_2})}{n_1 + n_2 - 2} $$

Which can also be written as (see [here](https://stackoverflow.com/a/21385702)):

$$ s_p^2 = \frac{(n_1 - 1)s_1^2 + n_2 - 1 (s_2^2)}{n_1 + n_2 - 2} $$

Let's calculate the t-value manually:

{% highlight r %}
# Variacne estimator of coefficients from linear model that meets assumptions:
# In this case Xs are just 0 or 1, Y is the body mass for respective species

var1 = var(test_df$body_mass_g[test_df$species == "Chinstrap"])
var2 = var(test_df$body_mass_g[test_df$species == "Adelie"])

n1 = length(test_df$body_mass_g[test_df$species == "Chinstrap"])
n2 = length(test_df$body_mass_g[test_df$species == "Adelie"])

# Pooled variance: 
var_pool = ( (n1-1)*var1 + (n2-1) * var2) / (n1+n2-2)

# Pooled standard error:
se_pool = sqrt( var_pool * (1/n1 + 1/n2) )
{% endhighlight %}

#### Regression t-value

Variance of the slope (sigma is variance of the error term):
$$ \frac{\sigma^{2}}{\sum(X_i - \bar{X})^{2}} $$

Here is a [video](https://www.youtube.com/watch?v=rODUBTRUV0U&ab_channel=jbstatistics) showing the derivation for variance:

And a [stack post](https://stats.stackexchange.com/questions/85943/how-to-derive-the-standard-error-of-linear-regression-coefficient) explaining standard error: 


You can kind of think of SE of slope in a similar manner to to SE of any data. The SE of data is when you repeatedly sample the data and get the mean. This distribution is called the sampling distribution of the mean and the SD of this distribution is the SE. Central limit theorm tells us the SE is $\frac{\sigma}{\sqrt{n}}$.

Similarly for slope, imagine if you repeatedly sampled your data and calculated the slope of the best fit line. If the residuals are small, we might except slope to have a narrower range of values. If the residuals are large we are less sure of the true value of the slope, and thus might have a larger range of values.


{% highlight r %}
# SE of the slope is the same as SE pooled
sqrt(sum((two_lvl_model$residuals - mean(two_lvl_model$residuals))^2) / (length( two_lvl_model$residuals) - 2)) / sqrt(sum( (test_df$x - mean(test_df$x) )^2 ))
{% endhighlight %}



{% highlight text %}
## [1] 64.09
{% endhighlight %}

The regression t-value is calculated using estimates of the mean difference and standard error obtained mathematically.

We see how the standard error of both are the same. We also know the numerator is the same. In the t-test it is the mean difference, and in regression it is the slope, which is equal to the mean difference in this case.

## Regression with a factor with 3 levels


{% highlight r %}
three_lvl_model = lm(body_mass_g ~ species, data = penguins_df)
summary(three_lvl_model)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## lm(formula = body_mass_g ~ species, data = penguins_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1142.4  -342.4   -33.1   307.6  1207.6 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        3706.2       38.1    97.2   <2e-16 ***
## speciesChinstrap     26.9       67.7     0.4     0.69    
## speciesGentoo      1386.3       56.9    24.4   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 461 on 330 degrees of freedom
## Multiple R-squared:  0.674,	Adjusted R-squared:  0.673 
## F-statistic:  342 on 2 and 330 DF,  p-value: <2e-16
{% endhighlight %}

#### The equivalant is one way anova and tukey HSD t-tests


{% highlight r %}
three_lvl_aov = aov(body_mass_g ~ species, data = penguins_df)
summary(three_lvl_aov)
{% endhighlight %}



{% highlight text %}
##              Df   Sum Sq  Mean Sq F value Pr(>F)    
## species       2 1.45e+08 72595110     342 <2e-16 ***
## Residuals   330 7.01e+07   212332                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
{% endhighlight %}
TukeyHSD gives us an additional piece of information: the Gentoo-Chinstrap relationship which is not present in our regression above.


{% highlight r %}
TukeyHSD(three_lvl_aov)
{% endhighlight %}



{% highlight text %}
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = body_mass_g ~ species, data = penguins_df)
## 
## $species
##                     diff    lwr    upr  p adj
## Chinstrap-Adelie   26.92 -132.4  186.2 0.9164
## Gentoo-Adelie    1386.27 1252.3 1520.3 0.0000
## Gentoo-Chinstrap 1359.35 1194.4 1524.3 0.0000
{% endhighlight %}

Now let's visualize this:

#### Plot data

{% highlight r %}
plot.default(x=penguins_df$species,y=penguins_df$body_mass_g)
lines(penguins_df$species, predict(three_lvl_model),col='blue')
{% endhighlight %}

![plot of chunk unnamed-chunk-17](/assets/images/regression_hypothesis_testing/unnamed-chunk-17-1.png)

Well this is pretty wild, are we still looking at linear regression? The lines are disjoint and broken. What could be going on here? However, note that it does manage to pass through the means of all three groups (based on regression coefficients) which means it does minimize the least squares.

Let's go back to the regression equation. In the regression output above we had an intercept and 2 slopes, meaning there was 2 $x$ variables. This is because inorder to represent 3 levels of a categorical variable, we need 2 dummy variables. So the one hot encoding R is doing will be something like this:

$$ y = m_1x_1 + m_2x_2 + b $$

Where different combinations of $x_1$ & $x_2$ will be different categories. Adelie would be $x_1 = 0, x_2 = 0$, Chinstrap would be $x_1 = 1, x_2 = 0$ and Gentoo would be $x_1 = 0, x_2 = 1 $. So, this is actually regression with 3 dimensions, but our plot was only 2 dimensions. [This post expains the concept beautifully.]( https://stats.stackexchange.com/questions/92065/why-is-polynomial-regression-considered-a-special-case-of-multiple-linear-regres)

We can manually create this coding and verify we get the exact same result:


{% highlight r %}
# Manually create dummy variables:
penguins_df$x1 = ifelse(penguins_df$species == 'Chinstrap',1,0)
penguins_df$x2 = ifelse(penguins_df$species == 'Gentoo',1,0)

penguins_df
{% endhighlight %}



{% highlight text %}
## # A tibble: 333 x 10
##    species island bill_length_mm bill_depth_mm flipper_length_~ body_mass_g
##    <fct>   <fct>           <dbl>         <dbl>            <int>       <int>
##  1 Adelie  Torge~           39.1          18.7              181        3750
##  2 Adelie  Torge~           39.5          17.4              186        3800
##  3 Adelie  Torge~           40.3          18                195        3250
##  4 Adelie  Torge~           36.7          19.3              193        3450
##  5 Adelie  Torge~           39.3          20.6              190        3650
##  6 Adelie  Torge~           38.9          17.8              181        3625
##  7 Adelie  Torge~           39.2          19.6              195        4675
##  8 Adelie  Torge~           41.1          17.6              182        3200
##  9 Adelie  Torge~           38.6          21.2              191        3800
## 10 Adelie  Torge~           34.6          21.1              198        4400
## # ... with 323 more rows, and 4 more variables: sex <fct>, year <int>,
## #   x1 <dbl>, x2 <dbl>
{% endhighlight %}


{% highlight r %}
# Double check that this is the same as the model we did before with UDF_text_07
three_lvl_dummy = lm(body_mass_g ~ x1+x2, data = penguins_df)
summary(three_lvl_dummy)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## lm(formula = body_mass_g ~ x1 + x2, data = penguins_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1142.4  -342.4   -33.1   307.6  1207.6 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   3706.2       38.1    97.2   <2e-16 ***
## x1              26.9       67.7     0.4     0.69    
## x2            1386.3       56.9    24.4   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 461 on 330 degrees of freedom
## Multiple R-squared:  0.674,	Adjusted R-squared:  0.673 
## F-statistic:  342 on 2 and 330 DF,  p-value: <2e-16
{% endhighlight %}


We get the exact same result as above.

We can visualize the 3D regression to get an idea of where the broken lines come from:

{% highlight r %}
s3d = scatterplot3d(z=penguins_df$body_mass_g, x=penguins_df$x1, y=penguins_df$x2)
s3d$plane3d(three_lvl_dummy,draw_polygon = TRUE)
{% endhighlight %}

![plot of chunk unnamed-chunk-20](/assets/images/regression_hypothesis_testing/unnamed-chunk-20-1.png)

If you try to mentally squish this into a 2D graph like we saw before, you can kind of see how we got the broken graph earlier.


## References:

https://stats.stackexchange.com/questions/92065/why-is-polynomial-regression-considered-a-special-case-of-multiple-linear-regres


How come we can use linear regression for hypothesis testing:
https://stats.stackexchange.com/questions/128723/understanding-of-p-value-in-multiple-linear-regression

https://stats.stackexchange.com/questions/117406/proof-that-the-coefficients-in-an-ols-model-follow-a-t-distribution-with-n-k-d

https://stats.stackexchange.com/questions/285986/distribution-of-linear-regression-coefficients/285992

https://stats.stackexchange.com/questions/342632/how-to-understand-se-of-regression-slope-equation/342672

https://stats.stackexchange.com/questions/344006/understanding-t-test-for-linear-regression

http://home.cc.umanitoba.ca/~godwinrt/4042/material/part3.pdf
