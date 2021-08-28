---
layout: post
title: "Regression & Hypothesis Testing: Continous Y, Categorical X"
date: "2021-08-27"
output:
  md_document:
    variant: markdown_github
    preserve_yaml: true

tags: [regression, categorical, hypothesis, testing, multiple, categories]
values:
  show_date: true
---

``` r
knitr::opts_knit$set(base.dir = "C:/Users/Laagi.yoganathan/Documents/GitLab/lyoganathan.github.io", base.url = "/")
knitr::opts_chunk$set(fig.path = "assets/images/")
```

Palmer penguins dataset
-----------------------

I’ve been watching a lot of Atypical recently and also found out there’s
a dataset with some of the penguins Sam talks about.

This will show how to do regression with categorical variables, and look
at similarities with hypothesis testing using t-tests. I’ll go into
what’s happening in the background and why we can get the same result
from a t-test and regression.

``` r
library(palmerpenguins)
library(ggplot2)
library(scatterplot3d)

penguins
```

    ## # A tibble: 344 x 8
    ##    species island bill_length_mm bill_depth_mm flipper_length_~ body_mass_g
    ##    <fct>   <fct>           <dbl>         <dbl>            <int>       <int>
    ##  1 Adelie  Torge~           39.1          18.7              181        3750
    ##  2 Adelie  Torge~           39.5          17.4              186        3800
    ##  3 Adelie  Torge~           40.3          18                195        3250
    ##  4 Adelie  Torge~           NA            NA                 NA          NA
    ##  5 Adelie  Torge~           36.7          19.3              193        3450
    ##  6 Adelie  Torge~           39.3          20.6              190        3650
    ##  7 Adelie  Torge~           38.9          17.8              181        3625
    ##  8 Adelie  Torge~           39.2          19.6              195        4675
    ##  9 Adelie  Torge~           34.1          18.1              193        3475
    ## 10 Adelie  Torge~           42            20.2              190        4250
    ## # ... with 334 more rows, and 2 more variables: sex <fct>, year <int>

``` r
# Remove NAs
penguins_df = na.omit(penguins)
```

Histogram / Density plot:

``` r
#position='identity' makes sure its not stacked
#ggplot(penguins_df, aes(x=body_mass_g, fill=species, color=species)) + geom_histogram(alpha=0.4, position='identity')

ggplot(penguins_df, aes(x=body_mass_g, fill=species)) + geom_density(alpha=0.8)
```

![](/assets/images/unnamed-chunk-4-1.png)

``` r
# Mean body mass for each species
aggregate(body_mass_g ~ species, penguins_df, mean)
```

    ##     species body_mass_g
    ## 1    Adelie    3706.164
    ## 2 Chinstrap    3733.088
    ## 3    Gentoo    5092.437

``` r
# Number of data points for each species
aggregate(body_mass_g ~ species, penguins_df, length)
```

    ##     species body_mass_g
    ## 1    Adelie         146
    ## 2 Chinstrap          68
    ## 3    Gentoo         119

Let’s test the difference between body mass of Adelie and Chinstrap
using regression and compare the result to a t-test.

``` r
# Here we keep rows that contain Adelie or Chinstrap and only keep species and body_mass_g column
test_df = penguins_df[penguins_df$species %in% c('Adelie','Chinstrap'), c("species","body_mass_g")]

# Create factors again to remove unused factor level:
test_df$species = factor(test_df$species)

# Difference between means:
diff(by(test_df$body_mass_g, test_df$species, mean))
```

    ## [1] 26.92385

Regression with categorical variables
-------------------------------------

Our x variable (species) is a category/factor. In python, you might have
to use one hot encoding on your categorical variables before you pass
your data into a linear model. The nice thing about R is that it does
the one hot encoding/dummy coding on factors automagically for us.

``` r
two_lvl_model = lm(body_mass_g ~ species, data = test_df)
summary(two_lvl_model)
```

    ## 
    ## Call:
    ## lm(formula = body_mass_g ~ species, data = test_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1033.09  -306.16   -20.59   268.35  1068.84 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3706.16      36.13  102.59   <2e-16 ***
    ## speciesChinstrap    26.92      64.09    0.42    0.675    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 436.5 on 212 degrees of freedom
    ## Multiple R-squared:  0.0008318,  Adjusted R-squared:  -0.003881 
    ## F-statistic: 0.1765 on 1 and 212 DF,  p-value: 0.6748

### Dummy Coding / One-Hot Encoding

Let’s manually do the dummy coding and compare results. If you think
about a linear model *y* = *m**x* + *b* where *x* is species and *y* is
body mass, we need *x* to be 0 for one category and 1 for the other. In
this case let’s make Chinstrap 1 and Adelie 0.

``` r
# Manually create dummy variables:
test_df$x = ifelse(test_df$species == 'Chinstrap',1,0)
```

Now we do regression with our new variable *x*.

``` r
two_lvl_dummy = lm(body_mass_g ~ x, data = test_df)
summary(two_lvl_dummy)
```

    ## 
    ## Call:
    ## lm(formula = body_mass_g ~ x, data = test_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1033.09  -306.16   -20.59   268.35  1068.84 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3706.16      36.13  102.59   <2e-16 ***
    ## x              26.92      64.09    0.42    0.675    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 436.5 on 212 degrees of freedom
    ## Multiple R-squared:  0.0008318,  Adjusted R-squared:  -0.003881 
    ## F-statistic: 0.1765 on 1 and 212 DF,  p-value: 0.6748

We see that we got the exact same result as before.

### Intrepreting Regression Coefficients

``` r
# Intercept:
two_lvl_model$coefficients[1]
```

    ## (Intercept) 
    ##    3706.164

``` r
# Slope
two_lvl_model$coefficients[2]
```

    ## speciesChinstrap 
    ##         26.92385

Do these numbers look familar? The slope is 26.92 which is actually the
group difference. The intercept is 3700.62 which is mean of Adelie
group. Why is this the case?

Let’s go back to the equation for this linear model, keeping in mind the
dummy coding of our categorical variable. Adelie is represented by
*x* = 0 and Chinstrap is *x* = 1.

If we sub in *x* = 0, we will get *y* = 3700.662 which is the mean of
Adelie body mass:

*y* = 32.42598(0) + 3700.662 = 3700.662

When *x* = 1, *y* = 3733.088 which is the mean of Chinstrap body mass:

*y* = 32.42598(1) + 3700.66 = 3733.088

I guess I didn’t answer the real questions: Why is the slope the mean
difference? Why is it that the line passes through the mean of both
groups?

Let’s plot the situation:

``` r
plot.default(x=test_df$species, y=test_df$body_mass_g)
lines(test_df$species, predict(two_lvl_model),col='blue')
```

![](/assets/images/unnamed-chunk-11-1.png)

The way the slope and intercept are calculated is called Oridnary Least
Squares. It minimizes the sum of squared deviations. For every x
variable our model predicts a value $\\hat{y\_i}$. Our x variable is
categorical with 2 levels: Adelie(0) or Chinstrap(1).

$\\sum\_{i=1}^{n} (y\_i - \\hat{y\_i})^{2}$

The least squares formula works by minimizing the sum of squared
deviations. In this case the mean of both groups is what minimizes the
sum of squared errors.

Let’s take a simple example with 3 data points in one dimension. Let’s
say each data point is *y*. What value of *ŷ* would minimize the sum of
squared errors (the formula above)?.

![Residuals in one dimension](/assets/images/sse_1d.svg)

The mean is 0.6. Imagine a line from each point to the mean. The length
of the line would be the difference between each point and the mean. The
sum of the squares of those lines would be the squared errors. Try with
any other point (for example the median 0.8). The mean is the one that
minimizes sum of squared errors.

The line of best fit passes through both group means. The group means
minimize the squared errors.

### Interpreting t-value & p-value of the slope:

What does the t-value represent here? Why does regression have a t-value
at all? Where is null t distribution and the area under the curve?

It is essentially a one sample t-test. The t-value and p-value are from
a one sample t-test on the regression coefficient. And as we saw above,
in this case the slope *is* the mean difference. So we are in a sense
testing if the difference in means is different from 0. If we compare
the p-value of the slope here to a t-test or permutation test, we should
see similar results. In fact, it should be identical to the t-test that
uses pooled variance.

The statistical test which is conducted for the statistical significance
of the coefficient is a one sample t-test. This is confusing since we do
not have a “sample” of multiple coefficients for X4, but we have an
estimate of the distributional properties of such a sample using the
central limit theorem. The mean and standard error describe the location
and shape of such a limiting distribution. If you take the column “Est”
and divide by “SE” that gives you the t-value. If you use the t-value
and look it up on a t-distribution with the given degrees of freedom,
this gives you the p-values.

We can also compare this result to what we would get from a t-test:

Regression vs t-test:
---------------------

``` r
# If you set var.equal = T it will give the same t-value & p-value as the regression
t.test(test_df$body_mass_g ~ test_df$species, var.equal = T)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  test_df$body_mass_g by test_df$species
    ## t = -0.42011, df = 212, p-value = 0.6748
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -153.2538   99.4061
    ## sample estimates:
    ##    mean in group Adelie mean in group Chinstrap 
    ##                3706.164                3733.088

### Why is the p-value for t-test and regression exactly the same?

T-test forumla:

$$t = \\frac{\\bar{x\_1} - \\bar{x\_2}}{ \\sqrt{s\_p^2 (\\frac{1}{n\_1} + \\frac{1}{n\_2}) }} $$

The denominator is the pooled standard error and
*s*<sub>*p*</sub><sup>2</sup> is the pooled variance which is:

$$s\_p^2 = \\frac{\\sum\_{i=1}^{n\_1}(x\_i - \\bar{x\_1}) + \\sum\_{j=1}^{n\_2}(x\_j - \\bar{x\_2})}{n\_1 + n\_2 - 2}$$

Which can also be written as (see
[here](https://stackoverflow.com/a/21385702)):

$$s\_p^2 = \\frac{(n\_1 - 1)s\_1^2 + n\_2 - 1 (s\_2^2)}{n\_1 + n\_2 - 2}$$

Let’s calculate the t-value manually:

``` r
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
```

### Regression t-value:

Variance of the slope (sigma is variance of the error term):
$$ \\frac{\\sigma^{2}}{\\sum(X\_i - \\bar{X})^{2}} $$

Here is a video showing the derivation for variance:
<a href="https://www.youtube.com/watch?v=rODUBTRUV0U&amp;ab_channel=jbstatistics" class="uri">https://www.youtube.com/watch?v=rODUBTRUV0U&amp;ab_channel=jbstatistics</a>

And a stack post explaining standard error:
<a href="https://stats.stackexchange.com/questions/85943/how-to-derive-the-standard-error-of-linear-regression-coefficient" class="uri">https://stats.stackexchange.com/questions/85943/how-to-derive-the-standard-error-of-linear-regression-coefficient</a>

You can kind of think of SE of slope in a similar manner to to SE of any
data. The SE of data is when you repeatedly sample the data and get the
mean. This distribution is called the sampling distribution of the mean
and the SD of this distribution is the SE. Central limit theorm tells us
the SE is $\\frac{\\sigma}{\\sqrt{n}}$.

Similarly for slope, imagine if you repeatedly sampled your data and
calculated the slope of the best fit line. If the residuals are small,
we might except slope to have a narrower range of values. If the
residuals are large we are less sure of the true value of the slope, and
thus might have a larger range of values.

``` r
# SE of the slope is the same as SE pooled
sqrt(sum((two_lvl_model$residuals - mean(two_lvl_model$residuals))^2) / (length( two_lvl_model$residuals) - 2)) / sqrt(sum( (test_df$x - mean(test_df$x) )^2 ))
```

    ## [1] 64.08729

The regression t-value is calculated using estimates of the mean
difference and standard error obtained mathematically.

We see how the standard error of both are the same. We also know the
numerator is the same. In the t-test it is the mean difference, and in
regression it is the slope, which is equal to the mean difference in
this case.

Regression with a factor with 3 levels:
---------------------------------------

``` r
three_lvl_model = lm(body_mass_g ~ species, data = penguins_df)
summary(three_lvl_model)
```

    ## 
    ## Call:
    ## lm(formula = body_mass_g ~ species, data = penguins_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1142.44  -342.44   -33.09   307.56  1207.56 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3706.16      38.14  97.184   <2e-16 ***
    ## speciesChinstrap    26.92      67.65   0.398    0.691    
    ## speciesGentoo     1386.27      56.91  24.359   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 460.8 on 330 degrees of freedom
    ## Multiple R-squared:  0.6745, Adjusted R-squared:  0.6725 
    ## F-statistic: 341.9 on 2 and 330 DF,  p-value: < 2.2e-16

The equivalant is one way anova and tukey HSD t-tests:
------------------------------------------------------

``` r
three_lvl_aov = aov(body_mass_g ~ species, data = penguins_df)
summary(three_lvl_aov)
```

    ##              Df    Sum Sq  Mean Sq F value Pr(>F)    
    ## species       2 145190219 72595110   341.9 <2e-16 ***
    ## Residuals   330  70069447   212332                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

TukeyHSD gives us an additional piece of information: the
Gentoo-Chinstrap relationship which is not present in our regression
above.

``` r
TukeyHSD(three_lvl_aov)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = body_mass_g ~ species, data = penguins_df)
    ## 
    ## $species
    ##                        diff       lwr       upr    p adj
    ## Chinstrap-Adelie   26.92385 -132.3528  186.2005 0.916431
    ## Gentoo-Adelie    1386.27259 1252.2897 1520.2554 0.000000
    ## Gentoo-Chinstrap 1359.34874 1194.4304 1524.2671 0.000000

Now let’s visualize this:

Plot data
=========

``` r
plot.default(x=penguins_df$species,y=penguins_df$body_mass_g)
lines(penguins_df$species, predict(three_lvl_model),col='blue')
```

![](/assets/images/unnamed-chunk-18-1.png)

Well this is pretty wild, are we still looking at linear regression? The
lines are disjoint and broken. What could be going on here? However,
note that it does manage to pass through the means of all three groups
(based on regression coefficients) which means it does minimize the
least squares.

Let’s go back to the regression equation. In the regression output above
we had an intercept and 2 slopes, meaning there was 2 *x* variables.
This is because inorder to represent 3 levels of a categorical variable,
we need 2 dummy variables. So the one hot encoding R is doing will be
something like this:

*y* = *m*<sub>1</sub>*x*<sub>1</sub> + *m*<sub>2</sub>*x*<sub>2</sub> + *b*

Where different combinations of *x*<sub>1</sub> & *x*<sub>2</sub> will
be different categories. Adelie would be
*x*<sub>1</sub> = 0, *x*<sub>2</sub> = 0, Chinstrap would be
*x*<sub>1</sub> = 1, *x*<sub>2</sub> = 0 and Gentoo would be
*x*<sub>1</sub> = 0, *x*<sub>2</sub> = 1. So, this is actually
regression with 3 dimensions, but our plot was only 2 dimensions. This
post expains the concept beautifully:
<a href="https://stats.stackexchange.com/questions/92065/why-is-polynomial-regression-considered-a-special-case-of-multiple-linear-regres" class="uri">https://stats.stackexchange.com/questions/92065/why-is-polynomial-regression-considered-a-special-case-of-multiple-linear-regres</a>

We can manually create this coding and verify we get the exact same
result:

``` r
# Manually create dummy variables:
penguins_df$x1 = ifelse(penguins_df$species == 'Chinstrap',1,0)
penguins_df$x2 = ifelse(penguins_df$species == 'Gentoo',1,0)

penguins_df
```

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

``` r
# Double check that this is the same as the model we did before with UDF_text_07
three_lvl_dummy = lm(body_mass_g ~ x1+x2, data = penguins_df)
summary(three_lvl_dummy)
```

    ## 
    ## Call:
    ## lm(formula = body_mass_g ~ x1 + x2, data = penguins_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1142.44  -342.44   -33.09   307.56  1207.56 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3706.16      38.14  97.184   <2e-16 ***
    ## x1             26.92      67.65   0.398    0.691    
    ## x2           1386.27      56.91  24.359   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 460.8 on 330 degrees of freedom
    ## Multiple R-squared:  0.6745, Adjusted R-squared:  0.6725 
    ## F-statistic: 341.9 on 2 and 330 DF,  p-value: < 2.2e-16

We get the exact same result as above.

We can visualize the 3D regression to get an idea of where the broken
lines come from:

``` r
s3d = scatterplot3d(z=penguins_df$body_mass_g, x=penguins_df$x1, y=penguins_df$x2)
s3d$plane3d(three_lvl_dummy,draw_polygon = TRUE)
```

![](/assets/images/unnamed-chunk-21-1.png)

If you try to mentally squish this into a 2D graph like we saw before,
you can kind of see how we got the broken graph earlier.

References
----------

I get most of my knowledge from stack overflow:

<a href="https://stats.stackexchange.com/questions/92065/why-is-polynomial-regression-considered-a-special-case-of-multiple-linear-regres" class="uri">https://stats.stackexchange.com/questions/92065/why-is-polynomial-regression-considered-a-special-case-of-multiple-linear-regres</a>

How come we can use linear regression for hypothesis testing:
<a href="https://stats.stackexchange.com/questions/128723/understanding-of-p-value-in-multiple-linear-regression" class="uri">https://stats.stackexchange.com/questions/128723/understanding-of-p-value-in-multiple-linear-regression</a>

<a href="https://stats.stackexchange.com/questions/117406/proof-that-the-coefficients-in-an-ols-model-follow-a-t-distribution-with-n-k-d" class="uri">https://stats.stackexchange.com/questions/117406/proof-that-the-coefficients-in-an-ols-model-follow-a-t-distribution-with-n-k-d</a>

<a href="https://stats.stackexchange.com/questions/285986/distribution-of-linear-regression-coefficients/285992" class="uri">https://stats.stackexchange.com/questions/285986/distribution-of-linear-regression-coefficients/285992</a>

<a href="https://stats.stackexchange.com/questions/342632/how-to-understand-se-of-regression-slope-equation/342672" class="uri">https://stats.stackexchange.com/questions/342632/how-to-understand-se-of-regression-slope-equation/342672</a>

<a href="https://stats.stackexchange.com/questions/344006/understanding-t-test-for-linear-regression" class="uri">https://stats.stackexchange.com/questions/344006/understanding-t-test-for-linear-regression</a>

<a href="http://home.cc.umanitoba.ca/~godwinrt/4042/material/part3.pdf" class="uri">http://home.cc.umanitoba.ca/~godwinrt/4042/material/part3.pdf</a>
