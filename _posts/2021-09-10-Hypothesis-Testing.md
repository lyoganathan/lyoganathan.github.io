---
layout: single
title: "Hypothesis Testing With Regression"
excerpt: "Comparing regression and t-test when you have a continous Y and a categorical X"
date: "2021-09-10"
tags: [regression, categorical, hypothesis, testing, multiple, categories]
values:
  show_date: true
toc: yes
---



## Intro

There's many different ways to do hypothesis testing: permutation test, t-test, chi-square test, [bayesian estimation](https://jkkweb.sitehost.iu.edu/articles/Kruschke2013JEPG.pdf), [GEE](https://quoradata.quora.com/A-Robust-Statistical-Test-for-Ratio-Metrics)...

This posts goes through how linear regression can be used for hypothesis testing and why it can give the same result as a t-test. I took inspiration from [this post](http://m-clark.github.io/docs/mixedModels/anovamixed.html) and from some great Stack Overflow answers which I've linked at the bottom.


## Palmer penguins dataset

I recently found out about a dataset on penguins in Antarctica called _palmerpenguins_. If you've seen Netflix's Atypical, some of these penguin names might be familiar.


{% highlight r %}
library(palmerpenguins)
library(ggplot2)
library(scatterplot3d)
library(knitr)

kable(penguins[sample(nrow(penguins),10),])
{% endhighlight %}



|species   |island | bill_length_mm| bill_depth_mm| flipper_length_mm| body_mass_g|sex    | year|
|:---------|:------|--------------:|-------------:|-----------------:|-----------:|:------|----:|
|Adelie    |Biscoe |           39.7|          18.9|               184|        3550|male   | 2009|
|Chinstrap |Dream  |           45.2|          16.6|               191|        3250|female | 2009|
|Adelie    |Dream  |           40.3|          18.5|               196|        4350|male   | 2008|
|Gentoo    |Biscoe |           48.4|          14.6|               213|        5850|male   | 2007|
|Chinstrap |Dream  |           52.0|          19.0|               197|        4150|male   | 2007|
|Adelie    |Biscoe |           37.9|          18.6|               172|        3150|female | 2007|
|Adelie    |Biscoe |           37.8|          20.0|               190|        4250|male   | 2009|
|Chinstrap |Dream  |           52.8|          20.0|               205|        4550|male   | 2008|
|Adelie    |Biscoe |           38.8|          17.2|               180|        3800|male   | 2007|
|Adelie    |Dream  |           35.7|          18.0|               202|        3550|female | 2008|

### Visualize body mass of species

{% highlight r %}
# Remove NAs
penguins_df = na.omit(penguins)

# Histogram, position='identity' makes sure its not stacked
#ggplot(penguins_df, aes(x=body_mass_g, fill=species, color=species)) + geom_histogram(alpha=0.4, position='identity')

# Density plot:
ggplot(penguins_df, aes(x=body_mass_g, fill=species)) + geom_density(alpha=0.8) + theme_minimal()
{% endhighlight %}

![plot of chunk density-plot-body-mass](/assets/images/regression_hypothesis_testing/density-plot-body-mass-1.png)

We can see that Adelie and Chinstrap have a similar body mass while Gentoo is heavier. If you are unsure about the differences or feel like your eyes are deciving you, performing a statistical test looking at confidence intervals can help quantify the difference and the uncertainty around that difference.

### Summary Statistics

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

### Adelie vs Chinstrap

Let's test the difference between body mass of Adelie and Chinstrap using regression and compare the result to a t-test. This will tell us if the observed difference is statistically significant or not. First let's look at the difference in means between groups:


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

Our $x$ variable, $Species$, is a category/factor. In python, you might have to use one hot encoding on your categorical variables before you pass your data into a linear model. The nice thing about R is that it does the one hot encoding/dummy coding on factors automagically for us:


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

### Dummy Coding / One-Hot Encoding

To better understand how regression on groups works, it's helpful to understand dummy coding. Let's manually do the dummy coding and compare results. If you think about a linear model $y=mx+b$ where $x$ is $Species$ and $y$ is $BodyMass$, we need $x$ to be $0$ for one category and $1$ for the other. To match the output from the previous linear model, let's make $Adelie$ $0$ and $Chinstrap$ $1$.


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

### Intrepreting Regression Coefficients

Based on the output above, the regression equation can be written as:

$$ y = 26.92x + 3706.164 $$

Do these numbers look familar? The slope $26.92$ is actually the difference between means of the groups. The intercept $3706.16$ is the mean of Adelie group. Why is this the case?

Let's sub in the dummy coding of our $Species$ variable. If we sub in $x=0$ for $Adelie$, we will get $y=3706.164$ which is the mean of Adelie body mass:

$$ y = 26.92(0) + 3706.164 = 3706.164 $$

When $x = 1$ for $Chinstrap$, $y=3733.088$ which is the mean of Chinstrap body mass:

$$ y = 26.92(1) + 3706.164 = 3733.088 $$

But _why_ is the slope the mean difference? How come the regression parameters were estimated this way? It may be helpful to visualize the regression.

### Plot regression with 2 species


{% highlight r %}
plot.default(x=test_df$x, y=test_df$body_mass_g,xlab = "Species (Dummy Coded)", ylab = "Body Mass (g)")
lines(test_df$x, predict(two_lvl_model),col='blue')
{% endhighlight %}

![plot of chunk boxplot-regression-line](/assets/images/regression_hypothesis_testing/boxplot-regression-line-1.png)

The way the slope and intercept are calculated is called Oridnary Least Squares (OLS). It finds values of the slope and intercept that minimizes the sum of squared deviations. For every $x_i$ variable our model predicts a value $\hat{y_i}$, and least squares tries to estimate the parameters (slope & intercept) that minimize the difference between the actual data point $y_i$ and the predicted value from the best fit line $\hat{y_i}$:

$$\sum_{i=1}^{n} (y_i - \hat{y_i})^{2}$$

Our $x$ variable only has 2 values: $0(Adelie)$ or $1(Chinstrap)$. The OLS formula finds the slope and intercept that minimizes the sum of squared deviations. In this case the mean of both groups is minimial distance to all other points.

To try and explain the intuition behind why this is the case, let's take a simple example with 3 data points in one dimension: $0$, $0.8$, $1$. In the above equation, each point would be a $y$. What value of $\hat{y}$ would minimize this equation?

![Residuals in one dimension](/assets/images/regression_hypothesis_testing/sse_1d.png)

The mean of $0.6$ is the point that minimizes sum of squared deviations:

$$ (0-0.6)^2+(0.8-0.6)^2+(1-0.6)^2 = 0.56 $$

You can try with any other value, but you will find that using the mean as $\hat{y}$ minimizes the above equation. So, the line of best fit passes through both group means because the group means minimize the squared errors.

In two dimensions, OLS can be used to find the equation of a line that minimizes distance to every other point. But when the $x$ variable only has two values of $0$ and $1$ we can find the equation of the line easily, as we know it will pass through the means of both groups. You only need two points to find the equation of a line. The slope of the line will be the mean difference and the intercept will be the mean of the group coded as $0$.

### Interpreting t-value & p-value of the slope

What does the t-value represent here? Why does regression have a t-value at all?

The t-value and p-value are from a one sample t-test on the regression coefficient. This can be confusing since we don't have a sample of slopes, we only have one value for slope. But we can estimate the distribution of such a sample using the central limit theorem.

In this case the slope **_is_** the mean difference. So we are in a sense testing if the difference in means is different from $0$. If we compare the p-value of the slope here to a t-test or permutation test, we should see similar results.

## Regression vs t-test

In fact, t-value and p-value of the slope coefficient should be identical to the t-test that uses pooled variance:


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

### Why is the p-value for t-test and regression exactly the same?

T-test forumla:

$$ t = \frac{\bar{x_1} - \bar{x_2}}{ \sqrt{s_p^2 (\frac{1}{n_1} + \frac{1}{n_2}) }} $$

The denominator is the pooled standard error and $s_p^2$ is the pooled variance which is:

$$ s_p^2 = \frac{\sum_{i=1}^{n_1}(x_i - \bar{x_1}) + \sum_{j=1}^{n_2}(x_j - \bar{x_2})}{n_1 + n_2 - 2} $$

Which can also be written as (see [here](https://stackoverflow.com/a/21385702)):

$$ s_p^2 = \frac{(n_1 - 1)s_1^2 + (n_2 - 1)s_2^2}{n_1 + n_2 - 2} $$

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

# Difference between group means:
mean_diff = diff(by(test_df$body_mass_g, test_df$species, mean))

# t-value: 
t_val = mean_diff/se_pool

# p-value from t-distribution (Negative our t-value to match output from t.test):
p_val = 2*pt(-t_val,213)
{% endhighlight %}


{% highlight text %}
## se_pool = 64.087288
## mean_diff = 26.923852
## t_val = 0.420112
## p_val = 0.674827
{% endhighlight %}

### Regression t-value

Standard error of the slope:

$$ \frac{\sqrt{s^2}}{\sqrt{\sum(X_i - \bar{X})^{2}}} $$

Where $\sqrt{s^2}$ is:

$$ \sqrt{\frac{\sum{(y_i-\hat{y_i})^2}}{n-2}} $$

Here is a [video](https://www.youtube.com/watch?v=rODUBTRUV0U&ab_channel=jbstatistics) showing the derivation for variance of the slope estimator. And a [stack post](https://stats.stackexchange.com/questions/85943/how-to-derive-the-standard-error-of-linear-regression-coefficient) explaining standard error.

You can kind of think of SE of slope in a similar manner to to SE of the mean. The SE of the mean is when you repeatedly sample the data and get the mean. If you plot all those samples means, the distribution is called the sampling distribution of the mean and the SD of this distribution is the SE. Central limit theorm tells us the SE is $\frac{\sigma}{\sqrt{n}}$.

Similarly for slope, imagine if you repeatedly sampled your data and calculated the slope of the best fit line. If the residuals are small, we might except slope to have a narrower range of values. If the residuals are large we are less sure of the true value of the slope, and thus might have a larger range of values.

Let's calcuate the values manually:


{% highlight r %}
# Value of slope:
slope = two_lvl_model$coefficients[2]

# SE of the slope:
slope_se = sqrt(sum((two_lvl_model$residuals - mean(two_lvl_model$residuals))^2) / 
       (length( two_lvl_model$residuals) - 2)) / 
  sqrt(sum( (test_df$x - mean(test_df$x) )^2 ))
{% endhighlight %}


{% highlight text %}
## slope = 26.923852
## slope_se = 64.087288
{% endhighlight %}


We see how the standard error of both regression slope and pooled standard error from the t-test are same. We also know the slope is equal to the mean difference. Hopefully this explains how we get same t-values and p-values for regression and t-tests.

## Regression with 3 categories

Now let's do a regression with all three species:


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

### Tukey HSD t-tests

TukeyHSD gives us an additional piece of information: the Gentoo-Chinstrap relationship which is not present in our regression above.


{% highlight r %}
three_lvl_aov = aov(body_mass_g ~ species, data = penguins_df)
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

Now let's visualize this.

### Plot regression with 3 species

{% highlight r %}
plot.default(x=penguins_df$species,y=penguins_df$body_mass_g)
lines(penguins_df$species, predict(three_lvl_model),col='blue')
{% endhighlight %}

![plot of chunk regression-3-level-factor](/assets/images/regression_hypothesis_testing/regression-3-level-factor-1.png)

Well this is pretty wild, are we still looking at linear regression? The lines are disjoint and broken. What could be going on here? However, note that it does manage to pass through the means of all three groups (based on regression coefficients) which means it does minimize the least squares.

Let's go back to the regression equation. In the regression output above we had an intercept and 2 slopes, meaning there was 2 $x$ variables. Inorder to represent 3 levels of a categorical variable, we need 2 dummy variables. So the dummy coding R is doing will be something like this:

$$ y = m_1x_1 + m_2x_2 + b $$

Where different combinations of $x_1$ & $x_2$ will be different categories. Adelie would be $x_1 = 0, x_2 = 0$, Chinstrap would be $x_1 = 1, x_2 = 0$ and Gentoo would be $x_1 = 0, x_2 = 1 $. So, this is actually regression with 3 dimensions, but our plot was only 2 dimensions. [This post expains the concept beautifully.]( https://stats.stackexchange.com/questions/92065/why-is-polynomial-regression-considered-a-special-case-of-multiple-linear-regres)

We can manually create this coding and verify we get the exact same result:


{% highlight r %}
# Manually create dummy variables:
penguins_df$x1 = ifelse(penguins_df$species == 'Chinstrap',1,0)
penguins_df$x2 = ifelse(penguins_df$species == 'Gentoo',1,0)

kable(penguins_df[sample(nrow(penguins),10),])
{% endhighlight %}



|species   |island    | bill_length_mm| bill_depth_mm| flipper_length_mm| body_mass_g|sex    | year| x1| x2|
|:---------|:---------|--------------:|-------------:|-----------------:|-----------:|:------|----:|--:|--:|
|Gentoo    |Biscoe    |           47.8|          15.0|               215|        5650|male   | 2007|  0|  1|
|Adelie    |Torgersen |           39.1|          18.7|               181|        3750|male   | 2007|  0|  0|
|Gentoo    |Biscoe    |           43.3|          14.0|               208|        4575|female | 2009|  0|  1|
|Adelie    |Dream     |           36.3|          19.5|               190|        3800|male   | 2008|  0|  0|
|Gentoo    |Biscoe    |           47.2|          15.5|               215|        4975|female | 2009|  0|  1|
|Chinstrap |Dream     |           51.9|          19.5|               206|        3950|male   | 2009|  1|  0|
|Gentoo    |Biscoe    |           48.2|          14.3|               210|        4600|female | 2007|  0|  1|
|Chinstrap |Dream     |           40.9|          16.6|               187|        3200|female | 2008|  1|  0|
|Gentoo    |Biscoe    |           40.9|          13.7|               214|        4650|female | 2007|  0|  1|
|Adelie    |Dream     |           36.2|          17.3|               187|        3300|female | 2008|  0|  0|


{% highlight r %}
# Double check that this is the same as the model we did before
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

### Plot regression in 3D

We can visualize the 3D regression to get an idea of where the broken lines come from:

{% highlight r %}
s3d = scatterplot3d(z=penguins_df$body_mass_g, x=penguins_df$x1, y=penguins_df$x2)
s3d$plane3d(three_lvl_dummy,draw_polygon = TRUE)
{% endhighlight %}

![plot of chunk regression-in-3d](/assets/images/regression_hypothesis_testing/regression-in-3d-1.png)

If you try to mentally squish this into a 2D graph like we saw before, you can kind of see how we got the broken graph earlier.


## References:

<https://stats.stackexchange.com/questions/92065/why-is-polynomial-regression-considered-a-special-case-of-multiple-linear-regres>

<https://stats.stackexchange.com/questions/128723/understanding-of-p-value-in-multiple-linear-regression>

<https://stats.stackexchange.com/questions/117406/proof-that-the-coefficients-in-an-ols-model-follow-a-t-distribution-with-n-k-d>

<https://stats.stackexchange.com/questions/285986/distribution-of-linear-regression-coefficients/285992>

<https://stats.stackexchange.com/questions/342632/how-to-understand-se-of-regression-slope-equation/342672>

<https://stats.stackexchange.com/questions/344006/understanding-t-test-for-linear-regression>

<http://home.cc.umanitoba.ca/~godwinrt/4042/material/part3.pdf>
