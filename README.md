Introduction
============

If you ever spent time in the field of marketing analytics, chances are
that you have analyzed the existence of a causal impact from a new local
TV campaign, a major PR event, or the emergence of a new local
competitor. From an analytical standpoint these type of events all have
one thing in common: the impact cannot be tracked at the individual
customer level and hence we have to analyze the impact from a bird’s eye
view using time series analysis at the market level (e.g., DMA, state,
etc.). Data science may be changing at a fast pace but this is an old
school use-case that is still very relevant no matter what industry
you’re in.

Intervention analyses require more judgement than evaluation of
randomized test/control studies. When analyzing interventions through
time series analysis we typically go through two steps, each of which
can involve multiple analytical decisions:

1.  Find matching *control* markets for the *test* market where the
    event took place using time series matching based on historical data
    prior to the event (the “pre period”).
2.  Analyze the causal impact of the event by comparing the observed
    data for the test and control markets following the event (the “post
    period”), while factoring in differences between the markets prior
    to the event.

The purpose of this document is to describe a robust approach to
intervention analysis based on two key `R` packages: the `CausalImpact`
package written by Kay Brodersen at Google and the `dtw` package
available in CRAN. In addition, we will introduce an `R` package called
`MarketMatching` which implements a simple intervention analysis
workflow based on these two packages.

A Traditional Approach
----------------------

For the time series matching step the most straightforward approach is
to use the Euclidian distance. However, this approach implicitly
over-penalizes instances where relationships between markets are
temporarily shifted. Although it is preferable for test and control
markets to be aligned consistently, occasional historical shifts should
not eliminate viable control market candidates. Or another option is to
match based on correlation, but this does not factor in size.

For the inference step, the traditional approach is a “diff in diff”
analysis. This is typically a static regression model that evaluates the
post-event change in the difference between the test and control
markets. However, this assumes that observations are i.i.d. and that the
differences between the test and control markets are constant. Both
assumptions rarely hold true for time series data.

A More Flexible and Robust Approach
-----------------------------------

A better approach is to use *dynamic time warping* to do the time series
matching (see \[2\]) . This technique finds the distance along the
*warping curve* – instead of the raw data – where the warping curve
represents the best alignment between two time series within some
user-defined constraints. Note that the Euclidian distance is a special
case of the warped distance.

For the intervention analysis the `CausalImpact` package provides an
approach that is more flexible and robust than the “diff in diff” model
(see \[1\]). The `CausalImpact` package constructs a synthetic baseline
for the post-intervention period based on a Bayesian structural time
series model that incorporates *multiple* matching control markets as
predictors, as well as other features of the time series.

We can summarize this workflow as follows:

1.  Pre-screening step: find the best control markets for each market in
    the dataset using dynamic time warping. The user can define how many
    matches should be retained. Note that this step merely creates a
    list of candidates markets; the final markets used for the
    post-event inference will be decided in the next step.

Note: If you don’t have a set of test markets to match, the
`MarketMatching` can provide suggested test/control market pairs using
the `suggest_market_splits`option in the `best_matches()` function.
Also, the `test_fake_lift()` function provides pseudo prospective power
analysis if you’re using the `MarketMatching` package to create your
test design (i.e., not just doing the post inference).

1.  Inference step: fit a Bayesian structural time series model that
    utilizes the control markets identified in step 1 as predictors.
    Based on this model, create a synthetic control series by producing
    a counterfactual prediction for the post period assuming that the
    event did not take place. We can then calculate the difference
    between the synthetic control and the test market for the
    post-intervention period – which is the estimated impact of the
    event – and compare to the posterior interval to gauge uncertainty.

### Notes on the Workflow

As mentioned above, the purpose of the dynamic time warping step is to
create a list of viable control market candidates. This is not a
strictly necessary step as we can select markets directly while building
the time series model during step 2. In fact, the `CausalImpact` package
selects the most predictive markets for the structural time series model
using spike-and-slab priors (for more information, see the technical
details below).

However, when dealing with a large set of candidate control markets it
is often prudent to trim the list of markets in advance as opposed to
relying solely on the variable selection process. Creating a synthetic
control based on markets that have small *distances* to the test market
tends to boost the face-validity of the analysis as similar-sized
markets are easily recognized as strong controls through simple line
plots.

Ultimately, however, this is a matter of preference and the good news is
that the `MarketMatching` package allows users to decide how many
control markets should be included in the pre-screen. The user can also
choose whether the pre-screening should be correlation-based or based on
time-warped distances, or some mix of the two.

About MarketMatching Package
============================

The `MarketMatching` package implements the workflow described above by
essentially providing an easy-to-use “wrapper” for the `dtw` and
`CausalImpact`. The function `best_matches()` finds the best control
markets for each market by looping through all viable candidates in a
parallel fashion and then ranking by distance and/or correlation. The
resulting output object can then be passed to the `inference()` function
which then analyzes the causal impact of an event using the pre-screened
control markets.

Hence, the package does *not* provide any new core functionality but it
simplifies the workflow of using `dtw` and `CausalImpact` together *and*
provides charts and data that are easy to manipulate. `R` packages are a
great way of implementing and documenting workflows.

Summary of features:
--------------------

-   Minimal inputs required. The only strictly necessary inputs are the
    name of the test market (for inference), the dates of the pre-period
    and post-period and, of course, the data.
-   Provides a data.frame with the best matches for all markets in the
    input dataset. The number of matches can be defined by the user.
-   Outputs all inference results as objects with intuitive names (e.g.,
    “AbsoluteEffect” and “RelativeEffect”).
-   Checks the quality of the input data and eliminates “bad” markets.
-   Calculates MAPE and Durbin-Watson for the pre-period. Shows how
    these statistics change when you alter the prior standard error of
    the local level term.
-   Plots and outputs the actual data for the markets selected during
    the initial market matching.
-   Plots and outputs actual versus predicted values.
-   Plots the final local level term.
-   Shows the average estimated coefficients for all the markets used in
    the linear regression component of the structural time series model.
-   Allows the user to choose how many markets are sent to the
    slab-and-prior model.
-   All plots are done in `ggplot2` and can easily be extracted and
    manipulated.
-   Facilitates pseudo prospective epower analysis for geo-based
    experiments.
-   Provides suggested optimal test/control market pairs for future
    studies (in case the test markets have not been identified).

How to Install
==============

``` r
## THIS PACKAGE IS IN CRAN.
## If you want to install from Github, use devtools version 1.11.1
## packageurl <- "http://cran.r-project.org/src/contrib/Archive/devtools/devtools_1.11.1.tar.gz"
## install.packages(packageurl, repos=NULL, type="source")
## library(devtools)
## install_github("klarsen1/MarketMatching", build_vignettes=TRUE)
library(MarketMatching)
```

Examples
========

The dataset supplied with the package has daily temperature readings for
20 areas (airports) for 2014. The dataset is a stacked time series
(panel data) where each row represents a unique combination of date and
area. It has three columns: area, date, and the average temperature
reading for the day.

This is *not* the most appropriate dataset to demonstrate intervention
inference, as humans cannot affect the weather in the short term (long
term impact is a different blog post). We’ll merely use the data to
demonstrate the features.

``` r
##-----------------------------------------------------------------------
## Find the best matches (default is 5) for each airport time series
##-----------------------------------------------------------------------
library(MarketMatching)
data(weather, package="MarketMatching")
mm <- best_matches(data=weather,
                   id_variable="Area",
                   date_variable="Date",
                   matching_variable="Mean_TemperatureF",
                   parallel=FALSE,
                   warping_limit=1, # warping limit=1
                   dtw_emphasis=1, # rely only on dtw for pre-screening
                   matches=5, # request 5 matches
                   start_match_period="2014-01-01",
                   end_match_period="2014-10-01")

##-----------------------------------------------------------------------
## Or just search for 5 control markets for CPH and SFO
##-----------------------------------------------------------------------
mm_only_cph <- best_matches(data=weather,
                   id_variable="Area",
                   date_variable="Date",
                   markets_to_be_matched=c"CPH", "SFO"),
                   matching_variable="Mean_TemperatureF",
                   parallel=FALSE,
                   warping_limit=1, # warping limit=1
                   dtw_emphasis=1, # rely only on dtw for pre-screening
                   matches=5, # request 5 matches
                   start_match_period="2014-01-01",
                   end_match_period="2014-10-01")


##-----------------------------------------------------------------------
## Analyze causal impact of a made-up weather intervention in Copenhagen
## Since this is weather data it is a not a very meaningful example.
## This is merely to demonstrate the functionality.
##-----------------------------------------------------------------------
results <- MarketMatching::inference(matched_markets = mm,
                                    test_market = "CPH",
                                    end_post_period = "2015-10-01")

##-----------------------------------------------------------------------
## You can also pass specific bsts model arguments (see bsts documentation)
##-----------------------------------------------------------------------
results <- MarketMatching::inference(matched_markets = mm,
                                    test_market = "CPH",
                                    analyze_betas=TRUE,
                                    bsts_modelargs = list(niter=2000, prior.level.sd=0.001),
                                    end_post_period = "2015-10-01")
```

A view of the best matches data.frame generated by the best\_matches()
function:

``` r
knitr::kable(head(mm$BestMatches))
```

Plot actual observations for test market (CPH) versus the expectation.
It looks like CPH deviated from its expectation during the winter:

``` r
results$PlotActualVersusExpected
```

Plot the cumulative impact. The posterior interval includes zero as
expected, which means that the cumulative deviation is likely noise:

``` r
results$PlotCumulativeEffect
```

Although it looks like some of the dips in the *point-wise* effects
toward the end of the post period seem to be truly negative:

``` r
results$PlotPointEffect
```

Store the actual versus predicted values in a data.frame:

``` r
pred <- results$Predictions
knitr::kable(head(pred))
```

Plot the actual data for the test and control markets:

``` r
results$PlotActuals
```

Check the Durbin-Watson statistic (DW), MAPE and largest market
coefficient for different values of the local level SE. It looks like it
will be hard to get a DW statistic close to 2, although our model may
benefit from a higher local level standard error than the default of
0.01:

``` r
results$PlotPriorLevelSdAnalysis
```

Store the average posterior coefficients in a data.frame. STR
(Stuttgart) receives the highest weight when predicting the weather in
Copenhagen:

``` r
coeff <- results$Coefficients
knitr::kable(head(coeff))
```

Prospective Power Curves
========================

In this example, we’re calculating power (probability of a causal impact
at alpha=0.05) using a fake interventions starting after 2014-10-01 and
ending at 2015-10-01. We’re analyzing fake lifts from 0 to 5 percent in
5 steps (default is 10). This will help you evaluate if your choice of
test and control markets creates a sufficient model to measure a
realistic lift from a future intervention.

``` r
##-----------------------------------------------------------------------
## Find the best 5 matches for each airport time series. Matching will
## rely entirely on dynamic time warping (dtw) with a limit of 1
##-----------------------------------------------------------------------

library(MarketMatching)
data(weather, package="MarketMatching")
mm <- best_matches(data=weather,
                   id_variable="Area",
                   date_variable="Date",
                   matching_variable="Mean_TemperatureF",
                   parallel=FALSE,
                   warping_limit=1, # warping limit=1
                   dtw_emphasis=1, # rely only on dtw for pre-screening
                   matches=5, # request 5 matches
                   start_match_period="2014-01-01",
                   end_match_period="2014-10-01")
#' ##-----------------------------------------------------------------------
#' ## Power analysis for a fake intervention ending at 2015-10-01
#' ## The maximum lift analyzed is 10 percent, the minimum is 0 percent
#' ## Since this is weather data it is a not a very meaningful example. 
#' ## This is merely to demonstrate the functionality.
#' ##-----------------------------------------------------------------------
power <- MarketMatching::prospective_power(matched_markets = mm, 
                                      test_market = "CPH", 
                                      end_fake_post_period = "2015-10-01", 
                                      prior_level_sd = 0.002, 
                                      steps=5,
                                      max_fake_lift=0.05)
```

Inspecting the power curve:

``` r
power$ResultsGraph
```

Getting optimized market pairs (test/control) recommendations
=============================================================

This example shows how to get test/control market pair suggestions from
the distances. The package stratifies the markets by size (sum of Y) and
the creates pairs based on the correlation of logged values. To invoke
this markets\_to\_matched must be NULL.

Once the optimized pairs have been generated they are passed to the
pseudo power function for evaluation. The `synthetic` parameter in the
roll\_up\_optimal\_pairs function determines if the control markets will
be aggregated (equal weights in `bsts` and `CausalImpact`) or if they’ll
be left as individual markets and get separate weighths (synthetic
control).

``` r
##-----------------------------------------------------------------------
## Find all matches for each airport (market) time series. 
##-----------------------------------------------------------------------
library(MarketMatching)
data(weather, package="MarketMatching")
mm <- MarketMatching::best_matches(data=weather,
                   id_variable="Area",
                   date_variable="Date",
                   matching_variable="Mean_TemperatureF",
                   suggest_market_splits=TRUE,
                   parallel=FALSE,
                   warping_limit=1, # warping limit=1
                   dtw_emphasis=0, # rely only on correlation
                   start_match_period="2014-01-01",
                   end_match_period="2014-10-01")
##-----------------------------------------------------------------------
## The file that contains the suggested test/control splits
## The file is sorted from the strongest market pair to the weakest pair.
##-----------------------------------------------------------------------
head(mm$SuggestedTestControlSplits)

##-----------------------------------------------------------------------
## Pass the results to test_fake_lift to get pseudo power curves for the splits
## Not a meaningful example for this data. Just to illustrate.
## Note that the rollup() function will label the test markets "TEST"
##-----------------------------------------------------------------------
rollup <- MarketMatching::roll_up_optimal_pairs(matched_markets = mm, 
                                synthetic=FALSE)

power <- MarketMatching::test_fake_lift(matched_markets = rollup, 
                        test_market = "TEST",
                        end_fake_post_period = "2015-10-01",
                        lift_pattern_type = "constant",
                        steps=20, 
                        max_fake_lift = 0.1)
```

References
==========

\[1\] CausalImpact version 1.0.3, Brodersen et al., Annals of Applied
Statistics (2015).

\[2\] The vignette for the `dtw` package (browseVignettes(“dtw”))

\[3\] Predicting the Present with Bayesian Structural Time Series,
Steven L. Scott and Hal Varian,
<a href="http://people.ischool.berkeley.edu/~hal/Papers/2013/pred-present-with-bsts.pdf" class="uri">http://people.ischool.berkeley.edu/~hal/Papers/2013/pred-present-with-bsts.pdf</a>.
