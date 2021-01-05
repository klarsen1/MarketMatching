#' @title Market Matching and Causal Impact Inference
#'
#' @details  
#' The MarketMatching package can be used to perform the following analyses:
#' 
#' - For all markets in the input dataset, find the best control markets using time series matching.
#' 
#' - Given a test market and a matching control market (from above), analyze the causal impact of an intervention.
#' 
#' - Create optimal test/control market splits and run pseudo prospective power analyses.
#' 
#' The package utilizes the dtw package in CRAN to do the time series matching, and the CausalImpact package to do the inference. 
#' (Created by Kay Brodersen at Google). For more information about the CausualImpact package, see the following reference:  
#' 
#' CausalImpact version 1.0.3, Brodersen et al., Annals of Applied Statistics (2015). http://google.github.io/CausalImpact/
#' 
#' The MarketMatching has two separate functions to perform the tasks described above:
#' 
#' - best_matches(): This function finds the best matching control markets for all markets in the input dataset. If you don't know the test markets
#' the funcrtion can also provide suggested optimized test/control pairs.
#' 
#' - inference(): Given an object from best_matches(), this function analyzes the causal impact of an intervention.
#' 
#' - test_fake_lift(): Calculate the probability of a causal impact for fake interventions (prospective pseudo power).
#' 
#' For more details, check out the vignette: browseVignettes("MarketMatching")
#' 
#' @author Kim Larsen (kblarsen4 at gmail.com)
#' @keywords ts htest
#' @docType package
#' @description 
#' For a given test market find the best matching control markets using time series matching and analyze the impact of an intervention (prospective or historical).
#' The intervention could be be a marketing event or some other local business tactic that is being tested. 
#' The package utilizes dynamic time warping to do the matching and the CausalImpact package to analyze the causal impact. 
#' In fact, MarketMatching is simply a wrapper and worfflow for those two packages. 
#' MarketMatching does not provide any functionality that cannot be found in these packages 
#' but simplifies the workflow of using dtw and CausalImpact together. In addition, if you don't already have a set of test markets to match, `MarketMatching` 
#' can provide suggested test/control market pairs using the `suggest_market_splits` option 
#' in the `best_matches()` function. Also, the `test_fake_lift()` function provides pseudo prospective power analysis if you're using the `MarketMatching` 
#' package to create your test design (i.e., not just doing the post inference).
#'
#' @name MarketMatching
#' @examples
#' 
#' ##-----------------------------------------------------------------------
#' ## Find best matches for CPH
#' ## If we leave test_market as NULL, best matches are found for all markets
#' ##-----------------------------------------------------------------------
#' library(MarketMatching)
#' data(weather, package="MarketMatching")
#' mm <- MarketMatching::best_matches(data=weather, 
#'                    id="Area",
#'                    date_variable="Date",
#'                    matching_variable="Mean_TemperatureF",
#'                    parallel=FALSE,
#'                    markets_to_be_matched="CPH",
#'                    warping_limit=1, # warping limit=1
#'                    dtw_emphasis=1, # rely only on dtw for pre-screening
#'                    matches=5, # request 5 matches
#'                    start_match_period="2014-01-01",
#'                    end_match_period="2014-10-01")
#' head(mm$Distances)
#' 
#' ##-----------------------------------------------------------------------
#' ## Analyze causal impact of a made-up weather intervention in Copenhagen
#' ## Since this is weather data it is a not a very meaningful example. 
#' ## This is merely to demonstrate the functionality.
#' ##-----------------------------------------------------------------------
#' results <- MarketMatching::inference(matched_markets = mm, 
#'                                      test_market = "CPH", 
#'                                      analyze_betas=FALSE,
#'                                      end_post_period = "2015-10-01", 
#'                                      prior_level_sd = 0.002)
#' 
#' ## Plot the impact
#' results$PlotCumulativeEffect
#' 
#' ## Plot actual observations for test market (CPH) versus the expectation (based on the control)
#' results$PlotActualVersusExpected
#' 
#' ##-----------------------------------------------------------------------
#' ## Power analysis for a fake intervention ending at 2015-10-01
#' ## The maximum lift analyzed is 5 percent, the minimum is 0 (using 5 steps)
#' ## Since this is weather data it is a not a very meaningful example. 
#' ## This is merely to demonstrate the functionality.
#' ##-----------------------------------------------------------------------
#' power <- MarketMatching::test_fake_lift(matched_markets = mm, 
#'                                      test_market = "CPH", 
#'                                      end_fake_post_period = "2015-10-01", 
#'                                      prior_level_sd = 0.002, 
#'                                      steps=20,
#'                                      max_fake_lift=0.05)
#' 
#' ## Plot the curve
#' power$ResultsGraph
#' 
#' ##-----------------------------------------------------------------------
#' ## Generate suggested test/control pairs
#' ##-----------------------------------------------------------------------
#'
#'data(weather, package="MarketMatching")
#'mm <- MarketMatching::best_matches(data=weather,
#'                                   id_variable="Area",
#'                                   date_variable="Date",
#'                                   matching_variable="Mean_TemperatureF",
#'                                   suggest_market_splits=TRUE,
#'                                   parallel=FALSE,
#'                                   dtw_emphasis=0, # rely only on correlation for this analysis
#'                                   start_match_period="2014-01-01",
#'                                   end_match_period="2014-10-01")
#'
#'##-----------------------------------------------------------------------
#'## The file that contains the suggested test/control splits
#'## The file is sorted from the strongest market pair to the weakest pair.
#'##-----------------------------------------------------------------------
#'head(mm$SuggestedTestControlSplits)
#'
#'##-----------------------------------------------------------------------
#'## Pass the results to test_fake_lift to get pseudo power curves for the splits.
#'## This tells us how well the design can detect various lifts.
#'## Not a meaningful example for this data. Just to illustrate.
#'## Note that the rollup() function will aggregate the test and control markets. 
#'## The new aggregated test markets will be labeled "TEST."
#'##-----------------------------------------------------------------------
#'rollup <- MarketMatching::roll_up_optimal_pairs(matched_markets = mm, 
#'                                                synthetic=FALSE)
#'
#'power <- MarketMatching::test_fake_lift(matched_markets = rollup, 
#'                                        test_market = "TEST",
#'                                        end_fake_post_period = "2015-10-01",
#'                                        lift_pattern_type = "constant",
#'                                        max_fake_lift = 0.1)

NULL
