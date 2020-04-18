#' @title Market Matching and Causal Impact Inference
#'
#' @details  
#' The MarketMatching package can be used to perform the following analyses:
#' 
#' - For all markets in the input dataset, find the best control markets using time series matching.
#' 
#' - Given a test market and a matching control market (from above), analyze the causal impact of an intervention
#' 
#' The package utilizes the dtw package in CRAN to do the time series matching, and the CausalImpact package to do the inference. 
#' (Created by Kay Brodersen at Google). For more information about the CausualImpact package, see the following reference:  
#' 
#' CausalImpact version 1.0.3, Brodersen et al., Annals of Applied Statistics (2015). http://google.github.io/CausalImpact/
#' 
#' The MarketMatching has two separate functions to perform the tasks described above:
#' 
#' - best_matches(): This function finds the best matching control markets for all markets in the input dataset.
#' 
#' - inference(): Given an object from best_matches(), this function analyzes the causal impact of an intervention.
#' 
#' - prospective_power(): Given an object from best_matches(), this function analyzes the power at fake intervention levels
#' For more details, check out the vignette: browseVignettes("MarketMatching")
#' @author Kim Larsen (kblarsen4 at gmail.com)
#' @keywords ts htest
#' @docType package
#' @description 
#' For a given test market find the best matching control markets using time series matching and analyze the impact of an intervention.
#' The intervention could be be a marketing event or some other local business tactic that is being tested. 
#' The package utilizes dynamic time warping to do the matching and the CausalImpact package to analyze the causal impact. 
#' In fact, MarketMatching is simply a wrapper and worfflow for those two packages. 
#' MarketMatching does not provide any functionality that cannot be found in these packages 
#' but simplifies the workflow of using dtw and CausalImpact together 
#' and provides charts and data that are easy to manipulate.
#'
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
#' mm <- best_matches(data=weather, 
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

NULL
