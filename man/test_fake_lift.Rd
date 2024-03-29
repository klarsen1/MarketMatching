% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{test_fake_lift}
\alias{test_fake_lift}
\title{Given a test market, analyze the impact of fake interventions (prospective power analysis)}
\usage{
test_fake_lift(matched_markets=NULL,
          test_market=NULL,
          end_fake_post_period=NULL,
          alpha=0.05,
          prior_level_sd=0.01,
          control_matches=NULL, 
          nseasons=NULL, 
          max_fake_lift=NULL, 
          steps=10,
          lift_pattern_type="constant")
}
\arguments{
\item{matched_markets}{A matched_market object created by the market_matching function
This parameter will overwrite the values specified in prior_level_sd and nseasons. ONLY use this if you're using intricate bsts settings
For most use-cases, using the prior_level_sd and nseasons parameters should be sufficient}

\item{test_market}{The name of the test market (character)}

\item{end_fake_post_period}{The end date of the post period. Must be a character of format "YYYY-MM-DD" -- e.g., "2015-11-01"}

\item{alpha}{Desired tail-area probability for posterior intervals. For example, 0.05 yields 0.95 intervals}

\item{prior_level_sd}{Prior SD for the local level term (Gaussian random walk). Default is 0.01. The bigger this number is, the more wiggliness is allowed for the local level term.
Note that more wiggly local level terms also translate into larger posterior intervals
This parameter will be overwritten if you're using the bsts_modelargs parameter}

\item{control_matches}{Number of matching control markets to use in the analysis (default is 5)}

\item{nseasons}{Seasonality for the bsts model -- e.g., 52 for weekly seasonality}

\item{max_fake_lift}{The maximum absolute fake lift -- e.g., 0.1 means that the max lift evaluated is 10 percent and the min lift is -10 percent
Note that randomization is injected into the lift, which means that the max lift will not be exactly as specified}

\item{steps}{The number of steps used to calculate the power curve (default is 10)}

\item{lift_pattern_type}{Lift pattern. Default is constant. The other choice is a random lift..}
}
\value{
Returns an object of type \code{matched_market_power}. The object has the
following elements:
\item{\code{ResultsData}}{The results stored in a data.frame}
\item{\code{ResultsGraph}}{The results stored in a ggplot graph}
\item{\code{LiftPattern}}{The random pattern applied to the lift}
\item{\code{FitCharts}}{The underlying actual versus fitted charts for each fake lift} 
\item{\code{FitData}}{The underlying actual versus fitted data for each fake lift}
}
\description{
\code{test_fake_lift} Given a matched_market object from the best_matches function, this function analyzes the causal impact of fake interventions using the CausalImpact package.
The function returns an object of type "market_inference" which contains the estimated impact of the intervention (absolute and relative).
}
\examples{
\dontrun{
library(MarketMatching)
##-----------------------------------------------------------------------
## Create a pseudo power curve for various levels of lift
## Since this is weather data it is a not a very meaningful example. 
## This is merely to demonstrate the function.
##-----------------------------------------------------------------------
data(weather, package="MarketMatching")
mm <- best_matches(data=weather, 
                   id="Area",
                   markets_to_be_matched=c("CPH", "SFO"),
                   date_variable="Date",
                   matching_variable="Mean_TemperatureF",
                   warping_limit=1, # warping limit=1
                   dtw_emphasis=0, # rely only on dtw for pre-screening
                   matches=5, # request 5 matches
                   start_match_period="2014-01-01",
                   end_match_period="2014-10-01")
library(CausalImpact)
results <- test_fake_lift(matched_markets=mm,
                     test_market="CPH",
                     lift_pattern_type="constant",
                     control_matches=5, # use all 5 matches for inference
                     end_fake_post_period="2015-12-15",
                     prior_level_sd=0.002, 
                     max_fake_lift=0.1)
}
}
