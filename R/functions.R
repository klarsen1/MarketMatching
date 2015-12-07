calculate_distances <- function(all_markets, data, id, i, warping_limit){
    row <- 1
    ThisMarket <- all_markets[i]
    distances <- data.frame(matrix(nrow=length(all_markets), ncol=5))
    names(distances) <- c(id, "BestControl", "RelativeDistance", "Correlation", "Length")
    messages <- 0
    for (j in 1:length(all_markets)){
      ThatMarket <- all_markets[j]
      distances[row, id] <- ThisMarket
      distances[row, "BestControl"] <- ThatMarket 
      mkts <- create_market_vectors(data, ThisMarket, ThatMarket)
      test <- mkts[[1]]
      ref <- mkts[[2]]
      dates <- mkts[[3]]
      if (var(test)==0 & messages==0){
        print(paste0("NOTE: test market ", ThisMarket, " has no variance and hence will be excluded"))
        messages <- messages + 1
      }
      if (ThisMarket != ThatMarket & messages==0 & var(ref)>0){
        dist <- dtw(test, ref, stepPattern=asymmetric, window.type=sakoeChibaWindow, window.size=warping_limit)$distance / abs(sum(test))
        distances[row, "Correlation"] <- cor(test, ref)
        distances[row, "RelativeDistance"] <- dist
        distances[row, "Skip"] <- FALSE
        distances[row, "Length"] <- length(ref)
      } else{
        distances[row, "Skip"] <- TRUE
        distances[row, "RelativeDistance"] <- NA
        distances[row, "Correlation"] <- NA
        distances[row, "Length"] <- NA
      }
      row <- row + 1
    }
    distances <- dplyr::filter(distances, Skip==FALSE) %>%
      dplyr::mutate(dist_rank=rank(RelativeDistance)) %>%
      dplyr::arrange(dist_rank) %>%
      dplyr::select(-dist_rank, -Skip) %>%
      dplyr::filter(row_number()==1)
    
    distances$MatchingStartDate <- min(dates)
    distances$MatchingEndDate <- max(dates)
    return(distances)  
}

stopif <- function(value, clause, message){
  if (value==clause){
    stop(message)
  }
}

check_inputs <- function(data=NULL, id=NULL, matching_variable=NULL, date_variable=NULL){
  stopif(is.null(data), TRUE, "ERROR: No data is provided")
  stopif(is.null(id), TRUE, "ERROR: No ID is provided")
  stopif(is.null(matching_variable), TRUE, "ERROR: No matching metric is provided")
  stopif(is.null(date_variable), TRUE, "ERROR: No date variable is provided")
  stopif(id %in% names(data), FALSE, "ERROR: ID variable not found in input data")
  stopif(date_variable %in% names(data), FALSE, "ERROR: date variable not found in input data")
  stopif(matching_variable %in% names(data), FALSE, "ERROR: matching metric not found in input data")
  stopif(length(unique(data[[id]]))>2, FALSE, "ERROR: Need at least 3 unique markets")
}

create_market_vectors <- function(data, test_market, ref_market){
   d <- subset(data, !is.na(match_var))
   test <- subset(data, id_var==test_market)$match_var
   ref <- subset(data, id_var==ref_market)$match_var
   date <- subset(data, id_var==test_market)$date_var
   return(list(test, ref, date))
}

mape_no_zeros <- function(test, ref){
  d <- cbind.data.frame(test, ref)
  d <- subset(d, abs(test)>0)
  return(mean(abs(lm(test ~ ref, data=d)$residuals)/d$test))
}

#' For each market, find the best matching control market

#' \code{best_matches} finds the best macthing control market for each market in the dataset. 
#' The function returns an object of type "market_matching" using dynamic time warping (dtw package). 
#' The element called "BestMatches" is a data.frame that shows the best matching control markets and the relative distances.

#' @param data input data.frame for analysis
#' @param id the name of the variable that identifies the markets
#' @param date_variable the time stamp variable
#' @param matching_variable the variable (metric) used to match the markets. For example, this could be sales or new customers
#' @param parallel set to TRUE for parallel processing. Default is TRUE
#' @param warping_limit the warping limit used for matching. Defauls is 2
#' @param start_match_period the start date of the matching period (pre period). 
#' Must be a character of format "YYYY-MM-DD" -- e.g., "2015-01-01"
#' @param end_match_period the end date of the matching period (pre period). 
#' Must be a character of format "YYYY-MM-DD" -- e.g., "2015-10-01"

#' @import foreach
#' @importFrom parallel detectCores 
#' @importFrom data.table rbindlist
#' @import dplyr
#' @import iterators
#' @import utils
#' @import dtw
#' @importFrom doParallel registerDoParallel stopImplicitCluster

#' @export best_matches
#' @examples  
#' ##-----------------------------------------------------------------------
#' ## Find best matches for each airport time series
#' ##-----------------------------------------------------------------------
#' library(MarketMatching)
#' data(weather, package="MarketMatching")
#' mm <- best_matches(data=weather, id="Area", 
#'                    date_variable="Date", 
#'                    matching_variable="Mean_TemperatureF", 
#'                    parallel=FALSE, 
#'                    start_match_period="2014-01-01",
#'                    end_match_period="2014-10-01")
#' head(mm$BestMatches)

best_matches <- function(data=NULL, id=NULL, date_variable=NULL, matching_variable=NULL, warping_limit=2, parallel=TRUE, start_match_period=NULL, end_match_period=NULL){
  
  ## Check the start date and end dates
  stopif(is.null(start_match_period), TRUE, "No start date provided")
  stopif(is.null(end_match_period), TRUE, "No end date provided")
  
  ## check the inputs
  check_inputs(data=data, id=id, matching_variable=matching_variable, date_variable=date_variable)
  data$date_var <- data[[date_variable]]
  data$id_var <- data[[id]]
  data$match_var <- data[[matching_variable]]
  
  data <- dplyr::arrange(data, id_var, date_var) %>% ungroup() %>% select(id_var, date_var, match_var)
  
  ## save a reduced version of the data
  saved_data <- data
  
  ## get a vector of all markets
  all_markets <- unique(data$id_var)
  
  ## set up a list to hold all distance matrices
  all_distances <- list()
  
  ## filter the dates
  data <- dplyr::filter(data, date_var>=as.Date(start_match_period) & date_var<=as.Date(end_match_period))

  ## check if any data is left
  stopif(nrow(data)>0, FALSE, "ERROR: no data left after filter for dates")
  
  ## loop through markets and compute distances
  if (parallel==FALSE){
    for (i in 1:length(all_markets)){
      all_distances[[i]] <- calculate_distances(all_markets, data, id, i, warping_limit)
    }
    shortest_distances <- data.frame(rbindlist(all_distances))
  } else{
    ncore <- detectCores()-1
    registerDoParallel(ncore)
    loop_result <- foreach(i=1:length(all_markets)) %dopar% {
      calculate_distances(all_markets, data, id, i, warping_limit)
    }
    shortest_distances <- data.frame(rbindlist(loop_result))
    stopImplicitCluster()
  }
  
  ### Return the results
  object <- list(BestMatches=shortest_distances, Data=as.data.frame(saved_data), MarketID=id, MatchingMetric=matching_variable, DateVariable=date_variable)
  class(object) <- "matched_market"
  return (object)
}


#' Given a test market, analyze the impact of an intervention
 
#' \code{inference} Analyzes the causal impact of an intervention using the CausalImpact package, given a test market and a matched_market object from the best_matches function.
#' The function returns an object of type "market_inference" which contains the estimated impact of the intervention (absolute and relative).

#' @param matched_markets A matched_market object created by the market_matching function
#' @param test_market The name of the test market (character)
#' @param end_post_period The end date of the post period. Must be a character of format "YYYY-MM-DD" -- e.g., "2015-11-01"
#' @param alpha Desired tail-area probability for posterior intervals. For example, 0.05 yields 0.95 intervals

#' @import CausalImpact
#' @import scales
#' @import Boom
#' @import ggplot2
#' @import zoo

#' @export inference
#' @examples  
#' library(MarketMatching)
#' ##-----------------------------------------------------------------------
#' ## Analyze causal impact of a made-up weather intervention in Copenhagen
#' ## Since this is weather data this is a meaningless example and we should 
#' ## expect no causal impact. This is just to demo the function.
#' ##-----------------------------------------------------------------------
#' data(weather, package="MarketMatching")
#' mm <- best_matches(data=weather, id="Area", 
#'                    date_variable="Date", 
#'                    matching_variable="Mean_TemperatureF", 
#'                    parallel=FALSE, 
#'                    start_match_period="2014-01-01",
#'                    end_match_period="2014-10-01")
#' results <- inference(matched_markets=mm, test_market="CPH", end_post_period="2015-12-15")

inference <- function(matched_markets=NULL, test_market=NULL, end_post_period=NULL, alpha=0.05){

  ## copy the distances
  mm <- matched_markets$BestMatches
  
  data <- matched_markets$Data  
  mm$id_var <- mm[[names(mm)[1]]]
  
  ## check if the test market exists
  stopif(test_market %in% unique(data$id_var), FALSE, paste0("test market ", test_market, " does not exist"))
  
  # filter for dates
  data <- dplyr::filter(data, date_var>=as.Date(mm[1, "MatchingStartDate"]) & date_var<=as.Date(end_post_period))

  ## get the control market name
  control_market <- subset(mm, id_var==test_market)$BestControl
  
  ## get the test and ref markets
  mkts <- create_market_vectors(data, test_market, control_market)
  test <- mkts[[1]]
  ref <- mkts[[2]]
  date <- mkts[[3]]
  post_period <- date[date > as.Date(mm[1, "MatchingEndDate"])]
  stopif(length(post_period)==0, TRUE, "ERROR: no valid data in the post period")
  post_period_start_date <- min(post_period)
  post_period_end_date <- max(post_period)
  ts <- zoo(cbind.data.frame(test, ref), date)
  names(ts) <- c("y", "x1")

  ## get the dates for the pre-period
  MatchingStartDate <- subset(mm, id_var==test_market)$MatchingStartDate
  MatchingEndDate <- subset(mm, id_var==test_market)$MatchingEndDate

  ## print the settings
  cat("\t------------- Inputs -------------\n")
  cat(paste0("\tTest Market: ", test_market, "\n"))
  cat(paste0("\tControl Market: ", control_market, "\n"))
  cat(paste0("\tMarket ID: ", matched_markets$MarketID, "\n"))
  cat(paste0("\tDate Variable: ", matched_markets$DateVariable, "\n"))
  cat(paste0("\tMatching (pre) Period Start Date: ", MatchingStartDate, "\n"))
  cat(paste0("\tMatching (pre) Period End Date: ", MatchingEndDate, "\n"))
  cat(paste0("\tPost Period Start Date: ", post_period_start_date, "\n"))
  cat(paste0("\tPost Period End Date: ", post_period_end_date, "\n"))
  cat(paste0("\tMatching Metric: ", matched_markets$MatchingMetric, "\n"))
  cat("\n")
  cat("\n")
  
  ## run the inference
  pre.period <- c(as.Date(MatchingStartDate), as.Date(MatchingEndDate))
  post.period <- c(as.Date(post_period_start_date), as.Date(post_period_end_date))
  impact <- CausalImpact(ts, pre.period, post.period, alpha=alpha)

  ## create statistics
  results <- list()
  results[[1]] <- impact$summary$AbsEffect[2]
  results[[2]] <- impact$summary$AbsEffect.lower[2]
  results[[3]] <- impact$summary$AbsEffect.upper[2]
  results[[4]] <- impact$summary$RelEffect[2]
  results[[5]] <- impact$summary$RelEffect.lower[2]
  results[[6]] <- impact$summary$RelEffect.upper[2]
  results[[7]] <- impact$summary$p[2]

  ## compute mape
  preperiod <- subset(impact$series, cum.effect == 0)
  results[[8]] <- mape_no_zeros(preperiod$response, preperiod$point.pred)
  
  ## create actual versus predicted plots
  plotdf <- cbind.data.frame(as.Date(row.names(data.frame(impact$series))), data.frame(impact$series)[,c("response", "point.pred", "point.pred.lower", "point.pred.upper")])
  names(plotdf) <- c("Date", "Response", "Predicted", "lower_bound", "upper_bound")
  results[[9]] <- ggplot(data=plotdf, aes(x=Date)) + 
                  geom_line(aes(y=Predicted, colour = "Actuals (test market)")) + 
                  geom_ribbon(aes(ymin=lower_bound, ymax=upper_bound), fill="grey", alpha=0.3) + 
                  geom_line(aes(y=Response, colour = "Expected based on control")) + 
                  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") + 
                  scale_colour_manual(breaks = c("Actuals (test market)", "Expected based on control"), values = c("black", "gray")) +
                  geom_vline(xintercept=as.numeric(MatchingEndDate), linetype=2)
    
    
  ## create lift plots
  plotdf <- cbind.data.frame(as.Date(row.names(data.frame(impact$series))), data.frame(impact$series)[,c("cum.effect", "cum.effect.lower", "cum.effect.upper")])
  names(plotdf) <- c("Date", "Cumulative", "lower_bound", "upper_bound")
  results[[10]] <- ggplot(data=plotdf, aes(x=Date, y=Cumulative)) + geom_line() + theme_bw() + 
                   scale_y_continuous(labels = comma) + ylab("Absolute Cumulative Effect") + xlab("") + 
                   geom_vline(xintercept=as.numeric(MatchingEndDate), linetype=2) + 
                   geom_ribbon(aes(ymin=lower_bound, ymax=upper_bound), fill="grey", alpha=0.3)

  ## create actual versus predicted plots
  plotdf <- cbind.data.frame(test, ref, date)
  results[[11]] <- ggplot(data=plotdf, aes(x=date)) + 
    geom_line(aes(y=test, colour = "Actuals (test market)")) + 
    geom_line(aes(y=ref, colour = "Actuals (control market)")) + 
    theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") + 
    scale_colour_manual(breaks = c("Actuals (test market)", "Actuals (control market)"), values = c("black", "gray")) +
    geom_vline(xintercept=as.numeric(MatchingEndDate), linetype=2)
  
  ### print results
  cat("\t------------- Effect Analysis -------------\n")
  cat(paste0("\tAbsolute Effect: ", round(results[[1]],2), " [", round(results[[2]],2), ", ", round(results[[3]],2), "]\n"))
  cat(paste0("\tRelative Effect: ", paste0(round(100*results[[4]],2), "%"), " [", paste0(round(100*results[[5]],2), "%"), ", ", paste0(round(100*results[[6]],2), "%"), "]\n"))
  cat(paste0("\tProbability of a causal impact: ", paste0(round(100*(1-results[[7]]),4), "%\n")))
  
  ### return the results
  object <- list(AbsoluteEffect=results[[1]], AbsoluteEffectLower=results[[2]], AbsoluteEffectUpper=results[[3]], 
                 RelativeEffect=results[[4]], RelativeEffectLower=results[[5]], RelativeEffectUpper=results[[6]], 
                 TailProb=results[[7]], PrePeriodMAPE=results[[8]], PlotActualVersusExpected=results[[9]], PlotAbsoluteEffect=results[[10]],
                 PlotActuals=results[[11]])
  class(object) <- "matched_market_inference"
  return (object)
}
