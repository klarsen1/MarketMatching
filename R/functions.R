lagp <- function(x, p){
  return(c(rep(0,p), x[1:(length(x)-p)]))
}

CMean <- function(b) {
  b <- b[b != 0]
  if (length(b) > 0) 
    return(mean(b))
  return(0)
}

calculate_distances <- function(all_markets, data, id, i, warping_limit, matches, dtw_emphasis){
  row <- 1
  ThisMarket <- all_markets[i]
  distances <- data.frame(matrix(nrow=length(all_markets), ncol=5))
  names(distances) <- c(id, "BestControl", "RelativeDistance", "Correlation", "Length")
  messages <- 0
  # For each market
  for (j in 1:length(all_markets)){
    isValidTest <- TRUE
    ThatMarket <- all_markets[j]
    distances[row, id] <- ThisMarket
    distances[row, "BestControl"] <- ThatMarket
    mkts <- create_market_vectors(data, ThisMarket, ThatMarket)
    test <- mkts[[1]]
    ref <- mkts[[2]]
    dates <- mkts[[3]]
    # If insufficient data or no variance
    if ((var(test)==0 | length(test)<=2*warping_limit+1)){
      isValidTest <- FALSE
      messages <- messages + 1
    }
    # If data and variance are sufficient and test vector was valid
    if (ThisMarket != ThatMarket & isValidTest==TRUE & var(ref)>0 & length(test)>2*warping_limit){
      if (dtw_emphasis>0){
        dist <- dtw(test, ref, window.type=sakoeChibaWindow, window.size=warping_limit)$distance / abs(sum(test))
      } else{
        dist <- 0
      }
      distances[row, "Correlation"] <- cor(test, ref)
      distances[row, "RelativeDistance"] <- dist
      distances[row, "Skip"] <- FALSE
      distances[row, "Length"] <- length(ref)
    } else{
      messages <- messages + 1
      distances[row, "Skip"] <- TRUE
      distances[row, "RelativeDistance"] <- NA
      distances[row, "Correlation"] <- NA
      distances[row, "Length"] <- NA
    }
    row <- row + 1
  }
  
  if(messages > 0){
    cat(paste0(messages, " markets were not matched with ", ThisMarket, " due to insufficient data or no variance."))
  }
  
  distances$matches <- matches
  distances$w <- dtw_emphasis
  distances$MatchingStartDate <- min(data$date_var)
  distances$MatchingEndDate <- max(data$date_var)
  # Filter down to only the top matches
  distances <- dplyr::filter(distances, Skip==FALSE) %>%
    dplyr::mutate(dist_rank=rank(RelativeDistance)) %>%
    dplyr::mutate(corr_rank=rank(-Correlation)) %>%
    dplyr::mutate(combined_rank=w*dist_rank+(1-w)*corr_rank) %>%
    dplyr::arrange(combined_rank) %>%
    dplyr::select(-dist_rank, -Skip, -combined_rank, -corr_rank) %>%
    dplyr::mutate(rank=row_number()) %>%
    dplyr::filter(rank<=matches) %>%
    dplyr::select(-matches, -w)

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

#' @importFrom reshape2 melt dcast
create_market_vectors <- function(data, test_market, ref_market){
  d <- subset(data, !is.na(match_var))
  test <- subset(d, id_var==test_market)[,c("date_var", "match_var")]
  names(test)[2] <- "y"
  if (length(ref_market)==1){
    ref <- subset(d, id_var == ref_market[1])[,c("date_var", "match_var")]
    names(ref)[2] <- "x1"
    f <- dplyr::inner_join(test, ref, by="date_var")
    return(list(as.numeric(f$y), as.numeric(f$x1), as.Date(f$date_var)))
  } else if (length(ref_market)>1){
    ref <- reshape2::dcast(subset(d, id_var %in% ref_market), date_var ~ id_var, value.var="match_var")
    names(ref) <- c("date_var", paste0("x", seq(1:length(ref_market))))
    f <- data.frame(dplyr::inner_join(test, ref, by="date_var"))
    return(list(as.numeric(f$y), dplyr::select(f, num_range("x", 1:length(ref_market))), as.Date(f$date_var)))
  }
}

mape_no_zeros <- function(test, ref){
  d <- cbind.data.frame(test, ref)
  d <- subset(d, abs(test)>0)
  return(mean(abs(lm(test ~ ref, data=d)$residuals)/d$test))
}

dw <- function(y, yhat){
  res <- y - yhat
  lagres <- lagp(res, 1)
  r <- cor(res[2:length(res)], lagres[2:length(lagres)])
  return(2*(1-r))
}


#' For each market, find the best matching control market
#'
#' \code{best_matches} finds the best matching control markets for each market in the dataset
#' using dynamic time warping (\code{dtw} package). The algorithm simply loops through all viable candidates for each
#' market in a parallel fashion, and then ranks by distance and/or correlation.
#'
#' @param data input data.frame for analysis. The dataset should be structured as "stacked" time series (i.e., a panel dataset).
#' In other words, markets are rows and not columns -- we have a unique row for each area/time combination.
#' @param id_variable the name of the variable that identifies the markets
#' @param date_variable the time stamp variable
#' @param matching_variable the variable (metric) used to match the markets. For example, this could be sales or new customers
#' @param parallel set to TRUE for parallel processing. Default is TRUE
#' @param warping_limit the warping limit used for matching. Default is 1, 
#' which means that a single query value can be mapped to at most 2 reference values.
#' @param start_match_period the start date of the matching period (pre period).
#' Must be a character of format "YYYY-MM-DD" -- e.g., "2015-01-01"
#' @param end_match_period the end date of the matching period (pre period).
#' Must be a character of format "YYYY-MM-DD" -- e.g., "2015-10-01"
#' @param matches Number of matching markets to keep in the output
#' @param dtw_emphasis Number from 0 to 1. The amount of emphasis placed on dtw distances, versus correlation, when ranking markets.
#' Default is 1 (all emphasis on dtw). If emphasis is set to 0, all emphasis would be put on correlation.
#' An emphasis of 0.5 would yield equal weighting.
#'
#' @import foreach
#' @importFrom parallel detectCores
#' @importFrom data.table rbindlist
#' @import dplyr
#' @import iterators
#' @import utils
#' @import dtw
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#'
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
#'
#' @usage
#' best_matches(data=NULL,
#'              id_variable=NULL,
#'              date_variable=NULL,
#'              matching_variable=NULL,
#'              warping_limit=1,
#'              parallel=TRUE,
#'              start_match_period=NULL,
#'              end_match_period=NULL,
#'              matches=5)
#'
#' @return Returns an object of type \code{market_matching}. The object has the
#' following elements:
#'
#' \item{\code{BestMatches}}{A data.frame that contains the best matches for each market in the input dataset}
#' \item{\code{Data}}{The raw data used to do the matching}
#' \item{\code{MarketID}}{The name of the market identifier}
#' \item{\code{MatchingMetric}}{The name of the matching variable}
#' \item{\code{DateVariable}}{The name of the date variable}


best_matches <- function(data=NULL, id_variable=NULL, date_variable=NULL, matching_variable=NULL, warping_limit=1, parallel=TRUE, start_match_period=NULL, end_match_period=NULL, matches=5, dtw_emphasis=1){

  ## Check the start date and end dates
  stopif(is.null(start_match_period), TRUE, "No start date provided")
  stopif(is.null(end_match_period), TRUE, "No end date provided")

  # Clean up the emphasis
  if (is.null(dtw_emphasis)){
    dtw_emphasis<-1
  } else if (dtw_emphasis>1){
    dtw_emphasis<-1
  } else if(dtw_emphasis<0){
    dtw_emphasis<-0
  }

  ## check the inputs
  check_inputs(data=data, id=id_variable, matching_variable=matching_variable, date_variable=date_variable)
  data$date_var <- data[[date_variable]]
  data$id_var <- data[[id_variable]]
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
      all_distances[[i]] <- calculate_distances(all_markets, data, id_variable, i, warping_limit, matches, dtw_emphasis)
    }
    shortest_distances <- data.frame(rbindlist(all_distances))
  } else{
    ncore <- detectCores()-1
    registerDoParallel(ncore)
    loop_result <- foreach(i=1:length(all_markets)) %dopar% {
      calculate_distances(all_markets, data, id_variable, i, warping_limit, matches, dtw_emphasis)
    }
    shortest_distances <- data.frame(rbindlist(loop_result))
    stopImplicitCluster()
  }

  ### Return the results
  object <- list(BestMatches=shortest_distances, Data=as.data.frame(saved_data), MarketID=id_variable, MatchingMetric=matching_variable, DateVariable=date_variable)
  class(object) <- "matched_market"
  return (object)
}


#' Given a test market, analyze the impact of an intervention
#'
#' \code{inference} Analyzes the causal impact of an intervention using the CausalImpact package, given a test market and a matched_market object from the best_matches function.
#' The function returns an object of type "market_inference" which contains the estimated impact of the intervention (absolute and relative).
#'
#' @param matched_markets A matched_market object created by the market_matching function
#' @param test_market The name of the test market (character)
#' @param end_post_period The end date of the post period. Must be a character of format "YYYY-MM-DD" -- e.g., "2015-11-01"
#' @param alpha Desired tail-area probability for posterior intervals. For example, 0.05 yields 0.95 intervals
#' @param prior_level_sd Prior SD for the local level term (Gaussian random walk). Default is 0.01. The bigger this number is, the more wiggliness is allowed for the local level term.
#' Note that more wiggly local level terms also translate into larger posterior intervals.
#' @param control_matches Number of matching control markets to use in the analysis

#' @importFrom scales comma
#' @import ggplot2
#' @import zoo

#' @export inference
#' @examples
#' library(MarketMatching)
#' ##-----------------------------------------------------------------------
#' ## Analyze causal impact of a made-up weather intervention in Copenhagen
#' ## Since this is weather data it is a not a very meaningful example. 
#' ## This is merely to demonstrate the function.
#' ##-----------------------------------------------------------------------
#' data(weather, package="MarketMatching")
#' mm <- best_matches(data=weather, id="Area",
#'                    date_variable="Date",
#'                    matching_variable="Mean_TemperatureF",
#'                    parallel=FALSE,
#'                    warping_limit=1, # warping limit=1
#'                    dtw_emphasis=1, # rely only on dtw for pre-screening
#'                    matches=5, # request 5 matches
#'                    start_match_period="2014-01-01",
#'                    end_match_period="2014-10-01")
#' library(CausalImpact)
#' results <- inference(matched_markets=mm,
#'                      test_market="CPH",
#'                      end_post_period="2015-12-15",
#'                      prior_level_sd=0.002)
#' @usage
#' inference(matched_markets=NULL,
#'           test_market=NULL,
#'           end_post_period=NULL,
#'           alpha=0.05,
#'           prior_level_sd=0.01,
#'           control_matches=5)
#'
#' @return Returns an object of type \code{inference}. The object has the
#' following elements:
#' \item{\code{AbsoluteEffect}}{The estimated absolute effect of the intervention}
#' \item{\code{AbsoluteEffectLower}}{The lower limit of the estimated absolute effect of the intervention.
#' This is based on the posterior interval of the counterfactual predictions.
#' The width of the interval is determined by the \code{alpha} parameter.}
#' \item{\code{AbsoluteEffectUpper}}{The upper limit of the estimated absolute effect of the intervention.
#' This is based on the posterior interval of the counterfactual predictions.
#' The width of the interval is determined by the \code{alpha} parameter.}
#' \item{\code{RelativeEffectLower}}{Same as the above, just for relative (percentage) effects}
#' \item{\code{RelativeEffectUpper}}{Same as the above, just for relative (percentage) effects}
#' \item{\code{TailProb}}{Posterior probability of a non-zero effect}
#' \item{\code{PrePeriodMAPE}}{Pre-intervention period MAPE}
#' \item{\code{DW}}{Durbin-Watson statistic. Should be close to 2.}
#' \item{\code{PlotActualVersusExpected}}{Plot of actual versus expected using \code{ggplot2}}
#' \item{\code{PlotCumulativeEffect}}{Plot of the cumulative effect using \code{ggplot2}}
#' \item{\code{PlotPointEffect}}{Plot of the pointwise effect using \code{ggplot2}}
#' \item{\code{PlotActuals}}{Plot of the actual values for the test and control markets using \code{ggplot2}}
#' \item{\code{PlotPriorLevelSdAnalysis}}{Plot of DW and MAPE for different values of the local level SE using \code{ggplot2}}
#' \item{\code{PlotLocalLevel}}{Plot of the local level term using \code{ggplot2}}
#' \item{\code{TestData}}{A \code{data.frame} with the test market data}
#' \item{\code{TestData}}{A \code{data.frame} with the data for the control markets}
#' \item{\code{PlotResiduals}}{Plot of the residuals using \code{ggplot2}}
#' \item{\code{TestName}}{The name of the test market}
#' \item{\code{TestName}}{The name of the control market}
#' \item{\code{zooData}}{A \code{zoo} time series object with the test and control data}
#' \item{\code{Predictions}}{Actual versus predicted values}
#' \item{\code{CausalImpactObject}}{The CausalImpact object created}
#' \item{\code{Coefficients}}{The average posterior coefficients}

inference <- function(matched_markets=NULL, test_market=NULL, end_post_period=NULL, alpha=0.05, prior_level_sd=0.01, control_matches=5){

  ## copy the distances
  mm <- dplyr::filter(matched_markets$BestMatches, rank<=control_matches)

  data <- matched_markets$Data
  mm$id_var <- mm[[names(mm)[1]]]
  mm <- dplyr::arrange(mm, id_var, BestControl)

  ## check if the test market exists
  stopif(test_market %in% unique(data$id_var), FALSE, paste0("test market ", test_market, " does not exist"))

  ## if an end date has not been provided, then choose the max of the data
  if (is.null(end_post_period)){
    end_post_period <- as.Date(max(subset(data, id_var==test_market)$date_var))
  }

  # filter for dates
  MatchingStartDate <- as.Date(subset(mm, id_var==test_market)$MatchingStartDate[1])
  MatchingEndDate <- as.Date(subset(mm, id_var==test_market)$MatchingEndDate[1])
  data <- dplyr::filter(data, date_var>=MatchingStartDate & date_var<=as.Date(end_post_period))

  ## get the control market name
  control_market <- subset(mm, id_var==test_market)$BestControl

  ## get the test and ref markets
  mkts <- create_market_vectors(data, test_market, control_market)
  y <- mkts[[1]]
  ref <- mkts[[2]]
  date <- mkts[[3]]
  end_post_period <- max(date)
  post_period <- date[date > as.Date(mm[1, "MatchingEndDate"])]
  stopif(length(post_period)==0, TRUE, "ERROR: no valid data in the post period")
  post_period_start_date <- min(post_period)
  post_period_end_date <- max(post_period)
  ts <- zoo(cbind.data.frame(y, ref), date)

  ## print the settings
  cat("\t------------- Inputs -------------\n")
  cat(paste0("\tTest Market: ", test_market, "\n"))
  for (i in 1:length(control_market)){
    cat(paste0("\tControl Market ", i, ": ", control_market[i], "\n"))
  }
  cat(paste0("\tMarket ID: ", matched_markets$MarketID, "\n"))
  cat(paste0("\tDate Variable: ", matched_markets$DateVariable, "\n"))
  cat(paste0("\tMatching (pre) Period Start Date: ", MatchingStartDate, "\n"))
  cat(paste0("\tMatching (pre) Period End Date: ", MatchingEndDate, "\n"))
  cat(paste0("\tPost Period Start Date: ", post_period_start_date, "\n"))
  cat(paste0("\tPost Period End Date: ", post_period_end_date, "\n"))
  cat(paste0("\tMatching Metric: ", matched_markets$MatchingMetric, "\n"))
  cat(paste0("\tLocal Level Prior SD: ", prior_level_sd, "\n"))
  cat(paste0("\tPosterior Intervals Tail Area: ", 100*(1-alpha), "%\n"))
  cat("\n")
  cat("\n")

  ## run the inference
  pre.period <- c(as.Date(MatchingStartDate), as.Date(MatchingEndDate))
  post.period <- c(as.Date(post_period_start_date), as.Date(post_period_end_date))
  set.seed(2015)
  impact <- CausalImpact(ts, pre.period, post.period, alpha=alpha, model.args=list(prior.level.sd=prior_level_sd))

  ## estimate betas for different values of prior sd
  betas <- data.frame(matrix(nrow=11, ncol=4))
  names(betas) <- c("SD", "SumBeta", "DW", "MAPE")
  for (i in 0:20){
    step <- (max(0.1, prior_level_sd) - min(0.001, prior_level_sd))/20
    sd <- min(0.001, prior_level_sd) + step*i
    m <- CausalImpact(ts, pre.period, post.period, alpha=alpha, model.args=list(prior.level.sd=sd))
    burn <- SuggestBurn(0.1, m$model$bsts.model)
    b <- sum(apply(m$model$bsts.model$coefficients[-(1:burn),], 2, CMean))
    betas[i+1, "SD"] <- sd
    betas[i+1, "SumBeta"] <- b
    preperiod <- subset(m$series, cum.effect == 0)
    betas[i+1, "DW"] <- dw(preperiod$response, preperiod$point.pred)
    betas[i+1, "MAPE"] <- mape_no_zeros(preperiod$response, preperiod$point.pred)
  }

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
  preperiod$res <- preperiod$response - preperiod$point.pred
  results[[8]] <- mape_no_zeros(preperiod$response, preperiod$point.pred)
  results[[9]] <- dw(preperiod$response, preperiod$point.pred)

  cat("\t------------- Model Stats -------------\n")
  cat(paste0("\tMatching (pre) Period MAPE: ", round(100*results[[8]], 2) , "%\n"))
  avg_coeffs <- data.frame(nrow=dim(impact$model$bsts.model$coefficients)[2]-1, ncol=2)
  names(avg_coeffs) <- c("Market", "AverageBeta")
  for (i in 2:dim(impact$model$bsts.model$coefficients)[2]){
    avg_coeffs[i-1, "Market"] <- control_market[i-1]
    avg_coeffs[i-1, "AverageBeta"] <- apply(impact$model$bsts.model$coefficients[-(1:burn),], 2, CMean)[i]
    cat(paste0("\tBeta ", i-1, " [", control_market[i-1], "]: ", round(avg_coeffs[i-1, "AverageBeta"], 4) , "\n"))
  }
  cat(paste0("\tDW: ", round(results[[9]], 2) , "\n"))
  cat("\n")
  cat("\n")

  ymin <- min(min(impact$series$response), min(impact$series$point.pred.lower), min(ref), min(y))
  ymax <- max(max(impact$series$response), max(impact$series$point.pred.upper), max(ref), max(y))

  ## create actual versus predicted plots
  avp <- cbind.data.frame(date, data.frame(impact$series)[,c("response", "point.pred", "point.pred.lower", "point.pred.upper")])
  names(avp) <- c("Date", "Response", "Predicted", "lower_bound", "upper_bound")
  avp$test_market <- test_market
  results[[10]] <- ggplot(data=avp, aes(x=Date)) +
    geom_line(aes(y=Response, colour = "Observed"), size=1.2) +
    geom_ribbon(aes(ymin=lower_bound, ymax=upper_bound), fill="grey", alpha=0.3) +
    geom_line(aes(y=Predicted, colour = "Expected"), size=1.2) +
    theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
    geom_vline(xintercept=as.numeric(MatchingEndDate), linetype=2) +
    scale_y_continuous(labels = scales::comma, limits=c(ymin, ymax)) +
    ggtitle(paste0("Test Market: ",test_market))
  avp$test_market <- NULL

  ## create cumulative lift plots
  plotdf <- cbind.data.frame(as.Date(row.names(data.frame(impact$series))), data.frame(impact$series)[,c("cum.effect", "cum.effect.lower", "cum.effect.upper")])
  names(plotdf) <- c("Date", "Cumulative", "lower_bound", "upper_bound")
  results[[11]] <- ggplot(data=plotdf, aes(x=Date, y=Cumulative)) + geom_line(size=1.2) + theme_bw() +
    scale_y_continuous(labels = scales::comma) + ylab("Cumulative Effect") + xlab("") +
    geom_vline(xintercept=as.numeric(MatchingEndDate), linetype=2) +
    geom_ribbon(aes(ymin=lower_bound, ymax=upper_bound), fill="grey", alpha=0.3)

  ## create plots of the actual data
  plotdf <- data[data$id_var %in% c(test_market, control_market),]
  results[[12]] <- ggplot(data=plotdf, aes(x=date_var, y=match_var, colour=id_var)) +
    geom_line() +
    theme_bw() + theme(legend.title = element_blank(), axis.title.x = element_blank()) + ylab("") + xlab("Date") +
    geom_vline(xintercept=as.numeric(MatchingEndDate), linetype=2) +
    scale_y_continuous(labels = scales::comma, limits=c(ymin, ymax))

  ## plot betas at various local level SDs
  results[[13]] <- ggplot(data=betas, aes(x=SD, y=Beta)) +
    geom_line() +
    theme_bw() + theme(legend.title = element_blank()) +
    geom_vline(xintercept=as.numeric(prior_level_sd), linetype=2) + xlab("Local Level Prior SD")

  ## plot DWs and MAPEs at different SDs
  plotdf <- melt(data=betas, id="SD")
  results[[14]] <- ggplot(data=plotdf, aes(x=SD, y=value, colour=variable)) + geom_line() +
    theme_bw() + theme(legend.title = element_blank()) +
    geom_vline(xintercept=as.numeric(prior_level_sd), linetype=2) + xlab("Local Level Prior SD") +
    facet_grid(variable ~ ., scales="free") + ylab("") + guides(colour=FALSE)

  burn <- SuggestBurn(0.1, impact$model$bsts.model)
  plotdf <- cbind.data.frame(date, colMeans(impact$model$bsts.model$state.contributions[-(1:burn), "trend", ])) %>% filter(date<=as.Date(MatchingEndDate))
  names(plotdf) <- c("Date", "LocalLevel")
  results[[15]] <- ggplot(data=plotdf, aes(x=Date, y=LocalLevel)) +
    geom_line() +
    theme_bw() + theme(legend.title = element_blank()) +
    ylab("Local Level") + xlab("")

  plotdf <- cbind.data.frame(date, data.frame(impact$series)[,c("response", "point.pred")]) %>% dplyr::filter(date<=as.Date(MatchingEndDate))
  names(plotdf) <- c("Date", "y", "yhat")
  plotdf$Residuals <- plotdf$y - plotdf$yhat
  results[[16]] <- ggplot(data=plotdf, aes(x=Date, y=Residuals)) +
    geom_line() +
    theme_bw() + theme(legend.title = element_blank()) +
    xlab("")

  ## create cumulative lift plots
  plotdf <- cbind.data.frame(as.Date(row.names(data.frame(impact$series))), data.frame(impact$series)[,c("point.effect", "point.effect.lower", "point.effect.upper")])
  names(plotdf) <- c("Date", "Pointwise", "lower_bound", "upper_bound")
  results[[17]] <- ggplot(data=plotdf, aes(x=Date, y=Pointwise)) + geom_line(size=1.2) + theme_bw() +
    scale_y_continuous(labels = scales::comma) + ylab("Point Effect") + xlab("") +
    geom_vline(xintercept=as.numeric(MatchingEndDate), linetype=2) +
    geom_ribbon(aes(ymin=lower_bound, ymax=upper_bound), fill="grey", alpha=0.3)
  
  ### print results
  cat("\t------------- Effect Analysis -------------\n")
  cat(paste0("\tAbsolute Effect: ", round(results[[1]],2), " [", round(results[[2]],2), ", ", round(results[[3]],2), "]\n"))
  cat(paste0("\tRelative Effect: ", paste0(round(100*results[[4]],2), "%"), " [", paste0(round(100*results[[5]],2), "%"), ", ", paste0(round(100*results[[6]],2), "%"), "]\n"))
  cat(paste0("\tProbability of a causal impact: ", paste0(round(100*(1-results[[7]]),4), "%\n")))

  ### return the results
  object <- list(AbsoluteEffect=results[[1]], AbsoluteEffectLower=results[[2]], AbsoluteEffectUpper=results[[3]],
                 RelativeEffect=results[[4]], RelativeEffectLower=results[[5]], RelativeEffectUpper=results[[6]],
                 TailProb=results[[7]], PrePeriodMAPE=results[[8]], DW=results[[9]],
                 PlotActualVersusExpected=results[[10]], PlotCumulativeEffect=results[[11]], PlotPointEffect=results[[17]],
                 PlotActuals=results[[12]], PlotPriorLevelSdAnalysis=results[[14]],
                 PlotLocalLevel=results[[15]], TestData=y, ControlData=ref, PlotResiduals=results[[16]],
                 TestName=test_market, ControlName=control_market, ZooData=ts, Predictions=avp,
                 CausalImpactObject=impact, Coefficients=avg_coeffs)
  class(object) <- "matched_market_inference"
  return (object)
}
