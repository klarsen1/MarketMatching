lagp <- function(x, p){
  return(c(rep(0,p), x[1:(length(x)-p)]))
}

CMean <- function(b) {
  b <- b[b != 0]
  if (length(b) > 0) 
    return(mean(b))
  return(0)
}

logplus <- function(x){
  sapply(x, function(x) {if (x<=0){
    return(0.00)
  } else{
    return(log(x))
  }})
}

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#' @importFrom stats cor var na.omit
#' @importFrom tidyr replace_na

calculate_distances <- function(markets_to_be_matched, data, id, i, warping_limit, matches, dtw_emphasis){
  ## Nulling to avoid CRAN notes
  Skip <- NULL
  RelativeDistance <- NULL
  Correlation <- NULL
  w <- NULL
  dist_rank <- NULL
  corr_rank <- NULL
  combined_rank <- NULL
  messages <- NULL
  NORMDIST <- NULL 
  RAWDIST <- NULL 
  populated <- NULL
  SUMTEST <- NULL
  SUMCNTL <- NULL
  
  
  if (dtw_emphasis==0){
    warping_limit <- 0
  }

  row <- 1
  ThisMarket <- markets_to_be_matched[i]
  distances <- data.frame(matrix(nrow=length(data$id_var)-1, ncol=10))
  names(distances) <- c(id, "BestControl", "RelativeDistance", "Correlation", "Length", "SUMTEST", "SUMCNTL", "RAWDIST", "Correlation_of_logs", "populated")
  distances[ ,"populated"] <- 0
  messages <- 0
  # For each market
  for (j in 1:length(unique(data$id_var))){
    isValidTest <- TRUE
    ThatMarket <- unique(data$id_var)[j]
    distances[row, id] <- ThisMarket
    distances[row, "BestControl"] <- ThatMarket
    mkts <- create_market_vectors(data, ThisMarket, ThatMarket)
    test <- mkts[[1]]
    ref <- mkts[[2]]
    dates <- mkts[[3]]
    sum_test <- NA
    sum_cntl <- NA
    dist <- 0
    # If insufficient data or no variance
    buffer <- 0.5
    if (dtw_emphasis>0){
      buffer <- warping_limit
    }
    if ((stats::var(test)==0 | length(test)<=2*buffer+1) | sum(abs(test))==0){
      isValidTest <- FALSE
      messages <- messages + 1
    }
    # If data and variance are sufficient and test vector was valid
    if (ThisMarket != ThatMarket & isValidTest==TRUE & var(ref)>0 & length(test)>2*warping_limit+1){
      sum_test <- abs(sum(test))
      sum_cntl <- abs(sum(ref))
      if (dtw_emphasis>0 & sum_test>0){
        rawdist <- dtw(test, ref, window.type=sakoeChibaWindow, window.size=warping_limit)$distance 
        dist <- rawdist / sum_test
      } else if (dtw_emphasis==0){
        dist <- 0
        rawdist <- 0
      } else{
        dist <- -1000000000
        rawdist <- -1000000000
      }
      distances[row, "Correlation"] <- cor(test, ref)
      distances[row, "populated"] <- 1
      distances[row, "RelativeDistance"] <- dist
      distances[row, "Skip"] <- FALSE
      distances[row, "Length"] <- length(test)
      distances[row, "SUMTEST"] <- sum_test
      distances[row, "SUMCNTL"] <- sum_cntl
      distances[row, "RAWDIST"] <- rawdist
      if (max(ref)>0 & max(test)>0 & sd(logplus(test))>0 & sd(logplus(ref))>0){
         distances[row, "Correlation_of_logs"] <- cor(logplus(test), logplus(ref))
      } else{
         distances[row, "Correlation_of_logs"] <- -1000000000
      }
      row <- row + 1
    } else{
      if (ThisMarket != ThatMarket){
         messages <- messages + 1
         distances[row, "Skip"] <- TRUE
         if (dtw_emphasis==0){
            distances[row, "RelativeDistance"] <- 0
            distances[row, "RAWDIST"] <- 0
         } else{
           distances[row, "RelativeDistance"] <- -1000000000
           distances[row, "RAWDIST"] <- -1000000000
         }
         distances[row, "populated"] <- 1
         distances[row, "Correlation"] <- -1000000000
         distances[row, "Length"] <- 0
         distances[row, "SUMTEST"] <- 0
         distances[row, "SUMCNTL"] <- 0
         distances[row, "Correlation_of_logs"] <- -1000000000
         row <- row + 1
      }
    }
  }
  
  if(messages > 0){
    cat(paste0(messages, " markets were not matched with ", ThisMarket, " due to insufficient data or no variance. \n"))
    if (dtw_emphasis>0){
      cat(paste0("Since dtw_emphasis>0, more than 2*warping_limit+1 records are required in the matching period to match a market. \n"))
    } else{
      cat(paste0("Note that More than 2 records are required in the matching period to match a market. \n"))
    }
    cat("\n")
  }
  
  distances$matches <- matches
  distances$w <- dtw_emphasis
  distances$MatchingStartDate <- min(data$date_var)
  distances$MatchingEndDate <- max(data$date_var)
  
  # Filter down to only the top matches
  distances <- 
    dplyr::filter(distances, populated==1) %>%
    dplyr::mutate(dist_rank=rank(RelativeDistance)) %>%
    dplyr::mutate(corr_rank=rank(-Correlation)) %>%
    dplyr::mutate(combined_rank=w*dist_rank+(1-w)*corr_rank) %>%
    dplyr::arrange(combined_rank) %>%
    dplyr::select(-dist_rank, -combined_rank, -corr_rank, -populated) %>%
    dplyr::mutate(rank=row_number()) %>%
    dplyr::filter(rank<=matches) %>%
    dplyr::select(-matches, -w) %>%
    dplyr::mutate(NORMDIST=dplyr::if_else(SUMTEST+SUMCNTL>0 & !(RAWDIST %in% c(-1000000000, 0)), 2*RAWDIST/(SUMTEST+SUMCNTL), -1000000000)) %>%
    dplyr::mutate(NORMDIST=dplyr::na_if(NORMDIST, -1000000000),  
                  NORMDIST=dplyr::na_if(NORMDIST, 0),  
                  RAWDIST=dplyr::na_if(RAWDIST, -1000000000), 
                  RAWDIST=dplyr::na_if(RAWDIST, 0))
  
  if (dtw_emphasis==0 & nrow(distances)>0){
    distances$RelativeDistance <- NA
  }

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
  stopif(length(unique(data[[id]]))>1, FALSE, "ERROR: Need at least 2 unique markets")
  stopif(TRUE %in% is.na(data[[id]]), TRUE, "ERROR: NAs found in the market column")
  stopif(TRUE %in% is.null(data[[id]]), TRUE, "ERROR: NULLs found in the market column")
  stopif('' %in% unique(data[[id]]), TRUE, "ERROR: Blanks found in the market column")
  stopif(TRUE %in% is.na(data[[matching_variable]]), TRUE, "ERROR: NAs found in the matching variable")
  stopif(class(data[[date_variable]]) == "Date", FALSE, "ERROR: date_variable is not a Date. Check your data frame or use as.Date().")
}

#' @importFrom reshape2 melt dcast
create_market_vectors <- function(data, test_market, ref_market){
  ## nulling to avoid CRAN notes
  id_var <- NULL
  date_var <- NULL
  match_var <- NULL
  
  d <- subset(data, !is.na(match_var))
  test <- subset(d, id_var==test_market)[,c("date_var", "match_var")]
  names(test)[2] <- "y"
  if (length(ref_market)==1){
    ref <- subset(d, id_var == ref_market[1])[,c("date_var", "match_var")]
    names(ref)[2] <- "x1"
    f <- dplyr::inner_join(test, ref, by="date_var")
    return(list(as.numeric(f$y), as.numeric(f$x1), as.Date(f$date_var)))
  } else if (length(ref_market)>1){
    d <- dplyr::distinct(d, id_var, date_var, .keep_all = TRUE)
    ref <- reshape2::dcast(subset(d, id_var %in% ref_market), date_var ~ id_var, value.var="match_var")
    names(ref) <- c("date_var", paste0("x", seq(1:length(ref_market))))
    f <- data.frame(dplyr::inner_join(test, ref, by="date_var"))
    f <- stats::na.omit(f)
    return(list(as.numeric(f$y), dplyr::select(f, num_range("x", 1:length(ref_market))), as.Date(f$date_var)))
  }
}

mape_no_zeros <- function(test, ref){
  d <- subset(data.frame(cbind(test, ref)), test>0)
  return(mean(abs(d$test - d$ref)/d$test))
}

dw <- function(y, yhat){
  res <- y - yhat
  lagres <- lagp(res, 1)
  r <- stats::cor(res[2:length(res)], lagres[2:length(lagres)])
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
#' @param markets_to_be_matched Use this parameter if you only want to get control matches for a subset of markets or a single market
#' The default is NULL which means that all markets will be paired with matching markets
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
#' @param matches Number of matching markets to keep in the output (to use less markets for inference, use the control_matches parameter when calling inference). Default is to keep all matches.
#' @param dtw_emphasis Number from 0 to 1. The amount of emphasis placed on dtw distances, versus correlation, when ranking markets.
#' Default is 1 (all emphasis on dtw). If emphasis is set to 0, all emphasis would be put on correlation, which is recommended when optimal splits are requested.
#' An emphasis of 0.5 would yield equal weighting.
#' @param suggest_market_splits if set to TRUE, best_matches will return suggested test/control splits based on correlation and market sizes. Default is FALSE.
#' For this option to be invoked, markets_to_be_matched must be NULL (i.e., you must run a full match).
#' Note that the algorithm will force test and control to have the same number of markets. So if the total number of markets is odd, one market will be left out.
#' @param splitbins Number of size-based bins used to stratify when splitting markets into test and control.
#' Only markets inside the same bin can be matched. More bins means more emphasis on market size when splitting.
#' Less bins means more emphasis on correlation. Default is 10.
#' @param log_for_splitting This parameter determines if optimal splitting is based on correlations of the raw 
#' matching metric values or the correlations of log(matching metric). Only relevant if suggest_market_splits is TRUE. Default is FALSE.
#' @import foreach
#' @importFrom parallel detectCores
#' @import CausalImpact
#' @import dplyr
#' @import iterators
#' @import utils
#' @import dtw
#' @import utf8
#' @importFrom reshape2 melt
#' @importFrom stats sd
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#'
#' @export best_matches
#' @examples
#' \dontrun{
#' ##-----------------------------------------------------------------------
#' ## Find the best matches for the CPH airport time series
#' ##-----------------------------------------------------------------------
#' library(MarketMatching)
#' data(weather, package="MarketMatching")
#' mm <- best_matches(data=weather, 
#'                    id="Area",
#'                    markets_to_be_matched=c("CPH", "SFO"),
#'                    date_variable="Date",
#'                    matching_variable="Mean_TemperatureF",
#'                    parallel=FALSE,
#'                    start_match_period="2014-01-01",
#'                    end_match_period="2014-10-01")
#' head(mm$BestMatches)
#' }
#'
#' @usage
#' best_matches(data=NULL,
#'              markets_to_be_matched=NULL,
#'              id_variable=NULL,
#'              date_variable=NULL,
#'              matching_variable=NULL,
#'              parallel=TRUE,
#'              warping_limit=1,
#'              start_match_period=NULL,
#'              end_match_period=NULL,
#'              matches=NULL,
#'              dtw_emphasis=1, 
#'              suggest_market_splits=FALSE,
#'              splitbins=10,
#'              log_for_splitting=FALSE)
#'
#' @return Returns an object of type \code{market_matching}. The object has the
#' following elements:
#'
#' \item{\code{BestMatches}}{A data.frame that contains the best matches for each market. All stats reflect data after the market pairs have been joined by date. Thus SUMTEST and SUMCNTL can have smaller values than what you see in the Bins output table}
#' \item{\code{Data}}{The raw data used to do the matching}
#' \item{\code{MarketID}}{The name of the market identifier}
#' \item{\code{MatchingMetric}}{The name of the matching variable}
#' \item{\code{DateVariable}}{The name of the date variable}
#' \item{\code{SuggestedTestControlSplits}}{Suggested test/control splits. SUMTEST and SUMCNTL are the total market volumes, not volume after joining with other markets. They're greater or equal to the values in the BestMatches file.}
#' \item{\code{Bins}}{Bins used for splitting and corresponding volumes}

best_matches <- function(data=NULL, markets_to_be_matched=NULL, id_variable=NULL, date_variable=NULL, matching_variable=NULL, parallel=TRUE, warping_limit=1, start_match_period=NULL, end_match_period=NULL, matches=NULL, dtw_emphasis=1, suggest_market_splits=FALSE, splitbins=10, log_for_splitting=FALSE){

  ## Nulling to avoid angry notes
  match_var <- NULL
  id_var <- NULL
  date_var <- NULL
  suggested_split <- NULL
  Sizes <-NULL
  BestControl <- NULL 
  Corr <- NULL 
  Correlation <- NULL 
  Correlation_of_logs <- NULL 
  SUMCNTL <- NULL 
  SUMTEST <- NULL 
  Segment <- NULL 
  Skip <- NULL 
  Volume <- NULL 
  control_market <- NULL
  market <- NULL 
  max_rows <- NULL 
  rows <- NULL 
  short <- NULL 
  test_market <- NULL
  
  ## Check the start date and end dates
  stopif(is.null(start_match_period), TRUE, "No start date provided")
  stopif(is.null(end_match_period), TRUE, "No end date provided")

  # Clean up the emphasis
  if (is.null(dtw_emphasis)){
    dtw_emphasis<-0
  } else if (dtw_emphasis>1){
    dtw_emphasis<-1
  } else if(dtw_emphasis<0){
    dtw_emphasis<-0
  }
  
  ## check the inputs
  stopif(date_variable %in% names(data), FALSE, "ERROR: date variable not found in input data")
  if (length(class(data[[date_variable]]))>1){
    if (!("Date" %in% class(data[[date_variable]]))){
      cat("NOTE: Date variable converted to Date using as.Date() \n")
      cat("\n")
    }
  } else if (class(data[[date_variable]]) != "Date"){
    cat("NOTE: Date variable converted to Date using as.Date() \n")
    cat("\n")
  }
    
  data[[date_variable]] <- as.Date(data[[date_variable]]) ## trim the date variable
  check_inputs(data=data, id=id_variable, matching_variable=matching_variable, date_variable=date_variable)
  data$date_var <- data[[date_variable]] 
  data$id_var <- data[[id_variable]]
  data$match_var <- data[[matching_variable]]
  
  if (is.null(markets_to_be_matched)==FALSE & suggest_market_splits==TRUE){
    cat("The suggest_market_splits parameter has been turned off since markets_to_be_matched is not NULL \n")
    cat("Set markets_to_be_matched to NULL if you want optimized pairs \n")
    cat("\n")
  } 

  if (is.null(matches)){
    if (is.null(markets_to_be_matched) & suggest_market_splits==TRUE){
      matches <- length(unique(data$id_var))
    } else{
      matches <- 5
    }
  } else{
    if (is.null(markets_to_be_matched) & suggest_market_splits==TRUE){
      matches <- length(unique(data$id_var))
      cat("The matches parameter has been overwritten for splitting to conduct a full search for optimized pairs \n")
      cat("\n")
    }  
  }
  
  ## check for dups
  ddup <- dplyr::distinct(data, id_var, date_var)
  stopif(nrow(ddup)<nrow(data), TRUE, "ERROR: There are date/market duplicates in the input data")
  rm(ddup)

  ## reduce the width of the data.frame
  data <- dplyr::arrange(data, id_var, date_var) %>% 
    ungroup() %>% 
    dplyr::select(id_var, date_var, match_var)

  ## save a reduced version of the data
  saved_data <- data
  
  ## set up a list to hold all distance matrices
  all_distances <- list()

  ## filter the dates
  data <- 
    dplyr::filter(data, date_var>=as.Date(start_match_period) & date_var<=as.Date(end_match_period)) %>%
    dplyr::group_by(id_var) %>%
    dplyr::mutate(rows=max(row_number())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(max_rows=max(rows)) %>%
    dplyr::mutate(short=(rows<max_rows)) %>%
    dplyr::select(-rows, -max_rows, -short) 
  
  ## check if any data is left
  stopif(nrow(data)>0, FALSE, "ERROR: no data left after filter for dates")

  ## get a vector of all markets that matches are wanted for. Check to ensure markets_to_be_matched exists in the data.
  if(is.null(markets_to_be_matched)){
    markets_to_be_matched <- unique(data$id_var)
  }else{
    markets_to_be_matched <- unique(markets_to_be_matched)
    for (k in 1:length(markets_to_be_matched)){
      stopif(markets_to_be_matched[k] %in% unique(data$id_var), FALSE, paste0("test market ", markets_to_be_matched[k], " does not exist"))
    }
  }
  
  ## loop through markets and compute distances
  if (parallel == FALSE) {
    for (i in 1:length(markets_to_be_matched)) {
        all_distances[[i]] <- calculate_distances(markets_to_be_matched, data, id_variable, i, warping_limit, matches, dtw_emphasis)
    }
    shortest_distances <- data.frame(dplyr::bind_rows(all_distances))
  }else{
    ncore <- detectCores() - 1
    if (ncore>length(markets_to_be_matched)){
       ncore <- length(markets_to_be_matched)  
    }
    registerDoParallel(ncore)
    loop_result <- foreach(i = 1:length(markets_to_be_matched)) %dopar% 
        {
            calculate_distances(markets_to_be_matched, data, id_variable, i, warping_limit, matches, dtw_emphasis)
        }
    shortest_distances <- data.frame(dplyr::bind_rows(loop_result))
    stopImplicitCluster()
  }
  
  if (suggest_market_splits==TRUE){
    
    sizes <- shortest_distances
    
    sizes$market <- sizes[[id_variable]]
    markets <- length(unique(sizes$market))
    maxbins <- floor(markets/2)
    bins <- maxbins
    if (maxbins>splitbins){
      bins <- splitbins
      if (bins==0){bins <- 1}
    } else if (maxbins==0){
      bins <- 1
    } else if (splitbins>maxbins){
      bins <- maxbins
    } else if (splitbins==0){
      bins <- 1
    }
    
    bin_size <- floor(markets/bins)

    cat(paste0("\tOptimal test/control market splits will be generated, targeting ", bin_size, " markets in each bin. \n"))
    if (dtw_emphasis>0){
      cat("\n")
      cat("\tFYI: It is recommended to set dtw_emphasis to 0 when running optimal splits. \n")
      cat("\n")
    }

    if (bin_size %% 2 != 0){
      bin_size <- bin_size-1
      if (bin_size<1){
        bin_size <- 1
      }
    }
    
    ## True volume before joining with other markets
    true_volumes <- dplyr::group_by(data, id_var) %>%
      dplyr::summarise(Volume=sum(match_var)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(market=id_var)
    
    sizes <- dplyr::select(sizes, market) %>%
      dplyr::distinct(market, .keep_all=TRUE) %>%
      dplyr::left_join(true_volumes, by="market") %>%
      dplyr::arrange(-Volume) %>%
      dplyr::mutate(
        bin=floor((dplyr::row_number()-0.1)/bin_size)+1) %>%
      dplyr::select(market, bin, Volume) %>%
      dplyr::mutate(test_market=market, 
                    control_market=market) %>%
      dplyr::group_split(bin)
    optimal_list <- list()
    j <- 1
    for (i in 1:length(sizes)){
      bin <- dplyr::pull(sizes[[i]], market)
      tdf <- shortest_distances
      tdf$test_market <- tdf[[id_variable]]
      if (log_for_splitting==TRUE){
         tdf$Corr <- tdf$Correlation_of_logs
      } else{
        tdf$Corr <- tdf$Correlation
      }
      tdf <- dplyr::filter(tdf, test_market %in% bin & BestControl %in%  bin) %>%
        dplyr::arrange(-Corr) %>%
        dplyr::mutate(
               control_market=BestControl, 
               Segment=i)
      rowsleft <- nrow(tdf)
      while(rowsleft>0){
        optimal_list[[j]] <- tdf[1,c("Segment", "test_market", "control_market", "Correlation_of_logs", "Correlation", "SUMTEST", "SUMCNTL", "Corr")]
        test <- tdf[1,"test_market"]
        cntl <- tdf[1,"control_market"]
        tdf <- dplyr::filter(tdf, !(test_market %in% c(test, cntl)))
        tdf <- dplyr::filter(tdf, !(control_market %in% c(test, cntl))) %>%
          dplyr::arrange(-Corr)
        rowsleft <- nrow(tdf)
        j <- j+1
      }
    }
   Sizes <- dplyr::bind_rows(sizes)
   
   suggested_split <- dplyr::bind_rows(optimal_list) %>%
        dplyr::select(-SUMTEST, -SUMCNTL) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(dplyr::select(Sizes, Volume, test_market), by="test_market") %>%
        dplyr::rename(SUMTEST=Volume) %>%
        dplyr::left_join(dplyr::select(Sizes, Volume, control_market), by="control_market") %>%
        dplyr::rename(SUMCNTL=Volume) %>%
        dplyr::arrange(Segment, -Corr) %>%
        dplyr::mutate(PairRank=row_number()) %>%
        dplyr::mutate(Volume=SUMTEST+SUMCNTL, 
                      percent_of_volume=cumsum(Volume)/sum(Volume)) %>%
       dplyr::select(-Corr) %>%
     dplyr::group_by(Segment) %>%
     dplyr::mutate(markets=n()*2) %>%
     dplyr::ungroup() %>%
     dplyr::mutate(Correlation_of_logs=dplyr::na_if(Correlation_of_logs, -1000000000),
                   Correlation=dplyr::na_if(Correlation, -1000000000))
     
   Sizes <- dplyr::select(Sizes, market, bin, Volume) %>%
     dplyr::group_by(bin) %>%
     dplyr::mutate(markets_in_bin=n()) %>%
     dplyr::ungroup() %>%
     dplyr::mutate(excluded_from_optimal_splits=dplyr::if_else(market %in% c(suggested_split$test_market,suggested_split$control_market), 0, 1))
   
   if (nrow(Sizes)-nrow(suggested_split)*2>0){
     cat("\t", paste0(nrow(Sizes)-nrow(suggested_split)*2, " market(s) were excluded from the splits due the total number of markets being odd \n"))
     cat("\t Check the Bins output file to identify the market(s) that were excluded \n")
     cat("\n")
   }
    
  } else{
    suggested_split <- NULL
    Sizes <- NULL
  }
  
  ### Return the results
  shortest_distances <- 
    dplyr::filter(shortest_distances, Skip==FALSE) %>%
    dplyr::select(-Skip)
  
  object <- list(BestMatches=shortest_distances, Data=as.data.frame(saved_data), MarketID=id_variable, MatchingMetric=matching_variable, DateVariable=date_variable, SuggestedTestControlSplits=suggested_split, Bins=Sizes)
  class(object) <- "matched_market"
  return (object)
}

#' Given a test market, analyze the impact of an intervention
#'
#' \code{inference} Analyzes the causal impact of an intervention using the CausalImpact package, given a test market and a matched_market object from the best_matches function.
#' The function returns an object of type "market_inference" which contains the estimated impact of the intervention (absolute and relative).
#'
#' @param matched_markets A matched_market object created by the market_matching function
#' @param bsts_modelargs A list() that passes model parameters directly to bsts -- such as list(niter = 1000, nseasons = 52, prior.level.sd=0.1)
#' This parameter will overwrite the values specified in prior_level_sd and nseasons. ONLY use this if you're using intricate bsts settings
#' For most use-cases, using the prior_level_sd and nseasons parameters should be sufficient
#' @param test_market The name of the test market (character)
#' @param end_post_period The end date of the post period. Must be a character of format "YYYY-MM-DD" -- e.g., "2015-11-01"
#' @param alpha Desired tail-area probability for posterior intervals. For example, 0.05 yields 0.95 intervals
#' @param prior_level_sd Prior SD for the local level term (Gaussian random walk). Default is 0.01. The bigger this number is, the more wiggliness is allowed for the local level term.
#' Note that more wiggly local level terms also translate into larger posterior intervals
#' This parameter will be overwritten if you're using the bsts_modelargs parameter
#' @param control_matches Number of matching control markets to use in the analysis (default is 5)
#' @param analyze_betas Controls whether to test the model under a variety of different values for prior_level_sd.
#' @param nseasons Seasonality for the bsts model -- e.g., 52 for weekly seasonality

#' @import ggplot2

#' @export inference
#' @examples
#' \dontrun{
#' library(MarketMatching)
#' ##-----------------------------------------------------------------------
#' ## Analyze causal impact of a made-up weather intervention in Copenhagen
#' ## Since this is weather data it is a not a very meaningful example. 
#' ## This is merely to demonstrate the function.
#' ##-----------------------------------------------------------------------
#' data(weather, package="MarketMatching")
#' mm <- best_matches(data=weather, 
#'                    id="Area",
#'                    markets_to_be_matched=c("CPH", "SFO"),
#'                    date_variable="Date",
#'                    matching_variable="Mean_TemperatureF",
#'                    parallel=FALSE,
#'                    warping_limit=1, # warping limit=1
#'                    dtw_emphasis=0, # rely only on dtw for pre-screening
#'                    matches=5, # request 5 matches
#'                    start_match_period="2014-01-01",
#'                    end_match_period="2014-10-01")
#' library(CausalImpact)
#' results <- inference(matched_markets=mm,
#'                      test_market="CPH",
#'                      analyze_betas=FALSE,
#'                      control_matches=5, # use all 5 matches for inference
#'                      end_post_period="2015-12-15",
#'                      prior_level_sd=0.002)
#' }
#' @usage
#' inference(matched_markets=NULL,
#'           bsts_modelargs=NULL,
#'           test_market=NULL,
#'           end_post_period=NULL,
#'           alpha=0.05,
#'           prior_level_sd=0.01,
#'           control_matches=5, 
#'           analyze_betas=FALSE, 
#'           nseasons=NULL)
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
#' \item{\code{ControlData}}{A \code{data.frame} with the data for the control markets}
#' \item{\code{PlotResiduals}}{Plot of the residuals using \code{ggplot2}}
#' \item{\code{TestName}}{The name of the test market}
#' \item{\code{TestName}}{The name of the control market}
#' \item{\code{zooData}}{A \code{zoo} time series object with the test and control data}
#' \item{\code{Predictions}}{Actual versus predicted values}
#' \item{\code{CausalImpactObject}}{The CausalImpact object created}
#' \item{\code{Coefficients}}{The average posterior coefficients}
#' 
#' @import zoo
#' @import bsts
#' @importFrom scales comma
#' @import Boom

inference <- function(matched_markets=NULL, bsts_modelargs=NULL, test_market=NULL, end_post_period=NULL, alpha=0.05, prior_level_sd=0.01, control_matches=5, analyze_betas=FALSE, nseasons=NULL){

  ## use nulling to avoid CRAN notes
  id_var <- NULL
  BestControl <- NULL
  date_var <- NULL
  cum.effect <- NULL
  Date <- NULL
  Response <- NULL
  lower_bound <- NULL
  upper_bound <- NULL
  Predicted <- NULL
  Cumulative <- NULL
  match_var <- NULL
  SD <- NULL
  Beta <- NULL
  value <- NULL
  variable <- NULL
  LocalLevel <- NULL
  Residuals <- NULL
  Pointwise <- NULL
  
  stopif(is.null(matched_markets), TRUE, "ERROR: Need to specify a matched market object")
  stopif(is.null(test_market), TRUE, "ERROR: Need to specify a test market")
  stopif(length(test_market)>1, TRUE, "ERROR: inference() can only analyze one test market at a time. Call the function separately for each test market")
  
  ## Model settings
  if (!is.null(bsts_modelargs) & !is.null(nseasons)){
    cat("\tNOTE: You're passing arguments directly to bsts while also specifying nseasons \n")
    cat("\tNOTE: bsts_modelargs will overwrite nseasons \n")
    cat("\n")
  }
  if (is.null(bsts_modelargs)){
    if (is.null(nseasons)){
      bsts_modelargs <- list(prior.level.sd=prior_level_sd)
    } else{
      bsts_modelargs <- list(nseasons=nseasons, prior.level.sd=prior_level_sd)
    }
  } else{
    if (analyze_betas==TRUE){
      analyze_betas <- FALSE
      cat("\tNOTE: analyze_betas turned off when bsts model arguments are passed directly \n")
      cat("\tConsider using the nseasons and prior_level_sd parameters instead \n")
      cat("\n")
    }
  }
  
  ## copy the distances
  mm <- dplyr::filter(matched_markets$BestMatches, rank<=control_matches)

  data <- matched_markets$Data
  mm$id_var <- mm[[names(mm)[1]]]
  mm <- dplyr::arrange(mm, id_var, BestControl)

  ## check if the test market exists
  stopif(test_market %in% unique(data$id_var), FALSE, paste0("ERROR: Test market ", test_market, " does not exist"))

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
  cat(paste0("\tMarket ID: ", matched_markets$MarketID, "\n"))
  cat(paste0("\tDate Variable: ", matched_markets$DateVariable, "\n"))
  cat(paste0("\tMatching Metric: ", matched_markets$MatchingMetric, "\n"))
  cat("\n")
  cat(paste0("\tTest Market: ", test_market, "\n"))
  for (i in 1:length(control_market)){
    cat(paste0("\tControl Market ", i, ": ", control_market[i], "\n"))
  }
  cat("\n")
  cat(paste0("\tMatching (pre) Period Start Date: ", MatchingStartDate, "\n"))
  cat(paste0("\tMatching (pre) Period End Date: ", MatchingEndDate, "\n"))
  cat(paste0("\tPost Period Start Date: ", post_period_start_date, "\n"))
  cat(paste0("\tPost Period End Date: ", post_period_end_date, "\n"))
  cat("\n")
  cat(paste0("\tbsts parameters: \n"))
  modelparms <- names(bsts_modelargs)
  for (p in 1:length(bsts_modelargs)){
    cat(paste0("\t  ", modelparms[p], ": ", bsts_modelargs[p], "\n"))
  }
  if (!("nseasons" %in% modelparms)){
    cat("\t  No seasonality component (controlled for by the matched markets) \n")
  }
  cat(paste0("\tPosterior Intervals Tail Area: ", 100*(1-alpha), "%\n"))
  cat("\n")

  ## run the inference
  pre.period <- c(as.Date(MatchingStartDate), as.Date(MatchingEndDate))
  post.period <- c(as.Date(post_period_start_date), as.Date(post_period_end_date))
  set.seed(2015)
  impact <- CausalImpact(ts, pre.period, post.period, alpha=alpha, model.args=bsts_modelargs)

  if(analyze_betas==TRUE){
    ## estimate betas for different values of prior sd
    betas <- data.frame(matrix(nrow=11, ncol=4))
    names(betas) <- c("SD", "SumBeta", "DW", "MAPE")
    for (i in 0:20){
      step <- (max(0.1, prior_level_sd) - min(0.001, prior_level_sd))/20
      sd <- min(0.001, prior_level_sd) + step*i
      if (is.null(nseasons)){
        args <- list(prior.level.sd=sd)
      } else{
        args <- list(prior.level.sd=sd, nseasons=nseasons)
      }
      m <- CausalImpact(ts, pre.period, post.period, alpha=alpha, model.args=args)
      burn <- SuggestBurn(0.1, m$model$bsts.model)
      b <- sum(apply(m$model$bsts.model$coefficients[-(1:burn),], 2, CMean))
      betas[i+1, "SD"] <- sd
      betas[i+1, "SumBeta"] <- b
      preperiod <- subset(m$series, cum.effect == 0)
      betas[i+1, "DW"] <- dw(preperiod$response, preperiod$point.pred)
      betas[i+1, "MAPE"] <- mape_no_zeros(preperiod$response, preperiod$point.pred)
    }
  }
  
  burn <- SuggestBurn(0.1, impact$model$bsts.model)
  
  
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

  ymin <- min(min(impact$series$response), min(impact$series$point.pred.lower), min(ref), min(y))
  ymax <- max(max(impact$series$response), max(impact$series$point.pred.upper), max(ref), max(y))

  ## create actual versus predicted plots
  stopif (length(date) != nrow(data.frame(impact$series)), "ERROR: you might have holes (skipped weeks/days/months) in your time series ")
    
  avp <- data.frame(cbind(date, data.frame(impact$series)[,c("response", "point.pred", "point.pred.lower", "point.pred.upper")]))
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
  results[[12]] <- ggplot(data=plotdf, aes(x=date_var, y=match_var, colour=as.factor(id_var))) +
    geom_line() +
    theme_bw() + theme(legend.title = element_blank(), axis.title.x = element_blank()) + ylab("") + xlab("Date") +
    geom_vline(xintercept=as.numeric(MatchingEndDate), linetype=2) +
    scale_y_continuous(labels = scales::comma, limits=c(ymin, ymax))

  if(analyze_betas==TRUE){
    ## plot betas at various local level SDs
    results[[13]] <- ggplot(data=betas, aes(x=SD, y=Beta)) +
      geom_line() +
      theme_bw() + theme(legend.title = element_blank()) +
      geom_vline(xintercept=as.numeric(prior_level_sd), linetype=2) + xlab("Local Level Prior SD")
    
    ## plot DWs and MAPEs at different SDs
    plotdf <- melt(data=betas, id="SD")
    results[[14]] <- ggplot(data=plotdf, aes(x=SD, y=value, colour=as.factor(variable))) + geom_line() +
      theme_bw() + theme(legend.title = element_blank()) +
      geom_vline(xintercept=as.numeric(prior_level_sd), linetype=2) + xlab("Local Level Prior SD") +
      facet_grid(variable ~ ., scales="free") + ylab("") + guides(colour="none")
  }
  
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

#' Given a test market, analyze the impact of fake interventions (prospective power analysis)
#'
#' \code{test_fake_lift} Given a matched_market object from the best_matches function, this function analyzes the causal impact of fake interventions using the CausalImpact package.
#' The function returns an object of type "market_inference" which contains the estimated impact of the intervention (absolute and relative).
#'
#' @param matched_markets A matched_market object created by the market_matching function
#' This parameter will overwrite the values specified in prior_level_sd and nseasons. ONLY use this if you're using intricate bsts settings
#' For most use-cases, using the prior_level_sd and nseasons parameters should be sufficient
#' @param test_market The name of the test market (character)
#' @param end_fake_post_period The end date of the post period. Must be a character of format "YYYY-MM-DD" -- e.g., "2015-11-01"
#' @param alpha Desired tail-area probability for posterior intervals. For example, 0.05 yields 0.95 intervals
#' @param prior_level_sd Prior SD for the local level term (Gaussian random walk). Default is 0.01. The bigger this number is, the more wiggliness is allowed for the local level term.
#' Note that more wiggly local level terms also translate into larger posterior intervals
#' This parameter will be overwritten if you're using the bsts_modelargs parameter
#' @param control_matches Number of matching control markets to use in the analysis (default is 5)
#' @param nseasons Seasonality for the bsts model -- e.g., 52 for weekly seasonality
#' @param max_fake_lift The maximum absolute fake lift -- e.g., 0.1 means that the max lift evaluated is 10 percent and the min lift is -10 percent
#' Note that randomization is injected into the lift, which means that the max lift will not be exactly as specified
#' @param steps The number of steps used to calculate the power curve (default is 10)
#' @param lift_pattern_type Lift pattern. Default is constant. The other choice is a random lift..
#' 
#' 
#' @import ggplot2

#' @export test_fake_lift
#' @examples
#' \dontrun{
#' library(MarketMatching)
#' ##-----------------------------------------------------------------------
#' ## Create a pseudo power curve for various levels of lift
#' ## Since this is weather data it is a not a very meaningful example. 
#' ## This is merely to demonstrate the function.
#' ##-----------------------------------------------------------------------
#' data(weather, package="MarketMatching")
#' mm <- best_matches(data=weather, 
#'                    id="Area",
#'                    markets_to_be_matched=c("CPH", "SFO"),
#'                    date_variable="Date",
#'                    matching_variable="Mean_TemperatureF",
#'                    warping_limit=1, # warping limit=1
#'                    dtw_emphasis=0, # rely only on dtw for pre-screening
#'                    matches=5, # request 5 matches
#'                    start_match_period="2014-01-01",
#'                    end_match_period="2014-10-01")
#' library(CausalImpact)
#' results <- test_fake_lift(matched_markets=mm,
#'                      test_market="CPH",
#'                      lift_pattern_type="constant",
#'                      control_matches=5, # use all 5 matches for inference
#'                      end_fake_post_period="2015-12-15",
#'                      prior_level_sd=0.002, 
#'                      max_fake_lift=0.1)
#' }
#' @usage
#' test_fake_lift(matched_markets=NULL,
#'           test_market=NULL,
#'           end_fake_post_period=NULL,
#'           alpha=0.05,
#'           prior_level_sd=0.01,
#'           control_matches=NULL, 
#'           nseasons=NULL, 
#'           max_fake_lift=NULL, 
#'           steps=10,
#'           lift_pattern_type="constant")
#'
#' @return Returns an object of type \code{matched_market_power}. The object has the
#' following elements:
#' \item{\code{ResultsData}}{The results stored in a data.frame}
#' \item{\code{ResultsGraph}}{The results stored in a ggplot graph}
#' \item{\code{LiftPattern}}{The random pattern applied to the lift}
#' \item{\code{FitCharts}}{The underlying actual versus fitted charts for each fake lift} 
#' \item{\code{FitData}}{The underlying actual versus fitted data for each fake lift} 
#' @import zoo
#' @importFrom scales percent

test_fake_lift <- function(matched_markets=NULL, test_market=NULL, end_fake_post_period=NULL, alpha=0.05, prior_level_sd=0.01, control_matches=NULL, nseasons=NULL, max_fake_lift=NULL, steps=10, lift_pattern_type="constant"){
  
  ## use nulling to avoid CRAN notes
  id_var <- NULL
  BestControl <- NULL
  date_var <- NULL
  Response <- NULL
  match_var <- NULL
  counter <- NULL
  s <- NULL
  m <- NULL
  upper_bound <- NULL
  lower_bound <- NULL 
  Predicted <- NULL
  Date <- NULL
  
  
  if (steps<10){steps <- 10}

  stopif(length(test_market)>1, TRUE, "ERROR: inference() can only analyze one test market at a time. Call the function separately for each test market")

  ## set the matches
  if (is.null(control_matches)){
    control_matches <- length(unique(matched_markets$Data$id_var))
  } 
  
  ## copy the distances
  mm <- dplyr::filter(matched_markets$BestMatches, rank<=control_matches)
  
  
  data <- matched_markets$Data
  mm$id_var <- mm[[names(mm)[1]]]
  mm <- dplyr::arrange(mm, id_var, BestControl)
  
  ## check if the test market exists
  stopif(test_market %in% unique(data$id_var), FALSE, paste0("ERROR: Test market ", test_market, " does not exist"))
  
  ## if an end date has not been provided, then choose the max of the data
  if (is.null(end_fake_post_period)){
    end_fake_post_period <- as.Date(max(subset(data, id_var==test_market)$date_var))
  }
  
  # filter for dates
  MatchingStartDate <- as.Date(subset(mm, id_var==test_market)$MatchingStartDate[1])
  MatchingEndDate <- as.Date(subset(mm, id_var==test_market)$MatchingEndDate[1])
  data <- dplyr::filter(data, date_var>=MatchingStartDate & date_var<=as.Date(end_fake_post_period))
  
  ## get the control market name
  control_market <- subset(mm, id_var==test_market)$BestControl
  
  ## get the test and ref markets
  mkts <- create_market_vectors(data, test_market, control_market)
  y <- mkts[[1]]
  ref <- mkts[[2]]
  date <- mkts[[3]]
  end_fake_post_period <- max(date)
  post_period <- date[date > as.Date(mm[1, "MatchingEndDate"])]
  stopif(length(post_period)==0, TRUE, "ERROR: no valid data in the post period")
  post_period_start_date <- min(post_period)
  post_period_end_date <- max(post_period)

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
  cat(paste0("\tFake post Period Start Date: ", post_period_start_date, "\n"))
  cat(paste0("\tFake post Period End Date: ", post_period_end_date, "\n"))
  cat(paste0("\tMatching Metric: ", matched_markets$MatchingMetric, "\n"))
  cat(paste0("\tbsts parameters: \n"))
  if (is.null(nseasons)){
    cat("\t   No seasonality component (controlled for by the matched markets) \n")
    nseaons <- 1
  }
  cat(paste0("\t   Prior level SD ", prior_level_sd))
  cat("\n")
  cat("\t   Good idea to run this at various levels of prior_level_sd (e.g., 0.0001 and 0.1)")
  cat("\n")
  cat("\n")
  
  ## model arguments (no standardization of data since we have to re-use the model)
  bsts_modelargs=list(standardize.data=TRUE, niter=1000, prior.level.sd=prior_level_sd, season.duration=1, dynamic.regression=FALSE, max.flips=-1, nseasons=nseasons)
  
  cat(paste0("\tMax Fake Lift: ", max_fake_lift, "\n"))
  cat("\n")
  pre.period <- c(as.Date(MatchingStartDate), as.Date(MatchingEndDate))
  post.period <- c(as.Date(post_period_start_date), as.Date(post_period_end_date))
  set.seed(2015)
  
  stepsize <- max_fake_lift/steps
  
  temp_df <- data.frame(cbind(y, ref, date))
  y_post <- subset(temp_df, date>as.Date(MatchingEndDate))$y
  ref_post <- subset(temp_df, date>as.Date(MatchingEndDate))$ref
  y_pre <- subset(temp_df, date<=as.Date(MatchingEndDate))$y

  if (toupper(lift_pattern_type)=="RANDOM"){
     pattern <- range01(sample(1:100, length(y_post), replace=T))
     s <- 1/mean(pattern)
     cat(paste0("\tLift pattern: ", lift_pattern_type, "\n"))
     cat("\n")
  } else if (toupper(lift_pattern_type)=="CONSTANT"){
    pattern <- rep(1, length(y_post))
    s <- 1/mean(pattern)
    cat(paste0("\tLift pattern: ", lift_pattern_type, "\n"))
    cat("\n")
  } else{
    cat(paste0("\tLift pattern ", lift_pattern_type, " not recognized. Using constant lift \n"))
    lift_pattern_type <- "constant"
    cat("\n")
  }

  power <- list()
  charts <- list()
  avps <- list()
  counter <- 0
    for (i in (-steps):steps){
       set.seed(2020)
       y_post_new <- (stepsize*i*pattern*s+1)*y_post
       y_new <- c(y_pre, y_post_new)
       fake_lift=sum((stepsize*i*pattern*s+1)*y_post)/sum(y_post)-1
       ts <- zoo(cbind(y_new, ref), date)
       if (counter>=0){
          impact <- CausalImpact(data=ts, pre.period=pre.period, post.period=post.period, alpha=alpha, model.args=bsts_modelargs)
          m <- impact$model$bsts.model
       } else{
         impact <- CausalImpact(post.period.response = y_post_new, alpha=alpha, bsts.model=m)
       }
       prob_causal <- 1-impact$summary$p[2]
       power[[counter+1]] <- data.frame(fake_lift, prob_causal)
       avp <- data.frame(cbind(date, data.frame(impact$series)[,c("response", "point.pred", "point.pred.lower", "point.pred.upper")]))
       names(avp) <- c("Date", "Response", "Predicted", "lower_bound", "upper_bound")
       ymin <- min(min(impact$series$response), min(impact$series$point.pred.lower), min(ref), min(y))
       ymax <- max(max(impact$series$response), max(impact$series$point.pred.upper), max(ref), max(y))
       avps[[counter+1]] <- avp
       charts[[counter+1]] <- ggplot(data=avp, aes(x=Date)) +
         geom_line(aes(y=Response, colour = "Observed"), size=1.2) +
         geom_ribbon(aes(ymin=lower_bound, ymax=upper_bound), fill="grey", alpha=0.3) +
         geom_line(aes(y=Predicted, colour = "Expected"), size=1.2) +
         theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
         geom_vline(xintercept=as.numeric(MatchingEndDate), linetype=2) +
         scale_y_continuous(labels = scales::comma, limits=c(ymin, ymax)) +
         ggtitle(paste0("Fake lift = ", round(fake_lift, 2)))
       counter <- counter+1
    }
  power_df <- data.frame(dplyr::bind_rows(power))
  power_chart <- ggplot(data=power_df, aes(x=fake_lift, y=prob_causal)) + 
    geom_line(size=2) + 
    ylab(paste0("Probability of causal impact")) + 
    xlab("Average Lift During the Fake Post Period") + 
    geom_vline(xintercept = 0) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)
  
  ### return the results
  object <- list(ResultsData=power_df, ResultsGraph=power_chart, LiftPattern=pattern, FitCharts=charts, FitData=avps)
  class(object) <- "fake_lift_analysis"
  return (object)
}

#' Roll up the suggested test/control optimal pairs for pseudo power analysis (testing fake lift)
#'
#' \code{roll_up_optimal_pairs} Takes the suggested optimal pairs from best_matches() and aggregates the data for pseudo power analysis (test_fake_lift()).
#' You run this function and then pass the result (a matched markets object) to test_fake_lift.
#' @param matched_markets A matched market object from best_matches. 
#' @param percent_cutoff The percent of data (by volume) to be included in the future study. Default is 1. 0.5 would be 50 percent.
#' @param synthetic If set to TRUE, the control markets are not aggregated so BSTS can determine weights for each market and create a synthetic control.
#' If set to FALSE then the control markets are aggregated and each market will essentially get the same weight.
#' If you have many control markets (say, more than 25) it is recommended to choose FALSE. Default is FALSE.
#' @import dplyr
#'
#' @export roll_up_optimal_pairs
#' @examples
#' \dontrun{
#' ##-----------------------------------------------------------------------
#' ## Generate the suggested test/control pairs
#' ##-----------------------------------------------------------------------
#' library(MarketMatching)
#' data(weather, package="MarketMatching")
#' mm <- best_matches(data=weather, 
#'                    id="Area",
#'                    date_variable="Date",
#'                    matching_variable="Mean_TemperatureF",
#'                    parallel=FALSE,
#'                    suggest_market_splits=TRUE,
#'                    start_match_period="2014-01-01",
#'                    end_match_period="2014-10-01")
#'                    
#' ##-----------------------------------------------------------------------
#' ## Roll up the pairs to generate test and control markets
#' ## Synthetic=FALSE means that the control markets will be aggregated 
#' ## -- i.e., equal weighhs in CausalImpact
#' ##-----------------------------------------------------------------------
#'                    
#' rollup <- roll_up_optimal_pairs(matched_markets=mm, 
#'                                 percent_cutoff=1, 
#'                                 synthetic=FALSE)
#'                                 
#' ##-----------------------------------------------------------------------
#' ## Pseudo power analysis (fake lift analysis)
#' ##-----------------------------------------------------------------------
#' 
#' results <- test_fake_lift(matched_markets=rollup,
#'                      test_market="TEST",
#'                      lift_pattern_type="constant",
#'                      end_fake_post_period="2015-12-15",
#'                      prior_level_sd=0.002, 
#'                      max_fake_lift=0.1)
#' }
#' @usage
#' roll_up_optimal_pairs(matched_markets=NULL,
#'                       percent_cutoff=1,
#'                       synthetic=FALSE)
#'
#' @return Returns an object of type \code{market_matching}. The object has the
#' following elements:
#' \item{\code{BestMatches}}{A data.frame that contains the best matches for each market in the input dataset}
#' \item{\code{Data}}{The raw data used to do the matching}
#' \item{\code{MarketID}}{The name of the market identifier}
#' \item{\code{MatchingMetric}}{The name of the matching variable}
#' \item{\code{DateVariable}}{The name of the date variable}
#' \item{\code{SuggestedTestControlSplits}}{Always NULL}

roll_up_optimal_pairs <- function(matched_markets=NULL, percent_cutoff=1, synthetic=FALSE){
  
  s <- NULL
  e <- NULL
  mm <- NULL
  percent_of_volume <- NULL
  date_var <- NULL 
  match_var <- NULL 
  TestCell <- NULL 
  
  
  ###  check out the matched markets object
  stopif(is.null(matched_markets), TRUE, "The matched_markets object is null")
  stopif(class(matched_markets)=="matched_market", FALSE, "The matched_markets object is not of type matched_market")
  stopif(is.null(matched_markets$SuggestedTestControlSplits), TRUE, "The matched_markets object does not contain suggested pairs. Try re-running with the right settings")
  cat("\n")
  
  if (min(matched_markets$SuggestedTestControlSplits$percent_of_volume)>=percent_cutoff){
    percent_cutoff <- min(matched_markets$SuggestedTestControlSplits$percent_of_volume)
  }
  
  d <- matched_markets$SuggestedTestControlSplits %>%
    dplyr::filter(percent_of_volume<=percent_cutoff)
  
  t <- d$test_market
  c <- d$control_market
  
  ### Roll up the data
  if (synthetic==FALSE){
     ru <- dplyr::mutate(matched_markets$Data, 
                        TestCell=case_when(id_var %in% t ~ "TEST", 
                                           id_var %in% c ~ "CONTROL",
                                         TRUE ~ "DELETE")) %>%
       dplyr::filter(!TestCell=="DELETE") %>%
       dplyr::group_by(TestCell, date_var) %>%
       dplyr::summarise(match_var=sum(match_var)) %>%
       dplyr::ungroup()
  } else {
    ru <- dplyr::mutate(matched_markets$Data, 
                        TestCell=case_when(id_var %in% t ~ "TEST",
                                           id_var %in% c ~ id_var,
                                           TRUE ~ "DELETE")) %>%
      dplyr::filter(!TestCell=="DELETE") %>%
      dplyr::group_by(TestCell, date_var) %>%
      dplyr::summarise(match_var=sum(match_var)) %>%
      dplyr::ungroup()
  }
  
  s <- as.character(max(matched_markets$BestMatches$MatchingStartDate))
  e <- as.character(min(matched_markets$BestMatches$MatchingEndDate))
  
  ## Generate the best matches object
  mm <- MarketMatching::best_matches(data=ru,
                     suggest_market_splits = FALSE,
                     dtw_emphasis = 0,
                     matches=NULL,
                     parallel = FALSE,
                     matching_variable = "match_var", 
                     date_variable = "date_var", 
                     id_variable = "TestCell", 
                     start_match_period = s, 
                     end_match_period = e)
  
  mm$DateVariable <- matched_markets$DateVariable
  mm$MatchingMetric <- matched_markets$MatchingMetric
  
  ## Return the results
  cat("The market ID variable has been renamed to TestCell")
  cat("\n")
  cat("\n")
  cat("The aggregated test cell market will be called TEST)")
  cat("\n")
  cat("\n")
  return(mm)
}
