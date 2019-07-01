Package: MarketMatching

Version: 1.1.2


MarketMacthing version 1.1.2
============================

Fixed documentation errors.

Added additional input checks.

Refactored the code to handle errors with markets_to_be_matched not being in data. Check happens prior to the execution of the calc distances loop now.

Modified calculate_distances and best_matches to allow for parallel processing to be executed when the specifies markets for markets_to_be_matched.

MarketMacthing version 1.1.1
============================

Submitted to CRAN.
