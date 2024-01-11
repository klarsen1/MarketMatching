Package: MarketMatching

Version: 1.2.1

# MarketMacthing version 1.2.1

-   Rebuilt based on R version 4.3.2
-   Removed ggplot settings that created warnings
-   Wraps as.Date() around the date variable to truncate
-   Improved error and warning messaging

# MarketMacthing version 1.2.0

-   This is a major release for CRAN.
-   The biggest differences between this version and the previous
    version in CRAN are:
    1.  Prospective pseudo power analysis by testing the design against
        fake interventions.
    2.  Generate optimal test/control market splits.

# MarketMacthing version 1.1.4

-   Changed MAPE calculation to not re-scale the residuals.
-   Minor bug fixes.
-   The fake lift testing function does not refit underlying BSTS model
    at every step on the curve.
-   Added the parameter suggest_market_splits to best_matches() to
    provide a market split recommendations.
-   You can now pass just 2 markets (no longer a requirement of 3+
    markets).
-   Added more details on the numbers behind the distance calculations.
-   Not submitted to CRAN.

# MarketMacthing version 1.1.4

-   Changed MAPE calculation to not re-scale the residuals.
-   Minor bug fixes.
-   test_fake_lift() does not refit underlying BSTS model at every step
    on the curve.
-   Added the parameter suggest_market_splits to best_matches() to
    provide a market split recommendations.
-   You can now pass just 2 markets (no longer a requirement of 3+
    markets).
-   Added more details on the numbers behind the distance calculations.

# MarketMacthing version 1.1.3

Added the function test_fake_lift() to calculate pseudo power curves.
Fixed minor bugs.

# MarketMacthing version 1.1.2

Fixed documentation errors.

Added additional input checks.

Refactored the code to handle errors with markets_to_be_matched not
being in data. Check happens prior to the execution of the calc
distances loop now.

Modified calculate_distances and best_matches to allow for parallel
processing to be executed when the specifies markets for
markets_to_be_matched.

# MarketMacthing version 1.1.1

Submit to CRAN.
