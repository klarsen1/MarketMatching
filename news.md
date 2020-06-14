Package: MarketMatching

Version: 1.1.3

MarketMacthing version 1.1.3
============================

Added the function prospective\_power() to calculate power curves. Fixed
minor bugs.

MarketMacthing version 1.1.2
============================

Fixed documentation errors.

Added additional input checks.

Refactored the code to handle errors with markets\_to\_be\_matched not
being in data. Check happens prior to the execution of the calc
distances loop now.

Modified calculate\_distances and best\_matches to allow for parallel
processing to be executed when the specifies markets for
markets\_to\_be\_matched.

MarketMacthing version 1.1.1
============================

Submit to CRAN.
