#################################################
# Load PCE deflator                             #
#################################################


# Packages
library("quantmod")

# environment in which to store data 
data <- new.env()

# set tickers
tickers <- c("PCEPI")

# import data from FRED database
getSymbols( tickers
            , src = "FRED"  # needed!
            , env = data
            , adjust = TRUE
)


# subset data to within time range
PCEPI <- data$PCEPI
a <- window(PCEPI, start="1972-01-01", end="2014-12-01")

# Changing monthly frequency to annual, end of the year value
year.end <- endpoints(a, on = "years")
yearly <- period.apply(a, INDEX = year.end, FUN = mean)
PCE <- data.frame(date=index(yearly), coredata(yearly))

PCE$year <- format(PCE$date, "%Y")
PCE$date <- NULL
PCE <- PCE[,c("year", "PCEPI")]

rm(a, PCEPI, data, tickers, year.end, yearly)
