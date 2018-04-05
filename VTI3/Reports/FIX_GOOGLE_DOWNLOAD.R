## aTtempts at finding other ways to download financial data
# Could not find anythign that worked


install.packages("Quandl")
library(Quandl)
quandl_api = "sNyK4QSJ_kTE8ASx6Uf8"

library(lubridate)
library(stringr)

dat <- read.csv("WIKI_PRICES.CSV",stringsAsFactors = FALSE)

# Add the key to the Quandl keychain
Quandl.api_key(quandl_api)

quandl_get <-
  function(sym, start_date = "2017-01-01") {
    require(devtools)
    require(Quandl)
    # create a vector with all lines
    tryCatch(Quandl(c(
      paste0("WIKI/", sym, ".8"),  #  Adj. Open
      paste0("WIKI/", sym, ".9"),  # Adj. High
      paste0("WIKI/", sym, ".10"), # Adj. Low
      paste0("WIKI/", sym, ".11"), # Adj. Close
      paste0("WIKI/", sym, ".12")), # Adj. Volume
      start_date = start_date,
      type = "zoo"
    ))
  }

df <- quandl_get("VTI")

df <- Quandl.datatable('WIKI/PRICES', date='1999-11-18', ticker='A')
df <- Quandl.datatable('WIKI/PRICES', ticker='A', date='1999-11-18,1999-11-19,1999-11-22')
?Quandl.datatable


today()
dates <- seq(as.Date("2015-01-01"),as.Date(today()),by=1)
df <- Quandl.datatable('WIKI/PRICES', ticker='VTI.11', date=as.character(dates)[1:40])


unique(dat$ticker)

library(quantmod)
getSymbols("VIX",src="yahoo",from="2001-01-01")
a
