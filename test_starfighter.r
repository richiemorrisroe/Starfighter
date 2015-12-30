require(testthat)
require(dplyr)
source("starfighter.R")
venue <- "TESTEX"
stock <- "FOOBAR"
apikey <- scan("apikey.txt", what="char")


direction <- "buy"
quote <- get_quote("TESTEX", "FOOBAR")
orderinfo <- get_bid(content(quote))
qty <- orderinfo[2]
price <- orderinfo[1]
ord <- create_order(account="EXB123456", venue=venue, stock=stock, qty=qty, price=price, direction=direction, orderType="limit")
sell <- create_order(account="EXB123456", venue=venue, stock=stock, qty=qty, price=price+1, direction="sell", orderType="limit")
res <- place_order(venue, stock, body=ord, apikey=apikey)
sellres <- place_order(venue, stock, body=sell, apikey)
