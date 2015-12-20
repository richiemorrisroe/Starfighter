## rm(list=ls())
library(dplyr)
library(lubridate)
source("starfighter.R")
account <- "MS45612558"
key <- scan("apikey.txt", what="char")
venue <- "VHIPEX"
stock <- "CHC"
fudge <- 0.00
## Sys.sleep(100)
orderinfo <- get_first_real_price(venue=venue, stock=stock, fudge=fudge)
orig_price <- NA
if(is.na(orig_price)) {
    cat("first bid ", orderinfo[1], "\n") 
    orig_price <- orderinfo[1]
    cat("original price was ", orig_price, "\n")
}
target_price <- orig_price+1
if(is.na(target_price)) {
    target_price <- floor(orderinfo[1])
    cat("target_price is", target_price, "\n")
}
last_price <- 0
total_filled <- 0
asking_price <- orig_price
ok <- TRUE
i <- 1
ordlist <- vector(mode="list", length=100000)
while(ok) {
    orders <- get_orderbook(venue, stock)
    ordlist[[i]] <- orders
    i <- i + 1 
    ok <- content(orders)$ok
    
}

## while(total_filled<100000) {
## bid <- target_price
## qty <- orderinfo[2]
## ord <- create_order(account=account, venue=venue, stock=stock, price=bid, qty=qty, direction="buy", orderType="immediate-or-cancel")
## cat("asking price", asking_price, "target price ", target_price, "\n")
## asking_price <- get_first_real_price(venue, stock, fudge=0)[1]
## if(asking_price<=target_price) {
##     t <- place_order(venue=venue, stock=stock, body=ord, apikey=key)
## }
## if(content(t)$ok) {
##     total_filled <- total_filled+content(t)$totalFilled
##     cat(total_filled, " shares purchased", "\n")
##     new_quote <- get_first_real_price(venue, stock)
##     asking_price <- new_quote[1]
##     cat("new ask ", new_quote[1], " available ", new_quote[2]*100, "\n")
##     new_qty <- new_quote[2]
##     Sys.sleep(5)
##     if(total_filled>99000) {
##         cat("Getting close", total_filled, "shares purchased", "\n")
##     }
## }
## else {
##     Sys.sleep(60)
## }
## }

