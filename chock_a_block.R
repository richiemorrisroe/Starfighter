## rm(list=ls())
library(dplyr)
library(lubridate)
source("starfighter_gm.R")
source("starfighter.R")
tryCatch( {
first <- start_level("chock_a_block")
contents_level <- content(first)
venue <- unlist(contents_level[["venues"]])
ticker <- unlist(contents_level[["tickers"]])
instance <- unlist(contents_level[["instanceId"]])
account <- unlist(contents_level[["account"]])
daylength <- unlist(contents_level[["secondsPerTradingDay"]])

key <- scan("apikey.txt", what="char")
fudge <- 0.00
last_price <- 0
total_filled <- 0

target_price <- NULL
bid <- NULL
## qty <- orderinfo[2]
orderbook <- NULL
test_qty <- c(10, 100, 1000)
while(is.null(bid)) {
    orderbook <- get_orderbook(venue=venue, stock=ticker) %>% parse_response()
    print(orderbook)
    if(is.null(orderbook$asks)) {
        next
    }
    else {
        bid <- min(orderbook[["asks"]]$price)+10
    }
}
while(is.null(target_price)) {
for(i in 1:length(test_qty)) {
    ord <- create_order(account=account, venue=venue, stock=ticker, price=bid, qty=test_qty[i], direction="buy", orderType="fill-or-kill")
    placed <- place_order(venue=venue, stock=ticker, body=ord, apikey=apikey)
    placed_response <- parse_response(placed)
    total_filled <- total_filled+placed_response$totalFilled
    cat("just filled ", total_filled, "\n")
    Sys.sleep(40)
    status <- level_status(level=first)
    stat_parsed <- parse_response(status)
    if(!is.null(stat_parsed$flash)) {
        risk <- stat_parsed$flash$info
        risksplit <- unlist(strsplit(risk, "\\s"))
        price <- risksplit[length(risksplit)]
        target_price <- as.numeric(gsub("[^0-9]+", "", x=price))
        cat("target price is ", target_price, "\n")
        break
    }
}
}
    if(!is.null(target_price)) {
        while(total_filled<100000) {
            orderbook <- get_orderbook(venue=venue, stock=ticker) %>% parse_response()
            if(is.null(orderbook$asks)) {
                next
            }
    else {
        bid <- min(orderbook[["asks"]]$price)+1
    }
            if(bid<target_price) {
                new_ord <- create_order(account=account, venue=venue, stock=ticker, price=bid, qty=10000, direction="buy", orderType="immediate-or-cancel")
                ord <- place_order(venue=venue, stock=ticker, body=new_ord, apikey=apikey)
                placed_response <- parse_response(placed)
                cat("filled ", placed_response$totalFilled, " with ", total_filled, "previous orders", "\n") 
                total_filled <- total_filled+placed_response$totalFilled
                Sys.sleep(20)
            }
            else {
                Sys.sleep(40)
            }
        }
    }
},
finally= {change_instance(first, "stop")})
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

