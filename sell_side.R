source("starfighter.R")
source("starfighter_gm.R")

tryCatch({
    level <- start_level("sell_side")
    if(content(level)$ok==TRUE) {
    account <- get_component(level, "account")
    venue <- get_component(level, "venues")
    ticker <- get_component(level, "tickers")
    balance <- get_component(level, "balances")
    position <- data.frame("bought"=0, "sold"=0, "NAV"=0)
    bill_position <- NA
    sell_price <- NA
    buy_price <- NA
    max_stock <- 1000
    NAV <- position$NAV
    stocks <- get_tickers(venue=venue)
    print(parse_response(stocks))
    }
    else {
        Sys.sleep(60)
        level <- start_level("sell_side")
    }
    while(NAV<10000) {
        NAV <- position$NAV
        current_book <- tryCatch({get_orderbook(venue=venue, stock=ticker)},
                                           error=function(e) return(NA))
        ordbook <- tryCatch({orderbook(parse_response(current_book))}, error=function(e) return(NA))
        if(is.null(ordbook)) {
            current_book <- get_orderbook(venue=venue, stock=ticker)
            ordbook <- orderbook(parse_response(current_book))
    
        }
        else {
            sell_price <- min(ordbook@asks$price)-1
            buy_price <- min(ordbook@bids$price)+1
            sell_qty <- min(ordbook@asks$qty)
            buy_qty <- min(ordbook@bids$qty)
        }
        if(!is.na(sell_price)) {
            ord.buy <- create_order(account=account,
                                venue=venue,
                                stock=ticker,
                                price=buy_price,
                                qty=buy_qty,
                                direction="buy",
                                ordertype="limit")

            placed.buy <- place_order(venue=venue, stock=ticker,
                                       body=ord.buy,
                                      apikey=apikey)
            conbuy <- parse_response(placed.buy)
            
            if(length(conbuy$fills)>0) {
                cat("bought ", conbuy$fills$qty, " shares", "\n")
                position$bought <- position$bought+conbuy$totalFilled
                sell_qty <- conbuy$totalFilled
                ord.sell <- create_order(
                    account=account,
                    venue=venue,
                    stock=ticker,
                    price=sell_price,
                    qty=sell_qty,
                    direction="sell",
                    ordertype="limit")
                placed.sell <- place_order(venue=venue,
                                           stock=ticker,
                                           body=ord.sell,
                                           apikey=apikey)
            
                consell <- parse_response(placed.sell)
                print(consell)
                position$sold <- position$sold+consell$totalFilled
                position$NAV <- position$NAV+
                    (consell$fills$price*consell$totalFilled)-
                    (conbuy$price*conbuy$totalFilled)

            }
            else {
                Sys.sleep(10)
            }
        }
 }       
}, finally=change_instance(level, "stop"))
