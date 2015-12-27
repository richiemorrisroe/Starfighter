source("starfighter.R")
source("starfighter_gm.R")

level <- start_level("sell_side")
tryCatch({
    account <- get_component(level, "account")
    venue <- get_component(level, "venues")
    ticker <- get_component(level, "tickers")
    balance <- get_component(level, "balances")
    position <- data.frame("bought"=0, "sold"=0, "NAV"=0)
    sell_price <- NA
    buy_price <- NA
    max_stock <- 10000*100
    while(NAV>10000) {
        NAV <- position$NAV
        print(NAV)
        current_book <- tryCatch({get_orderbook(venue=venue, stock=ticker)},
                                           error=function(e) return(NA))
        ordbook <- orderbook(parse_response(current_book))
        if(!is.na(ordbook)) {
        if(is.na(ordbook@bids) | is.na(ordbook@asks)) {
            current_book <- 
                    tryCatch(
                    {orderbook(
                         parse_response(get_orderbook(venue=venue, stock=ticker)))},
                    error=function(e) return(NA))
        }
        else {
            sell_price <- min(ordbook@asks$price)-1
            buy_price <- min(ordbook@bids$price)+1
            sell_qty <- max(ordbook@asks$qty)-1
            buy_qty <- max(ordbook@bids$qty)-1
        }
        }
        if(!is.na(sell_price)) {
            ord.buy <- create_order(account=account,
                                venue=venue,
                                stock=ticker,
                                price=buy_price,
                                qty=sell_qty,
                                direction="buy",
                                orderType="limit")

            placed.buy <- place_order(venue=venue, stock=ticker,
                                       body=ord.buy,
                                      apikey=apikey)
            conbuy <- parse_response(placed.buy)
            position$bought <- position$bought+totalFilled
            if(conbuy$totalFilled>0) {
                sell_qty <- conbuy$totalFilled
                ord.sell <- create_order(
                    account=account,
                    venue=venue,
                    stock=ticker,
                    price=sell_price,
                    qty=sell_qty,
                    direction="sell",
                    orderType="limit")
                placed.sell <- place_order(venue=venue,
                                           stock=ticker,
                                           body=ord.sell,
                                           apikey=apikey)
            
                consell <- parse_response(placed.sell)
                position$sold <- position$sold+consell$totalFilled
                position$NAV <- NAV+(consell$price*consell$totalFilled)-(conbuy$price*conbuy$totalFilled)
                
            }
            else {
                Sys.sleep(20)
            }
        }
    }
}, finally=change_instance(level, "stop"))

        
