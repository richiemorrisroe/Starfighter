source("starfighter.R")
source("starfighter_gm.R")

tryCatch({
    level <- start_level("sell_side")
    
    lp <- parse_response(level)
    levok <- TRUE
    while(isTRUE(lp$ok)) {
        ls <- level_status(level)
        browser()
        lsp <- ls %>% parse_response()
        levok <- lsp$ok
        print(lsp)
        venue <- unlist(lp$venue)
        stock <- unlist(lp$tickers)
        ob <- get_spreads(venue, stock)
        print(ob)
        trades <- trade(ob, details=lp)
        ob <- trades[[1]]
        Sys.sleep(5)
    }
    ## if(content(level)$ok==TRUE) {

    ## account <- get_component(level, "account")
    ## venue <- get_component(level, "venues")
    ## ticker <- get_component(level, "tickers")
    ## balance <- get_component(level, "balances")
    ## position <- data.frame("bought"=0, "sold"=0, "NAV"=0)
    ## bill_position <- NA
    ## sell_price <- NA
    ## buy_price <- NA
    ## max_stock <- 1000
    ## NAV <- position$NAV
    ## stocks <- get_tickers(venue=venue)
    ## print(parse_response(stocks))
    ## }
    ## else {
    ##     Sys.sleep(60)
    ##     level <- start_level("sell_side")
    ## }
    ## while(NAV<10000) {
    ##     NAV <- 0
    ##     bids <- NULL
    ##     asks <- NULL
    ##     while(is.null(bids) | is.null(asks)) {
    ##         orderbook <- get_orderbook(venue=venue, stock=stock)
    ##         ob.p <- orderbook(orderbook) %>% parse_response()
    ##         bids <- ob.p@bids
    ##         asks <- ob.p@asks
    ##         spread <- median(asks$price)median(bids$price)
    ##         ob <- list(orderbook=od.p, spread=spread)
    ##     }
    ##         sell_price <- min(ob.p@asks$price)+100
    ##         buy_price <- min(ob.p@bids$price)+100
    ##         sell_qty <- min(ordbook@asks$qty)
    ##         buy_qty <- min(ordbook@bids$qty)
    ##     market <- market_make(level=level, qty=100)
    ##     sold <- market$sell
    ##     bought <- market$bought
    ##         if(length(conbuy$fills)>0) {
    ##             cat("bought ", conbuy$fills$qty, " shares", "\n")
    ##             position$bought <- position$bought+conbuy$totalFilled
    ##             sell_qty <- conbuy$totalFilled
    ##             ord.sell <- create_order(
    ##                 account=account,
    ##                 venue=venue,
    ##                 stock=ticker,
    ##                 price=sell_price,
    ##                 qty=sell_qty,
    ##                 direction="sell",
    ##                 ordertype="limit")
    ##             placed.sell <- place_order(venue=venue,
    ##                                        stock=ticker,
    ##                                        body=ord.sell,
    ##                                        apikey=apikey)
            
    ##             consell <- parse_response(placed.sell)
    ##             print(consell)
    ##             position$sold <- position$sold+consell$totalFilled
    ##             position$NAV <- position$NAV+
    ##                 (consell$fills$price*consell$totalFilled)-
    ##                 (conbuy$price*conbuy$totalFilled)

    ##         }
    ##         else {
    ##             Sys.sleep(10)
    ##         }
    ##     }
        
}, finally=change_instance(level, "stop"))
