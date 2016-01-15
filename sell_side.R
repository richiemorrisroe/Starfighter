source("starfighter.R")
source("starfighter_gm.R")

tryCatch({
    level <- start_level("sell_side")
    system("Rscript monitor.R \"sell_side\" \"actions\"", wait=FALSE)
    lp <- parse_response(level)
    levok <- TRUE
    venue <- unlist(lp$venue)
    stock <- unlist(lp$tickers)
    account <- unlist(lp$account)
    while(isTRUE(levok)) {
        ls <- level_status(level)
        lsp <- ls %>% parse_response()
        levok <- lsp$ok
        ## print(lsp)
        ob <- get_spreads(venue, stock)
        print(lsp$flash$info)
        trades <- trade(ob, details=lp,qty=40)
        sold.p <- trades$trades$sell
        buy.p <- trades$trades$buy
        allord <- get_all_orders(venue, account) %>% parse_response()
        orders <- allord$orders
        position <- get_position(allord)
        openord <- filter(orders, open==TRUE)
        print(position)
        ob <- trades[[1]]
        if(lsp$state=="lost") {
            break
        }
    }
        
}, finally=change_instance(level, "stop"))
