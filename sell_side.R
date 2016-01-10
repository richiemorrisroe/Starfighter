source("starfighter.R")
source("starfighter_gm.R")

tryCatch({
    level <- start_level("sell_side")
    
    lp <- parse_response(level)
    levok <- TRUE
    while(isTRUE(levok)) {
        ls <- level_status(level)
        lsp <- ls %>% parse_response()
        levok <- lsp$ok
        print(lsp)
        venue <- unlist(lp$venue)
        stock <- unlist(lp$tickers)
        ob <- get_spreads(venue, stock)
        print(lsp$flash$info)
        trades <- trade(ob, details=lp, qty=50)
        ob <- trades[[1]]
        Sys.sleep(5)
    }
        
}, finally=change_instance(level, "stop"))
