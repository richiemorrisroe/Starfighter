require(httr)
source("starfighter_gm.R")
source("starfighter.R")
## level <- start_level("chock_a_block")
first <- start_level("sell_side")
contents_level <- content(first)
venue <- unlist(contents_level[["venues"]])
ticker <- unlist(contents_level[["tickers"]])
instance <- unlist(contents_level[["instanceId"]])
monitor <- function(venue, stock) {
    cat("Stock is ", ticker, " venue is ", venue, "\n")
    start_time <- Sys.time()
    ok <- TRUE
    ordlist <- vector(mode="list", length=1e7)
    i <- 1
    tryCatch({
    while(ok) {
        orders <- try(get_orderbook(venue=venue, stock=stock))
        ordlist[[i]] <- orders
        cat("iteration at ", i, "\n")
        i <- i + 1
        ok <- content(orders)$ok
        end_time <- Sys.time()
        cat("time taken ", end_time-start_time, "\n")
        ordlist
    }
    }, finally = {
        cat("reached finally", "\n")
        save(ordlist, file="orders_list_sell_side.rda")
    })
    ordlist
}


mon <- monitor(venue=venue, stock=ticker)
save(mon, file="orders_list_sell_side.rda")
