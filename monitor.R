require(httr)
require(methods)
source("starfighter_gm.R")
source("starfighter.R")
args <- commandArgs(TRUE)
## level <- start_level("chock_a_block")
first <- start_level(args[1])
contents_level <- content(first)
venue <- unlist(contents_level[["venues"]])
ticker <- unlist(contents_level[["tickers"]])
instance <- unlist(contents_level[["instanceId"]])
monitor <- function(venue, stock) {
    cat("Stock is ", ticker, " venue is ", venue, "\n")
    start_time <- Sys.time()
    ok <- TRUE
    ##this is total overkill, but hey
    ordlist <- vector(mode="list", length=1e6)
    quotelist <- vector(mode="list", length=1e6)
    i <- 1
    tryCatch({
    while(ok) {
        orders <- try(get_orderbook(venue=venue, stock=stock))
        quote <- get_quote(venue=venue, stock=stock)
        ordlist[[i]] <- orders
        quotelist[[i]] <- quote
        cat("iteration at ", i, "\n")
        i <- i + 1
        ok <- content(orders)$ok
        end_time <- Sys.time()
        cat("time taken ", end_time-start_time, "\n")
        ordlist
    }
    }, finally = {
        cat("reached finally", "\n")
        file.ord <- paste("orderlist_", args[1], ".rda", sep="")
        file.quote <- paste("quotelist_", args[1], ".rda", sep="")
        save(ordlist, file=file.ord)
        save(quotelist, file=file.quote)
        change_instance(level=first, "stop")
    })
    ordlist
}

mon <- monitor(venue=venue, stock=ticker)
                

