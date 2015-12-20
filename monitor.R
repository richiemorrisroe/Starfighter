require(httr)
level <- start_level("chock_a_block")
contents_level <- content(level)
venue <- contents_level$venue
ticker <- contents_level$venue
monitor <- function(venue, stock) {
    ok <- TRUE
    while(ok) {
    orders <- get_orderbook(venue, stock)
    ordlist[[i]] <- orders
    i <- i + 1 
    ok <- content(orders)$ok
    
    }
    save(ordlist, file="orders_list.rda")
}
    
