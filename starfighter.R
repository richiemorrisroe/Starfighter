require(httr)
require(dplyr)
apikey <- scan("apikey.txt", what="char")
base_url <- "https://api.stockfighter.io/ob/api"
get_quote <- function(venue, stock) {
    url <- paste(base_url,  "/venues/", venue, "/stocks/", stock, "/quote", sep="")
    httr::GET(url=url)
}
create_order <- function(account, venue, stock, price, qty, direction, orderType="limit"){
    res <- list(account=account, venue=venue, stock=stock,
                price=price, qty=qty, direction=direction, orderType=orderType)
}
place_order <- function(venue, stock, body, apikey) {
    url <- paste(base_url, "/venues/", venue, "/stocks/", stock, "/orders", sep="")
    res <- httr::POST(url=url, body=body, encode="json", add_headers("X-Starfighter-Authorization"=apikey), verbose())
    
}
as.fnumeric <- function(x) {
    as.numeric(as.character(x))
}

parse_quote <- function(quote) {
    quotecontents <-
        sapply(quote, content) %>%
        sapply(., unlist)  %>%
        do.call("rbind", .)
}
    
test.df2  <- test.df %>% mutate(bid=as.fnumeric(bid),
                                ask=as.fnumeric(ask),
                                bidsize=as.fnumeric(bidSize),
                                asksize=as.fnumeric(askSize),
                                biddepth=as.fnumeric(bidDepth),
                                askdepth=as.fnumeric(askDepth),
                                last=as.fnumeric(ask),
                                lasttrade=ymd_hms(lastTrade),
                                quotetime=ymd_hms(quoteTime))
