require(httr)
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
