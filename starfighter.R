require(httr)
base_url <- "https://api.stockfighter.io/ob/api"
get_quote <- function(venue, stock) {
    url <- paste(base_url,  "/venues/", venue, "/stocks/", stock, "/quote", sep="")
    httr::GET(url=url)
}

place_order <- function(venue, stock) {
    
}
