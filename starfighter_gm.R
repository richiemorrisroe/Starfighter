library(httr)
base_url <- "https://www.stockfighter.io/gm/levels/"
apikey <- scan("apikey.txt", what="char")
start_level <- function(level="first_steps") {
    url <- paste(base_url, level, sep="")
    res <- httr::POST(url, add_headers("X-Starfighter-Authorization"=apikey))
}
    
