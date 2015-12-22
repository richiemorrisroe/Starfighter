library(httr)
base_gm <- "https://www.stockfighter.io/gm/"
apikey <- scan("apikey.txt", what="char")
start_level <- function(level="first_steps") {
    base_gm <- "https://www.stockfighter.io/gm/levels/"
    url <- paste(base_gm, "/",level, sep="")
    res <- httr::POST(url, add_headers("X-Starfighter-Authorization"=apikey))
}
change_instance <- function(level, action) {
    level_data <- content(level)
    instance_id <- level_data[["instanceId"]]
    url <- paste(base_gm, "instances/", instance_id, "/", action, sep="")
    if(action %in% c("stop", "resume")) {
        res <- httr::POST(url=url, add_headers("X-Starfighter-Authorization"=apikey), verbose())
    }
    else {
        res <- httr::GET(url, add_headers("X-Starfighter-Authorization"=apikey), verbose())}
    res
}
level_status <- function(level, ...)  {
    instance <- content(level)[["instanceId"]]
    url <- paste(base_gm, "instances/", instance, sep="")
    res <- httr::GET(url, add_headers("X-Starfighter-Authorization"=apikey), ...)
    
}
get_tickertape <- function(account, venue, ...) {
    base_ws<- "https://www.stockfighter.io/ob/api/"
    url <- paste(base_ws, account, "/venues/", venue, "/tickertape", sep="")
    res <- httr::GET(url, add_headers(api_key=apikey), ...)
    return(res)
}
