require(httr)
require(dplyr)
apikey <- scan("apikey.txt", what="char")
base_url <- "https://api.stockfighter.io/ob/api/"
options(fractional.seconds=7)
get_quote <- function(venue, stock, ...) {
    url <- paste(base_url,  "venues/", venue, "/stocks/", stock, "/quote", sep="")
    res <- httr::GET(url=url, ...)
    return(res)
}
get_orderbook <- function(venue, stock) {
    url <- paste(base_url, "venues/", venue, "/stocks/", stock, sep="")
    res <- httr::GET(url=url)
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
    quotecontents <- sapply(quote, function(x) content(x))
    numrows <- length(quotecontents)
    numcols <- max(sapply(quotecontents, length))
    resmat <- matrix(data=NA, nrow=numrows, ncol=numcols)
    nameextractors <- tolower(names(quotecontents[[1]]))
    
    for(i in 1:length(nameextractors)) {
        fun <- get_component(component=nameextractors[1])
        browser()
        part <- fun(x=quotecontents)
        resmat[1:length(part),i] <- part
    }
    resmat
}    

get_component <- function(x, component) {
    res <- function (x) sapply(x, "[[", component=component)
}
## ok <- get_component(component="ok")
## symbol <- get_component(component="symbol")
## venue <- get_component(component="venue")
## bid <- get_component(component="bid")
## ask <- get_component(component="ask")
## bidsize <- get_component(component="bidSize")
## asksize <- get_component(component="askSize")
## biddepth <- get_component(component="bidDepth")
## askdepth <- get_component(component="askDepth")
## last <- get_component(component="last")
## lastsize <- get_component(component="lastSize")
## lasttrade <- get_component(component="lastTrade")
## quotetime <- get_component(component="quoteTime")

##man i really want this to work
##it doesn't though, suggesting I'm missing something subtle
repeat_call <- function(times, call, sleep=10) {
    reslist <- list()
    fun <- match.fun(call)
    for(i in 1:times) {
        reslist[[i]] <- fun()
        if(sleep) {
            Sys.sleep(sleep)
        }
    }
    reslist
}
as.fnumeric <- function(x) {
    as.numeric(as.character(x))
}

get_bid <- function(quote, fudge=0) {
    ask.cur <- quote$ask
    if(is.null(ask.cur)) {
        fudged_bid <- NA
    }
    else {
        fudged_bid <- floor(ask.cur*(fudge+1))
    }
    askSize <- quote$askSize
    if(is.null(askSize)) {
        qty <- NA
    }
    else {
        qty <- floor(askSize/100)
    }
    state <- c(fudged_bid, qty)
    
}

get_first_real_price <- function (venue, stock, fudge=0) {
    fudged_bid <- NA
    qty <- NA
    while(is.na(fudged_bid) | is.na(qty)) {
        orderinfo <- get_bid(content(get_quote(venue, stock)))
        fudged_bid <- orderinfo[1]
        qty <- orderinfo[2]
    }
    res <- c(fudged_bid, qty)
}
        
## test.df2  <- test.df %>% mutate(bid=as.fnumeric(bid),
##                                 ask=as.fnumeric(ask),
##                                 bidsize=as.fnumeric(bidSize),
##                                 asksize=as.fnumeric(askSize),
##                                 biddepth=as.fnumeric(bidDepth),
##                                 askdepth=as.fnumeric(askDepth),
##                                 last=as.fnumeric(ask),
##                                 lasttrade=ymd_hms(lastTrade),
##                                 quotetime=ymd_hms(quoteTime))
parse_response <- function (response) {
    content <- content(response, as="text")
    parsed <- jsonlite::fromJSON(content)

}
get_bids <- function(bid) {
    if(is.null(bid)) {
        return(NA)
    }
    else {
        return(bid)
    }
}
response_to_df <- function(parsed_response) {
    parsedmat <- do.call("rbind", parsed_response)
    parsed.df <- sapply(as.data.frame(parsedmat), unlist)
    parsed.df
}
get_tickertape <- function(account, venue) {
    ##sadface, Curl (and thus httr) doesn't support websockets
    base_url_wss <- "wss://api.stockfighter.io/ob/api/ws/"
    url <- paste(base_url_wss, account, "/venues/", venue, "/tickertape", sep="")
    res <- httr::GET(url, add_headers("X-Starfighter-Authorization"=apikey))
}
setClass("orderbook",
         slots = list(ok="logical", venue="character",
                      symbol="character", ymdhms="POSIXt",
                      milliseconds="numeric",
                      bids="data.frame", asks="data.frame"))
setClass("quote",
         slots=list(ok="logical",
                    venue="character",
                    symbol="character",
                    bid="integer",
                    bidSize="integer",
                    askSize="integer",
                    bidDepth="integer",
                    askDepth="integer",
                    last="integer",
                    lastSize="integer",
                    lastTrade="character",
                    quoteTime="character"),
         prototype=prototype(ok=NA,
                    venue=NA_character_,
                    symbol=NA_character_,
                    bid=NA_integer_,
                    bidSize=NA_integer_,
                    askSize=NA_integer_,
                    bidDepth=NA_integer_,
                    askDepth=NA_integer_,
                    last=NA_integer_,
                    lastSize=NA_integer_,
                    lastTrade=NA_character_,
                    quoteTime=NA_character_),
         contains="vector")
spreads.orderbook <- function(orderbook) {
    bids <- orderbook@bids
    asks <- orderbook@asks
    interval <- orderbook@milliseconds
    biddims <- dim(na.omit(bids))
    askdims <- dim(na.omit(asks))
    if(biddims[1]==askdims[1] & askdims[1]>0) {
    spread <- data.frame(bid_price=bids$price,
                         bid_qty=bids$qty,
                         ask_price=asks$price,
                         ask_qty=asks$qty) %>%
        dplyr::mutate(diff_price=ask_price-bid_price)
    }
    else {
        spread <- data.frame(bid_price=NA,
                             bid_qty=NA,
                             ask_price=NA,
                             ask_qty=NA,
                             diff_price=NA)
    }
    spread
}
df_or_null <- function(order, component) {
    if(is.null(order[[component]])) {
        order[[component]] <- data.frame(price=NA, qty=NA, isBuy=NA)
    }
    order
}
        
orderbook <- function(order) {
    order <- df_or_null(order, "bids")
    order <- df_or_null(order, "asks")
    if(is.null(order$error)) {
        tsparsed <- parse_ts(order)
    }
    if(is.null(order$error)) {
    orderbook <- with(order,
         new("orderbook",
             ok=ok,
             venue=venue,
             symbol=symbol,
             ymdhms=tsparsed[,1],
             milliseconds=tsparsed[,2],
             bids=bids,
             asks=asks))
    orderbook
    }
}
## obj_or_null <- function (object, data) {
##     ifelse(
quote_fields <- function(quote) {}
new_quote <- function(quote) {
    q <- new("quote")
    quote2 <- lapply(quote, function(x) ifelse(is.null(x), NA, x))
    q@ok <- ifelse(!is.null(quote2$ok), quote2$ok, q@ok)
    q@venue <- ifelse(!is.null(quote2$venue), quote2$venue, q@venue) 
    q@symbol <- ifelse(!is.null(quote2$symbol), quote2$symbol, q@symbol) 
    q@bid <- ifelse(!is.null(quote2$bid), quote2$bid, q@bid) 
    q@bidSize <- ifelse(!is.null(quote2$bidSize), quote2$bidSize, q@bidSize)
    q@askSize <- ifelse(!is.null(quote2$askSize), quote2$askSize, q@askSize)
    q@bidDepth <- ifelse(!is.null(quote2$bidDepth), quote2$bidDepth, q@bidDepth)
    q@askDepth <- ifelse(!is.null(quote2$askDepth), quote2$askDepth, q@askDepth)
    q@last <- ifelse(!is.null(quote2$last), quote2$last, q@last)
    q@lastSize <- ifelse(!is.null(quote2$lastSize), quote2$lastSize, q@lastSize)
    q@lastTrade <- ifelse(!is.null(quote2$lastTrade), quote2$lastTrade, q@lastTrade)
    q@quoteTime <- ifelse(!is.null(quote2$quoteTime), quote2$quoteTime, q@quoteTime)
    q
}
## orderbook.t <- list()
## for(i in 1:length(quotes.parsed2)) {
##     print(i)
##     orderbook.t[[i]] <- new_quote(quotes.parsed[[i]])
## }
parse_ts <- function(order, timestamp) {
    myts <- lubridate::ymd_hms(as.character(order[[timestamp]]))
    split <- unlist(strsplit(as.character(order[[timestamp]]), ".", fixed=TRUE))
    millis <- stringr::str_extract(split[2], "[0-9]+")
    browser()
    df <- return(data.frame(ymdhms=myts, milli=as.numeric(millis)))

}
get_component <- function(level, component) {
    levelcon <- parse_response(level)
    component <- unlist(levelcon[[component]])
    component
}
get_tickers <- function(venue) {
    url <- paste(base_url, "venues/", venue, "/stocks/", sep="")
    res <- httr::GET(url, add_headers("X-Starfighter-Authorization"=apikey))
    res
}
get_order_status <- function(id, venue, stock) {
    url <- paste(base_url, "venues/", venue, "/stocks/", stock, "/orders/", id, sep="")
    res <- httr::GET(url, add_headers("X-Starfighter-Authorization"=apikey))
    res
}
cancel_order <- function(id, venue, stock) {
    url <- paste(base_url, "venues/", venue, "/stocks/", stock, "/orders/", id, sep="")
    res <- httr::DELETE(url, add_headers("X-Starfighter-Authorization"=apikey))
    res
}
get_all_orders <- function(venue, account) {
    url <- paste(base_url, "venues/", venue, "/accounts/", account, "/orders/", sep="")
    res <- httr::GET(url, add_headers("X-Starfighter-Authorization"=apikey))
    res
}
qlist_to_df <- function(quotelist) {
    stopifnot(class(quotelist[[1]])=="quote")
    matrows <- length(quotelist)
    matcols <- length(getSlots("quote"))-1
    resmat <- matrix(data=NA, nrow=matrows, ncol=matcols)
    for(i in 1:length(quotelist)) {
        resmat[i,] <- as.vector.quote(quotelist[[i]])
    }
    resmat
    colnames(resmat) <- names(getSlots("quote")[2:length(getSlots("quote"))])
    resdf <- as.data.frame(resmat)
    resdf <- dplyr::mutate(resdf, ok=as.logical(ok),
                    bid=as.fnumeric(bid),
                    bidSize=as.fnumeric(bidSize),
                    askSize=as.fnumeric(askSize),
                    bidDepth=as.fnumeric(bidDepth),
                    last=as.fnumeric(last),
                    lastSize=as.fnumeric(lastSize),
                    last_trade=lubridate::ymd_hms(lastTrade),
                    quote_time=lubridate::ymd_hms(quoteTime))
    resdf
                    
                                 
}
as.vector.quote <- function(quote) {
    res <- c(quote@ok,
    quote@venue ,
    quote@symbol ,
    quote@bid ,
    quote@bidSize ,
    quote@askSize ,
    quote@bidDepth ,
    quote@askDepth ,
    quote@last ,
    quote@lastSize ,
    quote@lastTrade ,
    quote@quoteTime )
}
get_tickertape <- function(account, venue, ...) {
    base_ws<- "https://www.stockfighter.io/ob/api/"
    url <- paste(base_ws, account, "/venues/", venue, "/tickertape", sep="")
    res <- httr::GET(url, add_headers(api_key=apikey), ...)
    return(res)
}
