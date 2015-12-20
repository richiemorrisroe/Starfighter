require(httr)
require(dplyr)
apikey <- scan("apikey.txt", what="char")
base_url <- "https://api.stockfighter.io/ob/api"
get_quote <- function(venue, stock) {
    url <- paste(base_url,  "/venues/", venue, "/stocks/", stock, "/quote", sep="")
    res <- httr::GET(url=url)
    return(res)
}
get_orderbook <- function(venue, stock) {
    url <- paste(base_url, "/venues/", venue, "/stocks/", stock, sep="")
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
ok <- get_component(component="ok")
symbol <- get_component(component="symbol")
venue <- get_component(component="venue")
bid <- get_component(component="bid")
ask <- get_component(component="ask")
bidsize <- get_component(component="bidSize")
asksize <- get_component(component="askSize")
biddepth <- get_component(component="bidDepth")
askdepth <- get_component(component="askDepth")
last <- get_component(component="last")
lastsize <- get_component(component="lastSize")
lasttrade <- get_component(component="lastTrade")
quotetime <- get_component(component="quoteTime")

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
