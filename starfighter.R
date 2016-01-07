require(httr)
require(dplyr)
apikey <- scan("apikey.txt", what="char")
base_url <- "https://api.stockfighter.io/ob/api/"
options(fractional.seconds=7)
##' Return a quote for a given stock from a given venue
##' Note that the quote is valid for a time in the past, and may not reflect current state of the market. Makes a HTTP request to the relevant endpoint. 
##' @title get_quote
##' @param venue where the stock is being traded
##' @param stock which stoc to get quote for
##' @param ... further arguments passed to httr::GET
##' @return a named list with components
##' @author richie
get_quote <- function(venue, stock, ...) {
    url <- paste(base_url,  "venues/", venue, "/stocks/", stock, "/quote", sep="")
    res <- httr::GET(url=url, ...)
    return(res)
}
##' Get the state of the orderbook at a particular venue and for a particular stock
##' This function returns a more granular picture than get_quote, with two dataframes containing price and qty available for both bid (buy) and ask (sell)
##' @title get_orderbook
##' @param venue a particular venue
##' @param stock a particular stock
##' @return a named list with components documented in orderbook-class
##' @author richie
get_orderbook <- function(venue, stock) {
    url <- paste(base_url, "venues/", venue, "/stocks/", stock, sep="")
    res <- httr::GET(url=url)
}
##' Create a named list to be used in a POST request for trading a particular stock
##' Really just provides a shim around creating a list for conversion to JSON
##' @title create_order
##' @param account trading account for this level
##' @param venue venue for the level
##' @param stock the stock to trade
##' @param price price in US cents, must be integer
##' @param qty number of shares to trade
##' @param direction buy or sell
##' @param ordertype one of market, limit, fill-or-kill or immediate-or-cancel
##' @return a named list with components as per parameters to function
##' @author richie
create_order <- function(account, venue, stock, price, qty, direction, ordertype="limit"){
    res <- list(account=account, venue=venue, stock=stock,
                price=price, qty=qty, direction=direction, orderType=ordertype)
}
place_order <- function(venue, stock, body, apikey) {
    url <- paste(base_url, "/venues/", venue, "/stocks/", stock, "/orders", sep="")
    res <- httr::POST(url=url, body=body, encode="json", add_headers("X-Starfighter-Authorization"=apikey))
    
}
##' Convert factor variable to numeric
##'
##' Mostly because I dislike StringsAsFactors=FALSE
##' @title as.fnumeric
##' @param x a factor variable
##' @return a numeric variable
##' @author richie
as.fnumeric <- function(x) {
    as.numeric(as.character(x))
}
##' Misguided parsing function
##' See above
##' @title 
##' @param quote a quote object returned from the Stockfighter API
##' @return 
##' @author richie
parse_quote <- function(quote) {
    quotecontents <- sapply(quote, function(x) content(x))
    numrows <- length(quotecontents)
    numcols <- max(sapply(quotecontents, length))
    resmat <- matrix(data=NA, nrow=numrows, ncol=numcols)
    nameextractors <- tolower(names(quotecontents[[1]]))
    
    for(i in 1:length(nameextractors)) {
        fun <- get_component(component=nameextractors[i])
        browser()
        part <- fun(x=quotecontents)
        resmat[1:length(part),i] <- part
    }
    resmat
}  

get_component <- function(x, component) {
    res <- function (x) x[[component]]
    res
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
##' repeat a function n times, storing the results in a list
##'
##' Should really generalise this to also allow for while loops, as it would simplify the monitor function greatly
##' @title repeat_call
##' @param times A positive integer equal to or greater than 1. The number of times the function is called.
##' @param call a function. Use of function(x) notation will be required for most functions
##' @param sleep number of seconds to sleep between iterations
##' @return a list containing the results of repeatedly evaluating call. 
##' @author richie
repeat_call <- function(times, call, sleep=10) {
    reslist <- vector(mode="list", length=times)
    fun <- match.fun(call)
    for(i in 1:times) {
        reslist[[i]] <- fun()
        if(sleep) {
            Sys.sleep(sleep)
        }
    }
    reslist
}
## as.fnumeric <- function(x) {
##     as.numeric(as.character(x))
## }
##' Get the "current" asking price for a stock
##'
##' This is overcomplicated and conflates the price to pay with 
##' @title 
##' @param quote 
##' @param fudge 
##' @return 
##' @author richie
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
##' More level specific functions!
##' See above
##' @title 
##' @param venue venue
##' @param stock ticker
##' @param fudge how much to increase the price by
##' @return a suggested bid and qty. 
##' @author richie
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
        
##' Parse JSON formatted text responses from the API
##'
##' essentially just calls content() and then jsonlite::fromJSON
##' @title parse_response
##' @param response the results of an API call
##' @return a named list with the components of the response
##' @author richie
parse_response <- function (response) {
    content <- content(response, as="text")
    parsed <- jsonlite::fromJSON(content)

}
##' Helper function for the orderbook objects and functions
##' 
##' @title get_bids
##' @param bid the bid component of an orderbook object
##' @return either NA or a bid object
##' @author richie
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
                      bids="data.frame", asks="data.frame"), contains="list")
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
##' Calculate the spreads between bid and ask from an orderbook object
##'
##' Doesn't really work right now
##' @title spreads.orderbook
##' @param orderbook an orderbook object
##' @return a data.frame containing the prices and differences between them
##' @author richie
spreads.orderbook <- function(orderbook) {
    bids <- orderbook@bids
    asks <- orderbook@asks
    interval <- orderbook@milliseconds
    biddims <- dim(na.omit(bids))
    askdims <- dim(na.omit(asks))
    if(biddims[1]==askdims[1] & askdims[1]>0) {
        spread <- bids-asks
    }
    else {
        
    }
    spread <- NA
}
##' utility function for ensuring that bid/ask dfs do not return null
##' See description
##' @title df_or_null
##' @param order a list based including either a df of bids or asks
##' @param component either bid or ask
##' @return a new dataframe containing NA if component is null, else component
##' @author richie
df_or_null <- function(order, component) {
    if(is.null(order[[component]])) {
        order[[component]] <- data.frame(price=NA, qty=NA, isBuy=NA)
    }
    order
}
        
##' Function that creates a new orderbook object
##' wrapper around a call to "new" that performs null removals and other checks
##' @title orderbook
##' @param order a response from the API as a named list containing all fields or NULL
##' @return an orderbook object
##' @author richie
orderbook <- function(order) {
    order <- df_or_null(order, "bids")
    order <- df_or_null(order, "asks")
    if(is.null(order$error)) {
        tsparsed <- lubridate::ymd_hms(order$ts)
    }
    if(is.null(order$error)) {
    orderbook <- with(order,
         new("orderbook",
             ok=ok,
             venue=venue,
             symbol=symbol,
             ymdhms=tsparsed,
             milliseconds=NA_integer_,
             bids=bids,
             asks=asks))
    orderbook
    }
}
setClass("orderbook-list", contains="orderbook")
as.data.frame.orderbook <- function(orderbook) {
    ordslots <- slotNames("orderbook")[
        2:length(getSlots("orderbook"))]
    ##subtract two for bid and asks, add 3 for the cols of ob@bids/@asks
    totcols <- (length(ordslots))+1
    askdim <- nrow(orderbook@asks)
    biddim <- nrow(orderbook@bids)
    dfs <- rbind(orderbook@bids, orderbook@asks)
    resdim <- as.data.frame(matrix(NA,
                                   nrow=sum(askdim, biddim),
                                   ncol=totcols))
    dfnames <- names(slot(orderbook, "bids"))
    ## browser()
    fullnames <- c(ordslots, dfnames)
    ordslots2 <- ordslots[!(ordslots %in% c("bids", "asks"))]
    names(dfs) <- dfnames
    
    for(i in 1:length(ordslots2)) {
        cat("slot is ", ordslots[i], "\n")
        ## browser()
        current_slot <- rep(slot(orderbook, ordslots2[i]), times=nrow(dfs))
        dfs[,ordslots2[i]] <- current_slot
    }
    dfs
}
##' Creates an object of type quote from a named list returned by parse_response
##'
##' See above
##' @title new_quote
##' @param quote 
##' @return an S4 object of type quote
##' @author richie
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
##' An almost entirely unnecessary parsing function
##'
##' See above
##' @title 
##' @param order an object that has a ts type column
##' @param timestamp the column to convert (needs options(fractional.seconds=7+) to be useful
##' @return a dataframe containing the ymdhms and fractional second components
##' @author richie
parse_ts <- function(order, timestamp) {
    myts <- lubridate::ymd_hms(as.character(order[[timestamp]]))
    split <- unlist(strsplit(as.character(order[[timestamp]]), ".", fixed=TRUE))
    millis <- stringr::str_extract(split[2], "[0-9]+")
    df <- return(data.frame(ymdhms=myts, milli=as.numeric(millis)))

}
get_component <- function(level, component) {
    levelcon <- parse_response(level)
    component <- unlist(levelcon[[component]])
    component
}
##' Return number of stocks traded at a venue
##'
##' See above
##' @title get_tickers
##' @param venue the venue to request stocks for
##' @return a HTTP response containing the requested tickers
##' @author richie
get_tickers <- function(venue) {
    url <- paste(base_url, "venues/", venue, "/stocks/", sep="")
    res <- httr::GET(url, add_headers("X-Starfighter-Authorization"=apikey))
    res
}
##' get the status of an order
##'
##' Only useful for limit/market as ioc or fok return immediately
##' @title get_order_status
##' @param id the id returned from a previous order
##' @param venue the venue
##' @param stock the stock which was in the previous order
##' @return a HTTP response indicating the status of this order
##' @author richie
get_order_status <- function(id, venue, stock) {
    url <- paste(base_url, "venues/", venue, "/stocks/", stock, "/orders/", id, sep="")
    res <- httr::GET(url, add_headers("X-Starfighter-Authorization"=apikey))
    res
}
##' Cancel an outstanding order
##'
##' See above
##' @title cancel_order
##' @param id the order id
##' @param venue the venue
##' @param stock the stock
##' @return a HTTP response indicating the results of the call
##' @author richie
cancel_order <- function(id, venue, stock) {
    url <- paste(base_url, "venues/", venue, "/stocks/", stock, "/orders/", id, sep="")
    res <- httr::DELETE(url, add_headers("X-Starfighter-Authorization"=apikey))
    res
}
##' Get the status of all orders related to a trading account
##' 
##' @title get_all_orders
##' @param venue trading venue
##' @param account current account
##' @return a HTTP response with data (formatted as JSON)
##' @author richie
get_all_orders <- function(venue, account) {
    url <- paste(base_url, "venues/", venue, "/accounts/", account, "/orders/", sep="")
    res <- httr::GET(url, add_headers("X-Starfighter-Authorization"=apikey))
    res
}
##' Convert a list of quote objects to a dataframe
##'
##' Lists of quote objects are returned by the monitor function. 
##' @title qlist_to_df
##' @param quotelist a list of quote objects
##' @return a dataframe containing the quote data, with NA for any value that was NULL in the response
##' @author richie
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
##' Convert a quote object to a vector
##'
##' See above
##' @title as.vector.quote
##' @param quote an object of class quote
##' @return a vector containing the slots of the quote object
##' @author richie
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
##' An attempt at using the websocket endpoints.
##'
##' it appears that httr and curl don't support wss endpoints. I suspect that with some care, an upgrade request can be sent as per the spec (which allows for a HTTP response)
##' @title 
##' @param account 
##' @param venue 
##' @param ... 
##' @return 
##' @author richie
get_tickertape <- function(account, venue, ...) {
    base_ws<- "wss://api.stockfighter.io/ob/"
    url <- paste(base_ws, account, "/venues/", venue, "/tickertape", sep="")
    res <- httr::GET(url, add_headers(api_key=apikey,
                                      "Upgrade"="websocket",
                                      "Connection"="Upgrade",
                                      "Sec-Websocket-Version"="13"), ...)
    return(res)
}
monitor <- function(venue, stock, level=level, name=name) {
    cat("Stock is ", ticker, " venue is ", venue, "\n")
    start_time <- Sys.time()
    ok <- TRUE
    ##this is total overkill, but hey
    ordlist <- vector(mode="list", length=2e5)
    quotelist <- vector(mode="list", length=2e5)
    i <- 1
    tryCatch({
        while(ok) {
            new_time <- Sys.time()
            orders <- try(get_orderbook(venue=venue, stock=stock))
            quote <- get_quote(venue=venue, stock=stock)
            ordlist[[i]] <- orders
            quotelist[[i]] <- quote
            cat("iteration at ", i, "\n")
            i <- i + 1
            ok <- content(orders)$ok
            end_time <- Sys.time()
            cat("time taken ", end_time-new_time, "\n")
            ordlist
            status <- change_instance(level, "")
            browser()
            parsed_status <- parse_response(status)
            print(parsed_status)
            if(parsed_status$state != "open") {
                break
            }
        }
    }, finally = {
        cat("reached finally", "\n")
        ## myorders <- get_all_orders(
        file.ord <- paste("orderlist_", args[1], "_", args[2], ".rda", sep="")
        file.quote <- paste("quotelist_", args[1], "_", args[2], ".rda", sep="")
        save(ordlist, file=file.ord)
        save(quotelist, file=file.quote)
        change_instance(level=level, "stop")
        end_time <- Sys.time()
        cat("Total time taken, ", end_time-start_time, "\n")
    })
    ordlist
}
##' Top level function for buying and selling according to rules
##'
##' See above
##' @title 
##' @param level a level object
##' @param ordertype the type of orders to place
##' @return a list containing the orders placed
##' @author richie
market_make <- function(level, ordertype="limit", qty=NULL) {
    if(level=="TEST") {
        account <- "EXB123456"
        venue <- "TESTEX"
        ticker <- "FOOBAR"
        balance <- NULL
    }
    else {
    account <- get_component(level, "account")
    venue <- get_component(level, "venues")
    ticker <- get_component(level, "tickers")
    balance <- get_component(level, "balances")
    }
    ## browser()
    buys <- NA
    sells <- NA
    prices <- c(buys, sells)
    while(any(is.na(prices))) {
    orders <- get_orderbook(venue, ticker)
    parsed <- orderbook(parse_response(orders))
    if(level!="TEST") {
    status <- level_status(level=level)
    status.p <- parse_response(status)
    if(!is.null(status.p$flash)) {
        flash <- status.p$flash
        print(flash)
    }
    }
    if(is.na(parsed@bids$price)) {
        next
    }
    buys <- ceiling(min(parsed@bids$price))
    sells <- floor(max(parsed@asks$price))
    buy_qty <- floor(min(parsed@bids$qty))
    sell_qty <- floor(min(parsed@asks$qty))
    prices <- c(buys, sells)
    qties <- c(buy_qty, sell_qty)
    cat(prices, "\n")
    }
    directions <- c("buy", "sell")
    if(is.null(qty)) {
        qty <- 1
    }
    reslist <- list()

    
    stat <- level_status(level=level)
    sp <- stat %>% parse_response()
    if(!is.null(sp$flash)) {
    nums <- stringr::str_extract_all(unlist(stat[["flash"]]), "\\$[0-9]+")
    cash <- nums[[1]][1]
    NAV <-  nums[[1]][2]
    }
}
stupid_loop <- function(ordlist) {
    reslist <- vector(mode="list", length=length(ordlist))
    for(i in 1:length(ordlist)) {
        reslist[[i]] <- as.data.frame.orderbook(ordlist[[i]])
    }
    reslist
}
##' Take a series of calls to get_orderbook, and return the results as a df
##'
##' See above. Will document the fields here later
##' @title parse_orderlist
##' @param orderlist a list of calls to get_orderbook 
##' @return a dataframe containing columns titled price, qty, isBuy, ok, venue, symbol, ymdhms (the timestamp) and milliseconds
##' @author richie
parse_orderlist <- function(orderlist) {
    odl2 <- orderlist[sapply(orderlist, function(x) !is.null(x))]
    od.p <- lapply(odl2, parse_response)
    odlok <- od.p[sapply(od.p, function (x) x$ok==TRUE)]
    od.ob <- odlok %>% sapply(., orderbook)
    od.df <- lapply(od.ob, as.data.frame.orderbook)
    res <- do.call("rbind", od.df)
    res
}
get_spreads <- function(venue, stock) {
    bids <- data.frame(price=NA, qty=NA, isBuy=NA)
    asks <- data.frame(price=NA, qty=NA, isBuy=NA)
    nulls <- -1
    spread <- NA
    while(is.na(spread)) {
        nulls <- nulls + 1
        cat("There have been ", nulls, " nulls", "\n")
        orderbook <- get_orderbook(venue=venue, stock=stock)
        ob.p <- orderbook %>% parse_response() %>% orderbook()
        bids <- ob.p@bids
        asks <- ob.p@asks
        spread <- median(asks$price, na.rm=TRUE)-median(bids$price, na.rm=TRUE)
        bidqty <- ob.p@bids$qty
        askqty <- ob.p@asks$qty
        qty <- min(min(bidqty), min(askqty), na.rm=TRUE)
        ob <- list(orderbook=ob.p, spread=spread, minqty=qty)
        
    }
    return(ob)
}
trade <- function(orderbook, details=NULL, qty=NULL) {
    if(details=="TEST") {
        account <- "EXB123456"
        venue <- "TESTEX"
        stock <- "FOOBAR"
    }
    else {
    account <- details$account
    venue <- details$venues
    stock <- details$ticker
    }
    target_spread <- floor(orderbook$spread*0.9)
    ob <- orderbook[[1]]
    buyprice <- median(ob@bids$price) - target_spread
    sellprice <- median(ob@asks$price) + target_spread
    cat("buying at ", buyprice, "\n",
        "Selling at ", sellprice, "\n")
    if(orderbook$minqty<30) {
        qty <- qty
    }
    else {
        qty <- orderbook$minqty
    }
    prices <- c(buyprice, sellprice)
    directions <- c("buy", "sell")
    reslist <- list()
        for(i in 1:length(prices)) {
        ord<- create_order(account=account,
                           venue=venue,
                           stock=stock,
                           price=prices[i],
                           qty=qty,
                           direction=directions[i],
                           ordertype="limit")
    placed <- place_order(venue=venue, stock=stock, body=ord, apikey=apikey)
    reslist[[i]] <- placed %>% parse_response()
    }
    
    names(reslist) <- directions
    ob <- get_spreads(venue, stock)
    return(list(trades=reslist, ob=ob))
}

