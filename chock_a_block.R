library(dplyr)
library(lubridate)
source("starfighter.R")
account <- "KAM85332695"
key <- scan("apikey.txt", what="char")
venue <- "EDNNEX"
stock <- "ESIM"
bid <- 8000
ord <- create_order(account=account, venue=venue, stock=stock, price=bid, qty=1000, direction="buy", orderType="fill-or-kill")
for(i in 1:100) {
    place_order(venue=venue, stock=stock, body=ord, apikey=key)
    Sys.sleep(10)
}
qlist <- list()
for(i in 1:100) {
    qlist[[i]] <- get_quote(venue=venue, stock=stock)
}
as.fnumeric <- function(x) {
    as.numeric(as.character(x))
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
