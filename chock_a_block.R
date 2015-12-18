source("starfighter.R")
account <- "SCF45213909"
key <- scan("apikey.txt", what="char")
venue <- "GMVEX"
stock <- "TRI"
bid <- 6500
ord <- create_order(account=account, venue=venue, stock=stock, price=bid, qty=1000, direction="buy")
for(i in 1:1000) {
    place_order(venue=venue, stock=stock, body=ord, apikey=key)
    Sys.sleep(10)
}
