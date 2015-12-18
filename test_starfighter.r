require(testthat)
source("starfighter.R")
venue <- "TESTEX"
stock <- "FOOBAR"
apikey <- scan("apikey.txt", what="char")
qty <- 100
price <- 100
direction <- "sell"
quote <- get_quote("TESTEX", "FOOBAR")
ord <- create_order(account="EXB123456", venue=venue, stock=stock, qty=qty, price=price, direction=direction, orderType="limit")
res <- place_order(venue, stock, body=ord, apikey=apikey)
##fuck tests for now
## test_that("we can order a test stock", code={
##     expect_TRUE(
