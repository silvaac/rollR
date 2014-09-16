context("Apply functions")

x <- xts(1:100,order.by=as.Date("1900-10-10")+1:100)
y <- applyRolling(x,fn=mean,lookback=2)
test_that("Expect value",{
	expect_that(as.numeric(last(y)),equals(0.5*(100+99)))	
	expect_that(is.na(first(y)),is_true())	
})
