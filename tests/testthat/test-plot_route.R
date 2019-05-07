context("test-plot_route")

test_that("plotting works", {
  LatList <- runif(40,-10.8544921875,2.021484375)
  LongList <- runif(40,49.82380908513249,59.478568831926395)
  timeseq <- Sys.time()+runif(40,0,40000)
  timeseq <- as.POSIXct(timeseq)

  boxlist <- NULL
  expect_error(plot_route(LatList,LongList,timeseq, boxlist))

  boxlist <- matrix(c(1,2,3,4),nrow=1,ncol=4)
  expect_error(plot_route(LatList,LongList,timeseq, boxlist))

})
