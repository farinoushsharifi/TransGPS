context("test-interpolate_coords")

test_that("interpolation works", {

  LatList <- runif(40,-10.8544921875,2.021484375)
  LongList <- runif(40,49.82380908513249,59.478568831926395)
  timeseq <- Sys.time()+runif(40,0,40000)
  timeseq <- as.POSIXct(timeseq)

  result1 <- nrow(interpolate_coords(LatList,LongList,timeseq,timeint=1))
  result2 <- nrow(interpolate_coords(LatList,LongList,timeseq,timeint=3))
  expect_equal(round(result1/result2),3)

  timeint <- as.numeric(max(timeseq)-min(timeseq),unit="secs")+10
  expect_warning(interpolate_coords(LatList,LongList,timeseq,timeint))



})
