context("test-match_highway")

test_that("matching to highway works", {

  LatList <- runif(100,-10.8544921875,2.021484375)
  LongList <- runif(100,49.82380908513249,59.478568831926395)
  timeseq <- Sys.time()+runif(100,0,40000)
  timeseq <- as.POSIXct(timeseq)

  boxcuts <- round(runif(100,1,20))

  expect_warning(match_highway(LatList,LongList,timeseq,k=5,boxcuts = boxcuts))

  result1 <- match_highway(LatList,LongList,timeseq,k=2)
  expect_equal(length(result1[[1]]),length(LatList))
})
