context("test-get_boxes")

test_that("creating boxes works", {

  LatList <- runif(40,-10.8544921875,2.021484375)
  LongList <- runif(40,49.82380908513249,59.478568831926395)
  timeseq <- Sys.time()+runif(40,0,40000)
  timeseq <- as.POSIXct(timeseq)

  disttbl <- compute_distance(LatList,LongList,timeseq)
  totaldist <- disttbl$CumulativeDistance.km[nrow(disttbl)]
  expect_warning(get_boxes(LatList,LongList,timeseq,resolution = totaldist+0.01))

  meandist <- mean(disttbl$Distance.km)
  expect_warning(get_boxes(LatList,LongList,timeseq,resolution = meandist))

  result1 <- get_boxes(LatList,LongList,timeseq,resolution = max(disttbl$Distance.km))
  expect_equal(ncol(result1$boxlist),nlevels(factor(result1$boxtable$boxcuts)))

})
