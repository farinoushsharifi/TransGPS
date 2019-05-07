context("test-match_highway")

test_that("matching to highway works", {

  LatList <- runif(100,31.675141,31.685723)
  LongList <- runif(100,-106.329522,-106.326892)
  timeseq <- Sys.time()+runif(100,0,40000)
  timeseq <- as.POSIXct(timeseq)
  boxcuts <- round(runif(100,1,10))
  expect_error(match_highway(LatList,LongList,timeseq,k=5,boxcuts = boxcuts))

  result1 <- match_highway(LatList,LongList,timeseq,k=2,resolution = 5)
  expect_equal(length(result1),length(LatList))

  resultbox <- get_boxes(LatList,LongList,timeseq,resolution = 5)
  boxlist <- resultbox$boxlist
  boxtable <- resultbox$boxtable[sample(nrow(resultbox$boxtable)),]
  boxcuts <- boxtable$boxcuts
  LatList <- boxtable$Latitude
  LongList <- boxtable$Longitude
  timeseq <- boxtable$DateTime

  result2 <- match_highway(LatList,LongList,timeseq,k=2,
                           boxcuts = boxcuts,boxlist = boxlist)

  expect_equal(result1,result2)


})
