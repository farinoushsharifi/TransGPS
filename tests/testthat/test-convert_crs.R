context("test-convert_crs")

test_that("conversion works", {

  LatList <- runif(40,-10.8544921875,2.021484375)
  LongList <- runif(40,49.82380908513249,59.478568831926395)
  InitProj <- NextProj <- "+init=epsg:4326"
  result1 <- convert_crs(LatList,LongList,InitProj,NextProj)

  expect_equal(list(result1$Latitude,result1$Longitude),list(LatList,LongList))

  NextProj <- "+init=epsg:4121 +proj=longlat +ellps=GRS80 +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62"
  result2 <- convert_crs(LatList,LongList,InitProj,NextProj)
  result3 <- convert_crs(result2$Latitude,result2$Longitude,NextProj,InitProj)

  expect_equal(list(result3$Latitude,result3$Longitude),
               list(LatList,LongList))

  InitProj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  result4 <- convert_crs(LatList,LongList,InitProj)
  expect_equal(list(result4$Latitude,result4$Longitude),list(LatList,LongList))

  InitProj <- "+c"
  expect_error(convert_crs(LatList,LongList,InitProj,NextProj))

  InitProj <- as.character(NA)
  expect_error(convert_crs(LatList,LongList,InitProj,NextProj))

  InitProj <- NULL
  expect_error(convert_crs(LatList,LongList,InitProj,NextProj))



})
