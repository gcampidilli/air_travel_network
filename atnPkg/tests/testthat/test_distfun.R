test_that( "Distance matrix looks okay", {
  ll1 = data.frame(lon = airport_data[c(1:5),2], lat = airport_data[c(1:5),3])
  ll2 = data.frame(lon = airport_data[c(6:8),2], lat = airport_data[c(6:8),3])
  output = distfun(ll1,ll2)

  expect_equal(
    dim(output),
    c(5,3)
  )

})
