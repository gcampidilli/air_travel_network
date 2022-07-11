test_that( "Correct airports within 'x' miles of selected airport", {

  ith = airport_data[which(airport_data$codes == 'ITH'),]
  ll1 = data.frame(lon = ith[1,2], lat = ith[1,3])
  ll1_codes = ith$codes[1]
  ll_airport_total = data.frame(lon = airport_data[,2], lat = airport_data[,3])
  ll_airport_codes = airport_data$codes
  output = airports_nearby(ll1, ll1_codes, ll_airport_total, ll_airport_codes)

  expect_equal(
    output,
    c("BGM", "ELM", "SYR")
  )
})
