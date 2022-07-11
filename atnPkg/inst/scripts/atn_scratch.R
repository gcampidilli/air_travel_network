
# user must specify latitude and longitude coordinates

lonlat1 = airport_data[1,c(2:3)]
longlat1_code = airport_data[1,1]
lonlat2 = airport_data[2,c(2:3)]
lonlat2_code = airport_data[2,1]


ith = airport_data[which(airport_data$codes == 'ITH'),]
ll1 = data.frame(lon = ith[1,2], lat = ith[1,3])
ll1_codes = ith$codes[1]
ll_airport_total = data.frame(lon = airport_data[,2], lat = airport_data[,3])
ll_airport_codes = airport_data$codes

devtools::load_all()

devtools::test()

# distance between ABE and ABI airports
distfun(lonlat1, longlat1_code, lonlat2, lonlat2_code)

# airports within 75 miles of Ithaca
airports_nearby(ll1, ll1_codes, ll_airport_total, ll_airport_codes)


devtools::document()

?distfun
?airports_nearby
devtools::run_examples()


