#' Names of Airports within 'x' Miles of given lon lat coordinates
#'
#' Gets 3-digit airport code of airports within 'x' miles of given point
#'
#' @param lonlat a 1 x 2 matrix with columns 'lon' 'lat', values should be in decimal degrees, (positive for North and East, negative for South and West)
#' @param lonlat_code a string with the airport code associated with lonlat
#' @param lonlat_airport_total a 'n' x 2 matrix with columns 'lon' 'lat', for all airports to be included in the search
#' @param lonlat_airport_codes a vector of length 'n' with the airport names associated with, the lonlat pairs included in lonlat_airport_total
#' @param x the maximum distance an airport can be from given lonlat coordinate pair
#' @examples
#' # get airports within 75 miles of Ithaca, NY
#' ith = airport_data[which(airport_data$codes == 'ITH'),]
#' ll1 = data.frame(lon = ith[1,2], lat = ith[1,3])
#' ll1_codes = ith$codes[1]
#' ll_airport_total = data.frame(lon = airport_data[,2], lat = airport_data[,3])
#' ll_airport_codes = airport_data$codes
#' output = airports_nearby(ll1, ll1_codes, ll_airport_total, ll_airport_codes)
#'
#' @export

airports_nearby <- function(lonlat, lonlat_code, lonlat_airport_total, lonlat_airport_codes, x = 75){
  # use distfun to calculate distances to all airports
  distmat = t(atnPkg::distfun(lonlat,lonlat_airport_total))
  # subset to airports within 'x' miles of lonlat
  ports = lonlat_airport_codes[which(distmat < x)]
  # remove lonlat code from the vector
  ports = ports[!ports %in% lonlat_code]
  # return ports vector
  return( ports )
}

