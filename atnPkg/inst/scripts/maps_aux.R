library(fields)
load('data/airport_data.RData')
load('data/direct_from.RData')
devtools::load_all()

# Get unique airports ID
airports = airport_data$codes
n_airports = length(airports)

# Code that defines a grid of points covering the continental USA and figures out which points are inside the continental USA.
# Get the polygon defining the continental USA
library("maps")
library("sp")
usa_poly <- map("usa")
usa_poly$x <- c(NA,usa_poly$x,NA)
usa_poly$y <- c(NA,usa_poly$y,NA)
nai <- which( is.na( usa_poly$x ) )

# Define a grid of longitude and latitude points
n1 <- 180
n2 <- 90
lo <- seq(usa_poly$range[1], usa_poly$range[2], length.out = n1)
la <- seq(usa_poly$range[3], usa_poly$range[4], length.out = n2)
lonlat_grid <- as.matrix( expand.grid( lo, la ) )

# Figure out which points are inside USA
in_usa <- rep(FALSE, nrow(lonlat_grid))
for(j in 1:(length(nai)-1)){
  in_this <- sp::point.in.polygon(
    lonlat_grid[,1],
    lonlat_grid[,2],
    usa_poly$x[ (nai[j]+1):(nai[j+1]-1) ],
    usa_poly$y[ (nai[j]+1):(nai[j+1]-1) ]
  )
  in_usa <- in_usa | in_this
}

# Subset to the points in USA
lonlat_usa <- as.matrix( lonlat_grid[ in_usa, ] )
# Naming
colnames(lonlat_usa) = c('lon', 'lat')
lonlat_usa = data.frame(lonlat_usa)
# Number grid points
n_points = nrow(lonlat_usa)

# 5/13/22 Notes
# -------------
# Sequence of actions
#   (1) Find all grid points within `2x` distance of grid point
#   (2) Find all airports within `x` distance of grid point
#   (3) Find all connecting airports
#         -> `direct_from` object
#   (4) Find all points within connecting airport
#         -> object made with Grace in Upson;
#   (5) Calculate proportion of unique grid points reachable

compute_points_within = function(x){
  points_within = vector("list", length = n_points)
  for(point in 1:n_points){
    x1 = lonlat_usa[point,]
    x2 = lonlat_usa
    dists = rdist.earth(x1, x2, miles = TRUE)
    within = which(dists <= x)
    points_within[[point]] = within
  }
  return(points_within)
}

compute_airports_within = function(x){
  airports_within = vector("list", length = n_points)
  for(point in 1:n_points){
    x1 = lonlat_usa[point,]
    x2 = airport_data[, c(2, 3)]
    dists = rdist.earth(x1, x2, miles = TRUE)
    within = which(dists <= x)
    airports_within[[point]] = airport_data[within, 'codes']
  }
  return(airports_within)
}

# (1) All grid points within `2x` distance of grid point
points_within_150 = compute_points_within(150)
points_within_200 = compute_points_within(200)
# save(points_within_150, file = 'data/points_within_150.RData')
# save(points_within_200, file = 'data/points_within_200.RData')
# (2) All airports within `x` distance of grid point
airports_within_75 = compute_airports_within(75)
airports_within_100 = compute_airports_within(100)
# save(airports_within_75, file = 'data/airports_within_75.RData')
# save(airports_within_100, file = 'data/airports_within_100.RData')

# (3) All connecting airports
#       -> `direct_from` object; and calculated already
#       -> How to load into this script?

# (4) All points reachable within `x` distance of airports

compute_points_within_airport = function(x){
  points_within_airport = vector("list", length = n_airports)
  names(points_within_airport) = airports
  for(airport in airports){
    airport_idx = which(airport_data$codes == airport)
    x1 = airport_data[airport_idx, c(2, 3)]
    x2 = lonlat_usa
    dists = rdist.earth(x1, x2, miles = TRUE)
    within = which(dists <= x)
    points_within_airport[[airport]] = within
  }
  return(points_within_airport)
}

points_within_airport_75 = compute_points_within_airport(75)
points_within_airport_100 = compute_points_within_airport(100)
# save(points_within_airport_75, file = 'data/points_within_airport_75.RData')
# save(points_within_airport_100, file = 'data/points_within_airport_100.RData')

compute_proportion = function(points_within_2x, airports_within_x,
                              points_within_airport_x, direct_from){
  # Hard encoding; `n_points`
  #   -> Later compute from length of arguments
  n_points = length(points_within_2x)
  proportions = numeric(n_points)
  for(point in 1:n_points){
    # Reachbale gridpoints from gridpoint
    reachable_points = numeric()
    # (1) Points within `2x` miles of grid point
    points_within = points_within_2x[[point]]
    reachable_points = append(points_within, reachable_points)
    # (2) Airports within `x` miles of grid point
    airports_within = airports_within_x[[point]]
    for(airport in airports_within){
      # (3) Direct flights from airport
      direct_flights = direct_from[[airport]]
      for(flight in direct_flights){
        # (4) Reachable gridpoints `x` miles from airport
        points_within_airport = points_within_airport_x[[flight]]
        reachable_points = append(reachable_points, points_within_airport)
      }
    }
    # (5) Unique gridpoints reachable
    reachable_points = unique(reachable_points)
    # (5) Proportion of gridpoints reachable from gridpoint
    proportion = length(reachable_points) / n_points
    proportions[point] = proportion
  }
  return(proportions)
}

average_proportion_75 = compute_proportion(points_within_150, airports_within_75,
                                           points_within_airport_75, direct_from)
average_proportion_100 = compute_proportion(points_within_200, airports_within_100,
                                            points_within_airport_100, direct_from)


