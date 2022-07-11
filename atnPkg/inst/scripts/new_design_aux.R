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

check_A = function(direct_from_a, direct_from){
  nconnections_a = sum(sapply(direct_from_a, length))
  nconnections = sum(sapply(direct_from, length))
  return(nconnections == nconnections_a)
}

check_B = function(direct_from_b, direct_from){
  connections_b = sapply(direct_from_b, length)
  connections = sapply(direct_from, length)
  return(all.equal(connections, connections_b))
}

check_C = function(direct_from_c, direct_from){
  passes = TRUE
  airport_names = names(direct_from)
  for(airport in airport_names){
    s1 = direct_from_c[[airport]]
    s2 = direct_from[[airport]]
    c1 = (abs(length(s1) - length(s2))) <= 2
    c2 = (length(intersect(s2, s1))) >= (length(s2) - 2)
    if(!c1 & c2){
      passes = FALSE
    }
  }
  return(passes)
}
