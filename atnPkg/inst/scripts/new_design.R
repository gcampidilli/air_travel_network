library(fields)
load('data/airport_data.RData')
load('data/direct_from.RData')
# Import `points_within`
load('data/points_within_150.RData')
load('data/points_within_200.RData')
# Import `airports_within`
load('data/airports_within_75.RData')
load('data/airports_within_100.RData')
# Import `points_within_airport`
load('data/points_within_airport_75.RData')
load('data/points_within_airport_100.RData')
devtools::load_all()

# 4(a)
# ----------

redesign_A = function(points_within_2x, airports_within_x, points_within_airport_x,
                      direct_from, batch_size, n_iters=100){
  # Compute benchmark
  benchmark_proportion = mean(compute_proportion(points_within_2x, airports_within_x,
                                            points_within_airport_x, direct_from))
  print(benchmark_proportion)
  # Initialize candidate direct_from initial direct_from
  candidate_direct_from = direct_from
  # Initialize redesigned direct_from from initial direct_from
  redesigned_direct_from = direct_from
  # Initialize counter for patience threshold and 'Early Stopping'
  for(i in 1:n_iters){
    for(j in 1:batch_size){
      # Define pool of airports to sample from and to modify
      #   -> Condition: airports to modify have at least one connection
      airports = names(candidate_direct_from[sapply(candidate_direct_from, length) > 0])
      airport_1 = sample(airports, 1)
      # Outgoing
      airport_1_dests = candidate_direct_from[[airport_1]]
      # Connections not already reachable by airport;
      #   -> Further: `airport` not in; can't go to itself
      unreachable_dests = airports[(!airports %in% airport_1_dests) & (airports != airport_1)]
      # Sample random destination to remove
      airport_to_remove = sample(airport_1_dests, 1)
      # Sample random destination to add
      airport_to_add = sample(unreachable_dests, 1)
      # The index in which the destination to remove occurs
      airport_to_remove_idx = which(airport_1_dests == airport_to_remove)
      # Replace 'airport_to_remove' from 'airport_1's connections with
      # `airport_to_add`
      airport_1_dests[airport_to_remove_idx] = airport_to_add
      # Update value in candidate object
      candidate_direct_from[[airport_1]] = airport_1_dests
      # Update destinations from removed airport, and added airport
      #   -> Remove 'airport_1' from 'airport_to_remove' destinations
      airport_to_remove_dests = candidate_direct_from[[airport_to_remove]]
      airport_to_remove_dests = airport_to_remove_dests[-which(airport_to_remove_dests == airport_1)]
      # Update in 'candidate_direct_from' object
      candidate_direct_from[[airport_to_remove]] = airport_to_remove_dests
      #   -> Add 'airport_1' to 'airport_to_add' destinations
      airport_to_add_dests = candidate_direct_from[[airport_to_add]]
      airport_to_add_dests = append(airport_to_add_dests, airport_1)
      # Update in 'candidate_direct_from' object
      candidate_direct_from[[airport_to_add]] = airport_to_add_dests
    }
    # Compute proportion for new `direct_from` object
    #   -> If it is better than benchmark proportion; replace
    candidate_proportion = mean(compute_proportion(points_within_2x, airports_within_x,
                                              points_within_airport_x,
                                              candidate_direct_from))
    if(candidate_proportion > benchmark_proportion){
      print(candidate_proportion)
      # Update redeisgned object with current object with better proportion
      redesigned_direct_from = candidate_direct_from
      # Update benchmark proportion with current proportion
      benchmark_proportion = candidate_proportion
    }
    # Revert candidate_direct_from to most recent improved object
    candidate_direct_from = redesigned_direct_from
  }
  proportions = compute_proportion(points_within_2x, airports_within_x,
                                   points_within_airport_x,
                                   redesigned_direct_from)
  return(list('direct_from' = redesigned_direct_from, 'proportions' = proportions))
}

# redesign_A75 = redesign_A(points_within_150, airports_within_75,
#                                points_within_airport_75,
#                                direct_from, 32, 100)
# check_symmetric(redesign_A75[['direct_from']])[['symm']]
# sum(sapply(redesign_A75[['direct_from']], length))
# save(redesign_A75, file = 'data/redesign_A75.RData')

# redesign_A100 = redesign_A(points_within_200, airports_within_100,
#                                points_within_airport_100,
#                                direct_from, 32, 100)
# check_symmetric(redesign_A100[['direct_from']])[['symm']]
# sum(sapply(redesign_A100[['direct_from']], length))
# # sum(sapply((sapply(redesign_A100[['direct_from']], unique)), length))
# save(redesign_A100, file = 'data/redesign_A100.RData')

# 4(b)
# ----------

redesign_B = function(points_within_2x, airports_within_x, points_within_airport_x,
                      direct_from, swaps, n_iters=100){
  # Define pool of airports to sample from and to modify
  airports = names(direct_from)
  # Compute benchmark
  benchmark_proportion = mean(compute_proportion(points_within_2x, airports_within_x,
                                            points_within_airport_x, direct_from))
  print(benchmark_proportion)
  # Initialize candidate direct_from initial direct_from
  candidate_direct_from = direct_from
  # Initialize redesigned direct_from from initial direct_from
  redesigned_direct_from = direct_from
  for(i in 1:n_iters){
    for(j in 1:swaps){
      airports_to_alter = sample(airports, 2)
      airport_1 = airports_to_alter[1]
      airport_2 = airports_to_alter[2]
      dests_1 = candidate_direct_from[[airport_1]]
      dests_2 = candidate_direct_from[[airport_2]]
      # Connections reachable from airport 1 and NOT reachable from airport 2
      #   -> Handles edge case of swapping connections that are already reachable
      set_1 = setdiff(dests_1, dests_2)
      # Connections reachable from airport 2 and NOT reachable from airport 1
      set_2 = setdiff(dests_2, dests_1)
      # Check that airport 2 is NOT in set of airports reachable by airport 1;
      #   -> If it is; remove it
      if(airport_2 %in% set_1){
        set_1 = set_1[-which(set_1 == airport_2)]
      }
      # Check that airport 1 is NOT in set of airports reachable by airport 2;
      #   -> If it is; remove it
      if(airport_1 %in% set_2){
        set_2 = set_2[-which(set_2 == airport_1)]
      }
      # Check that there exists UNIQUE and NEW connections to swap airports
      #   -> If not; repeat process again
      new = (length(set_1) == 0) || (length(set_2) == 0)
      while(new){
        airports_to_alter = sample(airports, 2)
        airport_1 = airports_to_alter[1]
        airport_2 = airports_to_alter[2]
        dests_1 = candidate_direct_from[[airport_1]]
        dests_2 = candidate_direct_from[[airport_2]]
        # Connections reachable from airport 1 and NOT reachable from airport 2
        #   -> Handles edge case of swapping connections that are already reachable
        set_1 = setdiff(dests_1, dests_2)
        # Connections reachable from airport 2 and NOT reachable from airport 1
        set_2 = setdiff(dests_2, dests_1)
        # Check that airport 2 is NOT in set of airports reachable by airport 1;
        #   -> If it is; remove it
        if(airport_2 %in% set_1){
          set_1 = set_1[-which(set_1 == airport_2)]
        }
        # Check that airport 1 is NOT in set of airports reachable by airport 2;
        #   -> If it is; remove it
        if(airport_1 %in% set_2){
          set_2 = set_2[-which(set_2 == airport_1)]
        }
        # Check that there exists UNIQUE and NEW connections to swap airports
        #   -> If not; repeat process again
        new = (length(set_1) == 0) || (length(set_2) == 0)
      }
      dest_1 = sample(set_1, 1)
      dest_2 = sample(set_2, 1)
      # Swap the two destinations
      dests_1[which(dests_1 == dest_1)] = dest_2
      dests_2[which(dests_2 == dest_2)] = dest_1
      # Update value in candidate object
      candidate_direct_from[[airport_1]] = dests_1
      candidate_direct_from[[airport_2]] = dests_2
      # Add symmetry to swapped destinations; their original
      # outgoing destinations must be swapped too
      dests_1_inverse = candidate_direct_from[[dest_1]]
      dests_2_inverse = candidate_direct_from[[dest_2]]
      # Swap
      dests_1_inverse[which(dests_1_inverse == airport_1)] = airport_2
      dests_2_inverse[which(dests_2_inverse == airport_2)] = airport_1
      # Update value in candidate object
      candidate_direct_from[[dest_1]] = dests_1_inverse
      candidate_direct_from[[dest_2]] = dests_2_inverse
    }
    # Compute proportion for new `direct_from` object
    #   -> If it is better than benchmark proportion; replace
    candidate_proportion = mean(compute_proportion(points_within_2x, airports_within_x,
                                              points_within_airport_x,
                                              candidate_direct_from))
    if(candidate_proportion > benchmark_proportion){
      print(candidate_proportion)
      redesigned_direct_from = candidate_direct_from
      benchmark_proportion = candidate_proportion
    }
    # Revert candidate_direct_from to most recent improved object
    candidate_direct_from = redesigned_direct_from
  }
  proportions = compute_proportion(points_within_2x, airports_within_x,
                                   points_within_airport_x,
                                   redesigned_direct_from)
  return(list('direct_from' = redesigned_direct_from, 'proportions' = proportions))
}

# redesign_B75 = redesign_B(points_within_150, airports_within_75,
#                               points_within_airport_75,
#                               direct_from, 32, 100)
# check_symmetric(redesign_B75[['direct_from']])[['symm']]
# sum(sapply(redesign_B75[['direct_from']], length))
# save(redesign_B75, file = 'data/redesign_B75.RData')
# load('data/redesign_B75.RData')


# redesign_B100 = redesign_B(points_within_200, airports_within_100,
#                            points_within_airport_100,
#                            direct_from, 32, 100)
# check_symmetric(redesign_B100[['direct_from']])[['symm']]
# sum(sapply(redesign_B100[['direct_from']], length))
# save(redesign_B100, file = 'data/redesign_B100.RData')
# load('data/redesign_B100.RData')


# 4(c)
# ----------

redesign_C = function(points_within_2x, airports_within_x, points_within_airport_x,
                      direct_from, batch_size, n_iters=100){
  # Compute benchmark
  benchmark_proportion = mean(compute_proportion(points_within_2x, airports_within_x,
                                                 points_within_airport_x, direct_from))
  print(benchmark_proportion)
  # Initialize candidate direct_from initial direct_from
  candidate_direct_from = direct_from
  # Initialize redesigned direct_from from initial direct_from
  redesigned_direct_from = direct_from
  # Initialize vectors to track changes done to each airport
  #   -> At most; two connections may be subtracted or added
  added = numeric(length(direct_from))
  removed = numeric(length(direct_from))
  names(added) = names(direct_from)
  names(removed) = names(direct_from)
  for(i in 1:n_iters){
    # Intialized iter added/removed to most saved
    added_t = added
    removed_t = removed
    for(j in 1:batch_size){
      # Define pool of airports to sample from and to modify
      # Conditions:
      #   (1) Airport must have at least 4 connections
      #   (2) Airport must NOT have had more than 2 connections already added to it
      #         -> Can not add any more connections to it;
      airports = names(candidate_direct_from[(sapply(candidate_direct_from, length) > 9) & (added_t < 2)])
      airport_1 = sample(airports, 1)
      # Outgoing connections from 'airport_1'
      airport_1_dests = candidate_direct_from[[airport_1]]
      # Define pool of airports to sample from to add to 'airport_1'
      # Conditions:
      #   (1) Added connection must not already reachable by 'airport_1'
      #   (2) Added connection must be able to add a connection itself
      #   (3) Added connection must have a connection able to remove
      #   (4) Added connection must not be itself
      unreachable_dests = airports[(!airports %in% airport_1_dests) & (airports != airport_1)]
      # Sample a new connection to add to 'airport_1' connections
      airport_2 = sample(unreachable_dests, 1)
      # Add new connections to 'airport_2' connections
      airport_1_dests = append(airport_1_dests, airport_2)
      # Update in 'candidate_direct_from' object
      candidate_direct_from[[airport_1]] = airport_1_dests
      # Increment/decerement counters objects
      added_t[[airport_1]] = added_t[[airport_1]] + 1
      removed_t[[airport_1]] = removed_t[[airport_1]] - 1
      # For added new connection 'airport_2';
      #   (1) Add 'airport_1' to 'airport_2' connections by replacing it with one of
      #       its connections; 'airport_to_remove'
      #   (3) Remove from 'airport_to_remove' connections airport_2'
      # Outgoing connections from 'airport_2'
      airport_2_dests = candidate_direct_from[[airport_2]]
      # Define pool of airports from 'airport_2' to sample from and remove
      # Condition:
      #   (1) Removed connection must be able to remove connection
      airport_2_removeable_dests = airport_2_dests[(removed[airport_2_dests] < 2)]
      # Sample random airport to remove from 'airport_2' connections
      airport_to_remove = sample(airport_2_removeable_dests, 1)
      airport_to_remove_idx = which(airport_2_dests == airport_to_remove)
      # Remove from `airport_to_remove` connections `airport_2`
      airport_to_remove_dests = candidate_direct_from[[airport_to_remove]]
      airport_to_remove_dests_new = airport_to_remove_dests[-which(airport_to_remove_dests == airport_2)]
      # Update in 'candidate_direct_from' object
      candidate_direct_from[[airport_to_remove]] = airport_to_remove_dests_new
      # Increment modified counter
      added_t[[airport_to_remove]] = added_t[[airport_to_remove]] - 1
      removed_t[[airport_to_remove]] = removed_t[[airport_to_remove]] + 1
      # Replace 'airport_to_remove' from 'airport_2's connections with 'airport_1'
      airport_2_dests[airport_to_remove_idx] = airport_1
      # Update in 'candidate_direct_from' object
      candidate_direct_from[[airport_2]] = airport_2_dests
    }
    # Compute proportion for new `direct_from` object
    #   -> If it is better than benchmark proportion; replace
    candidate_proportion = mean(compute_proportion(points_within_2x, airports_within_x,
                                                   points_within_airport_x,
                                                   candidate_direct_from))
    if(candidate_proportion > benchmark_proportion){
      # Print new score
      print(candidate_proportion)
      # Save new object
      redesigned_direct_from = candidate_direct_from
      # Update benchmark to beat
      benchmark_proportion = candidate_proportion
      # Update increment counters
      #   -> We don't save changes to counters unless new object is also saved
      added = added_t
      removed = removed_t
    }
    # Revert candidate_direct_from to most recent improved object
    candidate_direct_from = redesigned_direct_from
  }
  proportions = compute_proportion(points_within_2x, airports_within_x,
                                   points_within_airport_x,
                                   redesigned_direct_from)
  return(list('direct_from' = redesigned_direct_from, 'proportions' = proportions))
}

# redesign_C75 = redesign_C(points_within_150, airports_within_75,
#                           points_within_airport_75,direct_from, 16, 100)
# # Check condition C
# check_C(redesign_C75[['direct_from']], direct_from)
# # Check object is symmetric
# check_symmetric(redesign_C75[['direct_from']])[['symm']]
# # Check number connections constant
# sum(sapply(redesign_C75[['direct_from']], length)) == 5108
# save(redesign_C75, file = 'data/redesign_C75.RData')

# redesign_C100 = redesign_C(points_within_200, airports_within_100,
#                            points_within_airport_100, direct_from, 16, 100)
# # Check condition C
# check_C(redesign_C100[['direct_from']], direct_from)
# # Check object is symmetric
# check_symmetric(redesign_C100[['direct_from']])[['symm']]
# # Check number connections constant
# sum(sapply(redesign_C100[['direct_from']], length)) == 5108
# save(redesign_C100, file = 'data/redesign_C100.RData')

