# A Decentralized Air Travel Network
STSCI 4520 Statistical Computing Capstone Project

"Your final project will analyze the existing air travel network using real flight data, and attempt to decentralize the network in order to provide more non-stop travel options to people who live far from hub airports.

The R package contains the following components:
1. A data frame called airport_data with at least three columns:
• all of the unique airport codes in datasets/airline_2019-07-01.csv, sorted alphabetically • the longitude coordinates of each airport
• the latitude coordinates of each airport

2. A function to compute the great circle distance between pairs of longitude/latitude coordinates.

3. A function to find all airports in airport_data within x miles of a longitude latitude point

4. Using the data in datasets/airline_2019-07-01.csv, create a list containing information about direct connections from each airport. 

5. A function for testing whether a direct_from object is symmetric. It should return a list whose first element is TRUE or FALSE (whether or not it is symmetric), and whose second element is an n x 2 character matrix giving the n asymmetric connections.

The analysis of this package will be the Rmarkdown vignette. This contains:
1. A map of the airport data that include information about the airport codes and the number of non-stop destinations from each airport in your map. 

2. Below is code that defines a grid of points covering the continental USA and figures out which points are inside the continental USA.
Given a point on the grid, figure out which other points are reachable by
a. Traveling a distance of 2x (as the crow flies), OR
b. Traveling less than x miles to an airport, taking one flight, and traveling less than x miles to another point.
Plot the points reachable from the gridpoint closest to Ithaca. Use x = 75 miles and x = 100 miles.

3. Repeat the previous exercise for every point in lonlat_usa. Make plots of the proportion of reachable grid points from every point in lonlat_usa for x = 75 miles and x = 100 miles. What is the average proportion of reachable grid points for x = 75 and x = 100?

4. Now your task is to redesign the airline network to improve the average proportion of reachable grid points. This means creating new direct_from objects. Your new direct_from objects should remain symmetric. This part will definitely take longer to run, so put it in one or more separate scripts.
Redesign networks separately under the following 3 scenarios:
a. The total number of direct connections must remain constant. In other words, if you add a connection, you must remove a different connection.
b. The total number of connections to and from each airport must remain constant.
c. You can add or subtract up to two connections from each airport, leaving the other connections unchanged, and keeping the total number of connections constant.
Your redesigned networks should be saved in your R package as data objects with the names direct_from_a, direct_from_b, and direct_from_c, corresponding to the scenarios above.
For each redesigned network, make a map of the reachable grid points from Ithaca, make a map of the proportion of reachable grid points from every grid point, and report the average number of reachable grid points.

#### Authors: Grace Campidilli (gec83@cornell.edu), Benjamin Yeh (by253@cornell.edu)
