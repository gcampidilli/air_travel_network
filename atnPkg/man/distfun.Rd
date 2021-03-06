% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distfun.R
\name{distfun}
\alias{distfun}
\title{Calculates distance matrix for 2 lonlat matrices}
\usage{
distfun(lonlat1, lonlat2)
}
\arguments{
\item{lonlat1}{n1 x 2 matrix of longitude and latitude pairs in decimal degrees
(positive for North and East, negative for South and West)}

\item{lonlat2}{n2 x 2 matrix of longitude and latitude pairs in decimal degrees
(positive for North and East, negative for South and West)}
}
\value{
a  n1 x n2 distance matrix
}
\description{
Given 2 matrices with lonlat pairs and vectors containing their respective airport codes,
'distfun()' returns a distance matrix with the great circle distance calculated for each lonlat pair combination
}
\examples{
ll1 = data.frame(lon = airport_data[c(1:5),2], lat = airport_data[c(1:5),3])
ll2 = data.frame(lon = airport_data[c(6:8),2], lat = airport_data[c(6:8),3])
output = distfun(ll1,ll2)
}
