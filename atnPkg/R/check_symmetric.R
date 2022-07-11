#' Function that returns whether direct_from object is symmetric
#'
#' Symmetry in direct_from indicates that there for any airports A and B, A has a direct flight to B and B has a direct flight to A
#'
#' @param direct_from A list of character vectors, indexable via airport code
#'  \itemize{
#'   \item \code{symm} Boolean that indicates whether direct_from is symmetric
#'   \item \code{conn} n x 2 character matrix that provides the 'n' asymmetric connections
#' }
#' @examples
#' # check whether default direct_from object is symmetric
#' # there are no asymmetric connections
#' check_symmetric(direct_from)
#'
#' @export

check_symmetric <- function(direct_from) {
  n = length(direct_from)
  # Initialize `symm``
  symm = TRUE
  # Initialize `conn`
  conn = matrix(nrow = n, ncol = 2)
  # Airport keys
  origins = names(direct_from)
  # Loop origin, dests pairs
  for(i in 1:n){
    origin = origins[i]
    dests = direct_from[[origin]]
    for(dest in dests){
      dest_inverse = direct_from[[dest]]
      if(!origin %in% dest_inverse){
        symm = FALSE
        # Origin is not in Dest outgoings
        conn[i,] = c(dest, origin)
      }
    }
  }
  # Drop NA from `conn`
  conn = conn[complete.cases(conn), ]
  return( list('symm' = symm, 'conn' = conn))
}
