## Two following functions cache the invers of a matrix.

## The following function creates a list of functions which are necessary
## for caching the given matrix and the inverse of the given matrix.

makeCacheMatrix <- function(x = matrix()) {
  invert_matrix <- NULL
  set <- function(y){       #
    x <<- y                 # This part is not necessary.  
    invert_matrix <<- NULL  # It is not used in cacheSolve()
  }                         #  
  get <- function()x
  set_invert_matrix <- function(invert) invert_matrix <<- invert
  get_invert_matrix <- function() invert_matrix
  list(set = set, get = get, 
       set_invert_matrix = set_invert_matrix, 
       get_invert_matrix = get_invert_matrix)
}


## The following function checks if there is chached inverse of given matrix.
## If it is, function return the cached inverse of given matrix. 
## If not, function compute the inverse of given matrix, caches the invers and return it.. 

cacheSolve <- function(x, ...) {
  invert_matrix <- x$get_invert_matrix()
  if(!is.null(invert_matrix)){
    message ('getting cached data')
    return (invert_matrix)
  }
  matrix <- x$get()
  invert_matrix <- solve(matrix, ...)
  x$set_invert_matrix(invert_matrix)
  invert_matrix
        ## Return a matrix that is the inverse of 'x'
}
