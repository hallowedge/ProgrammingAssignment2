## makeCacheMatrix and cacheSolve can be used in conjunction to 
## cache the results of a Solve() over a given matrix, so subsequent
## operations over the same matrix can be retrieved from cache.

## makeCacheMatrix creates a list for a given matrix that includes the functions
## to cache and retrieve the results.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve uses the functions attached to the matrix using makeCacheMatrix
## to either retrieve the cached result, or Solve the matrix and save the result
## to cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
