## Caching the inverse of a matrix

## Function makeCacheMatrix creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(x_val) {
    x <<- x_val
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse_m) inverse <<- inverse_m
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Function cacheSolve checks if the inverse has already been calculated for 
## passed matrix X and retrieve the inverse from the cache or computes the 
## inverse in case it has not been done yet. Returns a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("Getting cached data..")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  inverse        
}
