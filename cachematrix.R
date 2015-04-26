## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix - cache functionality surrounding a matrix object, 
# allowing its computated inverse value to be stored in order to save
# unnecessary recalculation
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get  <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve - takes a matrix x, determines if its inverse i has been calculated
# yet ... either returns the cached calculation, or runs an inverse calculation on said matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("using cached inverse")
    return(i)    
  }
  matrix <- x$get()
  i <- solve(x, ...)
  x$setInverse(i)
  i  
}
