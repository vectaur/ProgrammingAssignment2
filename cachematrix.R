## Two functions below to demonstrate R's ability to scope
## in different environments, to allow caching
## Function makeCacheMatrix allows the creation of a cached matrix
## Function cacheSolve returns the inverse of a matrix, using the 
## cached matrix if possible


## create a special matrix that allows caching using the <<- operator
## this is actually a list with values set, get, setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## return a matrix that is the inverse of the argument
## if the cached inverse matrix already exists, return that
## matrix without recalculating

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setInverse(inv)
  inv
}
